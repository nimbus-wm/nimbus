//! The app actor manages messaging to an application using the system
//! accessibility APIs.
//!
//! These APIs support reading and writing window states like position and size.

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    num::NonZeroU32,
    rc::{Rc, Weak},
    sync::{
        atomic::{AtomicI32, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use accessibility::{AXUIElement, AXUIElementActions, AXUIElementAttributes};
use accessibility_sys::{
    kAXApplicationActivatedNotification, kAXApplicationDeactivatedNotification,
    kAXMainWindowChangedNotification, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
};
use core_foundation::runloop::CFRunLoop;
use icrate::{
    objc2::{class, msg_send_id, rc::Id},
    AppKit::{NSApplicationActivationOptions, NSRunningApplication},
    Foundation::{CGPoint, CGRect},
};
use serde::{Deserialize, Serialize};
use tokio::sync::oneshot;
use tracing::{debug, error, info, instrument, trace, warn, Span};

use crate::{
    actor::reactor::{AppState, Event, Requested, TransactionId},
    sys::{
        app::running_apps,
        geometry::{ToCGType, ToICrate},
        observer::Observer,
        run_loop::WakeupHandle,
        window_server::WindowServerId,
    },
};

pub use crate::sys::app::{pid_t, AppInfo, WindowInfo};

/// An identifier representing a window.
///
/// This identifier is only valid for the lifetime of the process that owns it.
/// It is not stable across restarts of the window manager.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct WindowId {
    pub pid: pid_t,
    idx: NonZeroU32,
}

impl WindowId {
    #[cfg(test)]
    pub(crate) fn new(pid: pid_t, idx: u32) -> WindowId {
        WindowId {
            pid,
            idx: NonZeroU32::new(idx).unwrap(),
        }
    }
}

pub struct AppThreadHandle {
    requests_tx: Sender<(Span, Request)>,
    wakeup: WakeupHandle,
}

impl AppThreadHandle {
    #[cfg(test)]
    pub(crate) fn new_for_test(requests_tx: Sender<(Span, Request)>) -> Self {
        let this = AppThreadHandle {
            requests_tx,
            wakeup: WakeupHandle::for_current_thread(0, || {}),
        };
        this
    }

    pub fn send(&self, req: Request) -> Result<(), std::sync::mpsc::SendError<(Span, Request)>> {
        self.requests_tx.send((Span::current(), req))?;
        self.wakeup.wake();
        Ok(())
    }
}

impl Debug for AppThreadHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ThreadHandle").finish()
    }
}

#[derive(Debug)]
pub enum Request {
    Terminate,
    GetVisibleWindows,

    SetWindowFrame(WindowId, CGRect, TransactionId),
    SetWindowPos(WindowId, CGPoint, TransactionId),

    /// Temporarily suspend position and size update events for this window.
    BeginWindowAnimation(WindowId),
    /// Resume position and size events for the window. One position and size
    /// event are sent immediately upon receiving the request.
    EndWindowAnimation(WindowId),

    /// Raise the window by making it the main window of this app and then
    /// making this app frontmost.
    ///
    /// Events attributed to this request will have the [`Quiet`] parameter
    /// attached to them.
    Raise(WindowId, RaiseToken, Option<oneshot::Sender<()>>, Quiet),
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub enum Quiet {
    Yes,
    #[default]
    No,
}

/// Prevents stale activation requests from happening after more recent ones.
///
/// This token holds the pid of the latest activation request from the reactor,
/// and provides synchronization between the app threads to ensure that multiple
/// requests aren't handled simultaneously.
///
/// It is also designed not to block the main reactor thread.
#[derive(Clone, Debug, Default)]
pub struct RaiseToken(Arc<(Mutex<()>, AtomicI32)>);

impl RaiseToken {
    /// Checks if the most recent activation request was for `pid`. Calls the
    /// supplied closure if it was.
    pub fn with<R>(&self, pid: pid_t, f: impl FnOnce() -> R) -> Option<R> {
        let _lock = self.0 .0.lock().unwrap();
        if pid == self.0 .1.load(Ordering::SeqCst) {
            Some(f())
        } else {
            None
        }
    }

    pub fn set_pid(&self, pid: pid_t) {
        // Even though we don't hold the lock, we know that the app servicing
        // the Raise request will have to hold it while it activates itself.
        // This means any apps that are first in the queue have either completed
        // their activation request or timed out.
        self.0 .1.store(pid, Ordering::SeqCst)
    }
}

pub fn spawn_initial_app_threads(events_tx: Sender<(Span, Event)>) {
    for (pid, info) in running_apps(None) {
        spawn_app_thread(pid, info, events_tx.clone());
    }
}

pub fn spawn_app_thread(pid: pid_t, info: AppInfo, events_tx: Sender<(Span, Event)>) {
    thread::spawn(move || app_thread_main(pid, info, events_tx));
}

struct State {
    pid: pid_t,
    bundle_id: Option<String>,
    running_app: Id<NSRunningApplication>,
    app: AXUIElement,
    observer: Observer,
    events_tx: Sender<(Span, Event)>,
    requests_rx: Receiver<(Span, Request)>,
    windows: HashMap<WindowId, WindowState>,
    last_window_idx: u32,
    main_window: Option<WindowId>,
    last_activated: Option<Instant>,
}

struct WindowState {
    elem: AXUIElement,
    last_seen_txid: TransactionId,
}

const APP_NOTIFICATIONS: &[&str] = &[
    kAXApplicationActivatedNotification,
    kAXApplicationDeactivatedNotification,
    kAXMainWindowChangedNotification,
    kAXWindowCreatedNotification,
];

const WINDOW_NOTIFICATIONS: &[&str] = &[
    kAXUIElementDestroyedNotification,
    kAXWindowMovedNotification,
    kAXWindowResizedNotification,
    kAXWindowMiniaturizedNotification,
    kAXWindowDeminiaturizedNotification,
    kAXTitleChangedNotification,
];

const WINDOW_ANIMATION_NOTIFICATIONS: &[&str] =
    &[kAXWindowMovedNotification, kAXWindowResizedNotification];

impl State {
    #[instrument(skip_all, fields(?info))]
    #[must_use]
    fn init(&mut self, handle: AppThreadHandle, info: AppInfo) -> bool {
        // Register for notifications on the application element.
        for notif in APP_NOTIFICATIONS {
            let res = self.observer.add_notification(&self.app, notif);
            if let Err(err) = res {
                debug!(pid = ?self.pid, ?err, "Watching app failed");
                return false;
            }
        }

        // Now that we will observe new window events, read the list of windows.
        let Ok(initial_window_elements) = self.app.windows() else {
            // This is probably not a normal application, or it has exited.
            return false;
        };

        // Process the list and register notifications on all windows.
        self.windows.reserve(initial_window_elements.len() as usize);
        let mut windows = Vec::with_capacity(initial_window_elements.len() as usize);
        for elem in initial_window_elements.iter() {
            let elem = elem.clone();
            let Ok(info) = WindowInfo::try_from(&elem) else {
                continue;
            };
            let Some(wid) = self.register_window(elem) else {
                continue;
            };
            windows.push((wid, info));
        }

        self.main_window = self.app.main_window().ok().and_then(|w| self.id(&w).ok());
        let is_frontmost = self.app.frontmost().map(|b| b.into()).unwrap_or(false);
        if is_frontmost {
            self.last_activated = Some(Instant::now());
        }

        // Send the ApplicationLaunched event.
        let app_state = AppState {
            handle,
            info,
            main_window: self.main_window,
            is_frontmost,
            frontmost_is_quiet: Quiet::No,
        };
        if self
            .events_tx
            .send((
                Span::current(),
                Event::ApplicationLaunched(self.pid, app_state),
            ))
            .is_err()
            || self
                .events_tx
                .send((
                    Span::current(),
                    Event::WindowsDiscovered {
                        pid: self.pid,
                        new: windows,
                        known_visible: vec![],
                    },
                ))
                .is_err()
        {
            debug!(pid = ?self.pid, "Failed to send ApplicationLaunched event, exiting thread");
            return false;
        };

        true
    }

    #[instrument(skip_all, fields(app = ?self.app, ?request))]
    fn handle_request(&mut self, request: &mut Request) -> Result<(), accessibility::Error> {
        match request {
            Request::Terminate => {
                CFRunLoop::get_current().stop();
                self.send_event(Event::ApplicationThreadTerminated(self.pid));
            }
            Request::GetVisibleWindows => {
                let window_elems = match self.app.windows() {
                    Ok(elems) => elems,
                    Err(e) => {
                        // Send an empty event so that any previously known
                        // windows for this app are cleared.
                        self.send_event(Event::WindowsDiscovered {
                            pid: self.pid,
                            new: Default::default(),
                            known_visible: Default::default(),
                        });
                        return Err(e);
                    }
                };
                let mut new = Vec::with_capacity(window_elems.len() as usize);
                let mut known_visible = Vec::with_capacity(window_elems.len() as usize);
                for elem in window_elems.iter() {
                    let elem = elem.clone();
                    // FIXME: This check is quadratic.
                    if let Ok(id) = self.id(&elem) {
                        known_visible.push(id);
                        continue;
                    }
                    let Ok(info) = WindowInfo::try_from(&elem) else {
                        continue;
                    };
                    let Some(wid) = self.register_window(elem) else {
                        continue;
                    };
                    new.push((wid, info));
                }
                self.send_event(Event::WindowsDiscovered {
                    pid: self.pid,
                    new,
                    known_visible,
                });
            }
            &mut Request::SetWindowPos(wid, pos, txid) => {
                let window = self.window_mut(wid)?;
                window.last_seen_txid = txid;
                trace("set_position", &window.elem, || {
                    window.elem.set_position(pos.to_cgtype())
                })?;
                let frame = trace("frame", &window.elem, || window.elem.frame())?;
                self.send_event(Event::WindowFrameChanged(
                    wid,
                    frame.to_icrate(),
                    txid,
                    Requested(true),
                ));
            }
            &mut Request::SetWindowFrame(wid, frame, txid) => {
                let window = self.window_mut(wid)?;
                window.last_seen_txid = txid;
                trace("set_position", &window.elem, || {
                    window.elem.set_position(frame.origin.to_cgtype())
                })?;
                trace("set_size", &window.elem, || {
                    window.elem.set_size(frame.size.to_cgtype())
                })?;
                let frame = trace("frame", &window.elem, || window.elem.frame())?;
                self.send_event(Event::WindowFrameChanged(
                    wid,
                    frame.to_icrate(),
                    txid,
                    Requested(true),
                ));
            }
            &mut Request::BeginWindowAnimation(wid) => {
                let window = self.window(wid)?;
                self.stop_notifications_for_animation(&window.elem);
            }
            &mut Request::EndWindowAnimation(wid) => {
                let &WindowState { ref elem, last_seen_txid } = self.window(wid)?;
                self.restart_notifications_after_animation(elem);
                let frame = trace("frame", elem, || elem.frame())?;
                self.send_event(Event::WindowFrameChanged(
                    wid,
                    frame.to_icrate(),
                    last_seen_txid,
                    Requested(true),
                ));
            }
            &mut Request::Raise(wid, ref token, ref mut done, quiet) => {
                let window = self.window(wid)?;
                trace("raise", &window.elem, || window.elem.raise())?;
                let quiet_if = (quiet == Quiet::Yes).then_some(wid);
                // FIXME: quiet is ignored by reactor;
                // if we're activated quietly and then service a non-quiet raise
                // request, we need to signal the reactor that we are not quietly
                // raised anymore.
                //
                // This should be more correct than trying to do something with
                // `quiet` on main window change events directly.
                //
                // Another framing:
                // There are two "edges" that can transition from one main window
                // state to another. One is an app main window change and the other
                // is a frontmost app change. Either can be labelled with quiet;
                // in the case of the frontmost app, the quiet label from the most
                // recent frontmost update of that app applies (even if the actual
                // event was a global frontmost change). The quiet label of the
                // transitioning edge should be applied to the main window change.
                //
                // This seems easiest to model in a separate component.
                let main_window = self.on_main_window_changed(quiet_if);
                if main_window != Some(wid) {
                    warn!(
                        "Raise request failed to raise {desired:?}; instead got {main_window:?}",
                        desired = self.window(wid)?.elem
                    );
                    return Ok(());
                }
                // This request could be handled out of order with respect to
                // later requests sent to other apps by the reactor. To avoid
                // raising ourselves after a later request was processed to
                // raise a different app, we check the last-raised pid while
                // holding a lock that ensures no other apps are executing a
                // raise request at the same time.
                //
                // FIXME: Unfonrtunately this is still very racy in that we now
                // use the unsynchronized NSRunningApplication API to raise the
                // application, which still relies on the application itself to
                // see and respond to a request, and there is no apparent
                // ordering between this and the accessibility messaging. The
                // only way to know whether a raise request was processed is
                // to wait for an event telling us the app has been activated.
                // This might benefit from using async/await.
                //
                // The below comments are for the old way which used the
                // accessibility API. This solved the ordering problem, but has
                // the unfortunate issue that it raises *all* windows of the
                // application, not just the main window.
                //
                // ---
                //
                // The only way this can fail to provide eventual consistency is
                // if we time out on the set_frontmost request but the app
                // processes it later. For now we set a fairly long timeout to
                // mitigate this (500ms – not too long, to avoid blocking all
                // raise requests on an unresponsive app). It's unlikely that an
                // app will be unresponsive for so long after responding to the
                // raise request.
                //
                // In the future, we could do better by asking the app if it was
                // activated (with an unlimited timeout while not holding the
                // lock). If it was and another app was activated in the
                // meantime, we would "undo" our activation in favor of the app
                // that is supposed to be activated. This requires taking into
                // account user-initiated activations.
                token
                    .with(self.pid, || {
                        // This option is deprecated, but there is no alternative.
                        #[allow(non_upper_case_globals)]
                        const NSApplicationActivateIgnoringOtherApps:
                            NSApplicationActivationOptions = 1 << 1;
                        let success = unsafe {
                            // This should be marked as safe.
                            self.running_app
                                .activateWithOptions(NSApplicationActivateIgnoringOtherApps)
                        };
                        if success {
                            // For now we assume the activation worked if the
                            // call succeeded.
                            // TODO: Don't send the event, just wait for the real one and attach
                            // `quiet` to it (under timeout). Also don't send `done` until then
                            // (or timeout).
                            // Idea: Maybe we can control the timeout in client and use the send
                            // result to decide whether `quiet` applies.
                            self.last_activated = Some(Instant::now());
                            self.send_event(Event::ApplicationActivated(self.pid, quiet));
                        } else {
                            warn!(?self.pid, "Failed to activate app");
                        }
                        Ok(())
                    })
                    .unwrap_or(Ok(()))?;
                if let Some(done) = done.take() {
                    let _ = done.send(());
                }
            }
        }
        Ok(())
    }

    fn on_main_window_changed(&mut self, quiet_if: Option<WindowId>) -> Option<WindowId> {
        // Always read back the main window instead of getting it from an event,
        // in case the event is stale. This is necessary because we sometimes
        // manufacture events and don't want them to be incorrectly interleaved.
        let elem = match trace("main_window", &self.app, || self.app.main_window()) {
            Ok(elem) => elem,
            Err(e) => {
                error!("Failed to read main window: {e:?}");
                return None;
            }
        };
        let Some(wid) = self.id(&elem).ok() else {
            // I suspect we may hit this at some point (before receiving the
            // window created notification); it isn't handled correctly today.
            error!("Got MainWindowChanged on unknown window {elem:?}");
            return None;
        };
        if self.main_window == Some(wid) {
            // Suppress redundant events.
            return Some(wid);
        }
        self.main_window = Some(wid);
        let quiet = match quiet_if {
            Some(id) if id == wid => Quiet::Yes,
            _ => Quiet::No,
        };
        self.send_event(Event::ApplicationMainWindowChanged(
            self.pid,
            Some(wid),
            quiet,
        ));
        Some(wid)
    }

    #[instrument(skip_all, fields(app = ?self.app, ?notif))]
    fn handle_notification(&mut self, elem: AXUIElement, notif: &str) {
        trace!(?notif, ?elem, "Got notification");
        #[allow(non_upper_case_globals)]
        #[forbid(non_snake_case)]
        // TODO: Handle all of these.
        match notif {
            kAXApplicationActivatedNotification => {
                // Suppress events from our own activation.
                // Since the events we manufacture aren't guaranteed to have happened,
                // we are stuck with using a timeout so we don't suppress real events
                // in the future.
                match self.last_activated {
                    Some(ts) if ts.elapsed() < Duration::from_millis(1000) => (),
                    _ => {
                        // Unfortunately, if the user clicks on a new main window to
                        // activate this app, we get this notification before getting
                        // the main window changed notification. First read the main
                        // window and send a notification if it changed.
                        self.on_main_window_changed(None);
                        self.send_event(Event::ApplicationActivated(self.pid, Quiet::default()));
                    }
                }
            }
            kAXApplicationDeactivatedNotification => {
                self.last_activated = None;
                self.send_event(Event::ApplicationDeactivated(self.pid));
            }
            kAXMainWindowChangedNotification => {
                self.on_main_window_changed(None);
            }
            kAXWindowCreatedNotification => {
                let Ok(window) = WindowInfo::try_from(&elem) else {
                    return;
                };
                let Some(wid) = self.register_window(elem) else {
                    return;
                };
                self.send_event(Event::WindowCreated(wid, window));
            }
            kAXUIElementDestroyedNotification => {
                let Some((&wid, _)) = self.windows.iter().find(|(_, w)| w.elem == elem) else {
                    return;
                };
                self.windows.remove(&wid);
                self.send_event(Event::WindowDestroyed(wid));
            }
            kAXWindowMovedNotification | kAXWindowResizedNotification => {
                // The difference between these two events isn't very useful to
                // expose. Anytime there's a resize we'll want to check the
                // position to see which corner the window was resized from. So
                // we always read and send the full frame since it's a single
                // request anyway.
                let Ok(wid) = self.id(&elem) else {
                    return;
                };
                let last_seen = self.window(wid).unwrap().last_seen_txid;
                let Ok(frame) = elem.frame() else {
                    return;
                };
                self.send_event(Event::WindowFrameChanged(
                    wid,
                    frame.to_icrate(),
                    last_seen,
                    Requested(false),
                ));
            }
            kAXWindowMiniaturizedNotification => {}
            kAXWindowDeminiaturizedNotification => {}
            kAXTitleChangedNotification => {}
            _ => {
                error!("Unhandled notification {notif:?} on {elem:#?}");
            }
        }
    }

    #[must_use]
    fn register_window(&mut self, elem: AXUIElement) -> Option<WindowId> {
        if !register_notifs(&elem, self) {
            return None;
        }
        let idx = WindowServerId::try_from(&elem)
            .or_else(|e| {
                info!("Could not get window server id for {elem:?}: {e}");
                Err(e)
            })
            .ok()
            .map(|id| NonZeroU32::new(id.as_u32()).expect("Window server id was 0"))
            .unwrap_or_else(|| {
                self.last_window_idx += 1;
                NonZeroU32::new(self.last_window_idx).unwrap()
            });
        let wid = WindowId { pid: self.pid, idx };
        let old = self.windows.insert(
            wid,
            WindowState {
                elem,
                last_seen_txid: TransactionId::default(),
            },
        );
        assert!(old.is_none(), "Duplicate window id {wid:?}");
        return Some(wid);

        fn register_notifs(win: &AXUIElement, state: &State) -> bool {
            // Filter out elements that aren't regular windows.
            match win.role() {
                Ok(role) if role == kAXWindowRole => (),
                _ => return false,
            }
            for notif in WINDOW_NOTIFICATIONS {
                let res = state.observer.add_notification(win, notif);
                if let Err(err) = res {
                    trace!("Watching failed with error {err:?} on window {win:#?}");
                    return false;
                }
            }
            true
        }
    }

    fn send_event(&self, event: Event) {
        self.events_tx.send((Span::current(), event)).unwrap();
    }

    fn window(&self, wid: WindowId) -> Result<&WindowState, accessibility::Error> {
        assert_eq!(wid.pid, self.pid);
        self.windows.get(&wid).ok_or(accessibility::Error::NotFound)
    }

    fn window_mut(&mut self, wid: WindowId) -> Result<&mut WindowState, accessibility::Error> {
        assert_eq!(wid.pid, self.pid);
        self.windows.get_mut(&wid).ok_or(accessibility::Error::NotFound)
    }

    fn id(&self, elem: &AXUIElement) -> Result<WindowId, accessibility::Error> {
        let Some((&wid, _)) = self.windows.iter().find(|(_, w)| &w.elem == elem) else {
            return Err(accessibility::Error::NotFound);
        };
        Ok(wid)
    }

    fn stop_notifications_for_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let res = self.observer.remove_notification(elem, notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!(
                    ?notif,
                    ?elem,
                    "Removing notification failed with error {err}"
                );
            }
        }
    }

    fn restart_notifications_after_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let res = self.observer.add_notification(elem, notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!(?notif, ?elem, "Adding notification failed with error {err}");
            }
        }
    }
}

fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: Sender<(Span, Event)>) {
    let app = AXUIElement::application(pid);
    let running_app: Id<NSRunningApplication> = unsafe {
        // For some reason this binding isn't generated in icrate.
        msg_send_id![class!(NSRunningApplication), runningApplicationWithProcessIdentifier:pid]
    };
    let (requests_tx, requests_rx) = channel();
    let Ok(observer) = Observer::new(pid) else {
        debug!(?pid, "Making observer failed; exiting app thread");
        return;
    };

    // Create our app state and set up the observer callback.
    let state = Rc::new_cyclic(|weak: &Weak<RefCell<State>>| {
        let weak = weak.clone();
        let observer = observer.install(move |elem, notif| {
            if let Some(state) = weak.upgrade() {
                state.borrow_mut().handle_notification(elem, notif)
            }
        });

        RefCell::new(State {
            pid,
            running_app,
            bundle_id: info.bundle_id.clone(),
            app: app.clone(),
            observer,
            events_tx,
            requests_rx,
            windows: HashMap::new(),
            last_window_idx: 0,
            main_window: None,
            last_activated: None,
        })
    });

    // Set up our request handler.
    let st = state.clone();
    let wakeup = WakeupHandle::for_current_thread(0, move || handle_requests(&st));
    let handle = AppThreadHandle { requests_tx, wakeup };

    // Initialize the app.
    if !state.borrow_mut().init(handle, info) {
        return;
    }

    // Finally, invoke the run loop to handle events.
    CFRunLoop::run_current();

    fn handle_requests(state: &Rc<RefCell<State>>) {
        // Multiple source wakeups can be collapsed into one, so we have to make
        // sure all pending events are handled eventually. For now just handle
        // them all.
        let mut state = state.borrow_mut();
        while let Ok((span, mut request)) = state.requests_rx.try_recv() {
            let _guard = span.enter();
            debug!(?state.bundle_id, ?state.pid, ?request, "Got request");
            match state.handle_request(&mut request) {
                Ok(()) => (),
                Err(err) => {
                    error!(?state.bundle_id, ?state.pid, ?request, "Error handling request: {err}");
                }
            }
        }
    }
}

fn trace<T>(
    desc: &str,
    elem: &AXUIElement,
    f: impl FnOnce() -> Result<T, accessibility::Error>,
) -> Result<T, accessibility::Error> {
    let start = Instant::now();
    let out = f();
    let end = Instant::now();
    trace!(time = ?(end - start), ?elem, "{desc:12}");
    if let Err(err) = &out {
        let app = elem.parent();
        debug!("{desc} failed with {err} for element {elem:#?} with parent {app:#?}");
    }
    out
}
