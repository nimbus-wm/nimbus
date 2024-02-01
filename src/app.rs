mod observer;

use std::{
    cell::RefCell,
    fmt::Debug,
    rc::{Rc, Weak},
    sync::{
        atomic::{AtomicI32, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread,
    time::Instant,
};

use accessibility::{AXUIElement, AXUIElementActions, AXUIElementAttributes};
use accessibility_sys::{
    kAXApplicationActivatedNotification, kAXApplicationDeactivatedNotification,
    kAXMainWindowChangedNotification, kAXStandardWindowSubrole, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
};
use core_foundation::runloop::CFRunLoop;
use icrate::{
    AppKit::{NSRunningApplication, NSWorkspace},
    Foundation::{CGPoint, CGRect},
};
use tracing::{debug, error, instrument, trace, Span};

use crate::{
    app::observer::Observer,
    reactor::{self, AppInfo, AppState, Event, WindowInfo},
    run_loop::WakeupHandle,
    util::{NSRunningApplicationExt, ToCGType, ToICrate},
};

pub use accessibility_sys::pid_t;

/// An identifier representing a window.
///
/// This identifier is only valid for the lifetime of the process that owns it.
/// It is not stable across restarts of the window manager.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct WindowId {
    pub pid: pid_t,
    idx: i32,
}

impl WindowId {
    #[cfg(test)]
    pub(crate) fn new(pid: pid_t, idx: i32) -> WindowId {
        WindowId { pid, idx }
    }
}

pub fn running_apps(bundle: Option<String>) -> impl Iterator<Item = (pid_t, AppInfo)> {
    unsafe { NSWorkspace::sharedWorkspace().runningApplications() }
        .into_iter()
        .flat_map(move |app| {
            let bundle_id = app.bundle_id()?.to_string();
            if let Some(filter) = &bundle {
                if !bundle_id.contains(filter) {
                    return None;
                }
            }
            Some((app.pid(), AppInfo::from(&*app)))
        })
}

pub struct AppThreadHandle {
    requests_tx: Sender<(Span, Request)>,
    wakeup: WakeupHandle,
}

impl AppThreadHandle {
    #[cfg(test)]
    pub(crate) fn new_for_test() -> (Self, Receiver<(Span, Request)>) {
        let (requests_tx, requests_rx) = channel();
        let this = AppThreadHandle {
            requests_tx,
            wakeup: WakeupHandle::for_current_thread(0, || {}),
        };
        (this, requests_rx)
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

#[derive(Debug, Clone)]
pub enum Request {
    SetWindowFrame(WindowId, CGRect),
    SetWindowPos(WindowId, CGPoint),

    /// Temporarily suspends position and size update events for this window.
    BeginWindowAnimation(WindowId),
    /// Resumes position and size events for the window. One position and size
    /// event are sent immediately upon receiving the request.
    EndWindowAnimation(WindowId),

    Raise(WindowId, RaiseToken),
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
    app: AXUIElement,
    windows: Vec<WindowState>,
    events_tx: Sender<(Span, Event)>,
    requests_rx: Receiver<(Span, Request)>,
    pid: pid_t,
    bundle_id: Option<String>,
    last_window_idx: i32,
    observer: Observer,
}

struct WindowState {
    wid: WindowId,
    elem: AXUIElement,
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
                debug!("Watching app {} failed with {err:?}", self.pid);
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

        // Send the ApplicationLaunched event.
        let app_state = AppState {
            handle,
            info,
            main_window: self.app.main_window().ok().and_then(|w| self.id(w).ok()),
            is_frontmost: self.app.frontmost().map(|b| b.into()).unwrap_or(false),
        };
        let Ok(()) = self.events_tx.send((
            Span::current(),
            Event::ApplicationLaunched(self.pid, app_state, windows),
        )) else {
            debug!(
                "Failed to send ApplicationLaunched event for {pid}, exiting thread",
                pid = self.pid,
            );
            return false;
        };

        true
    }

    #[instrument(skip_all, fields(app = ?self.app, ?request))]
    fn handle_request(&self, request: Request) -> Result<(), accessibility::Error> {
        match request {
            Request::SetWindowPos(wid, pos) => {
                let window = self.window_element(wid)?;
                trace("set_position", window, || {
                    window.set_position(pos.to_cgtype())
                })?;
            }
            Request::SetWindowFrame(wid, frame) => {
                let window = self.window_element(wid)?;
                trace("set_position", window, || {
                    window.set_position(frame.origin.to_cgtype())
                })?;
                trace("set_size", window, || {
                    window.set_size(frame.size.to_cgtype())
                })?;
            }
            Request::BeginWindowAnimation(wid) => {
                let window = self.window_element(wid)?;
                self.stop_notifications_for_animation(window);
            }
            Request::EndWindowAnimation(wid) => {
                let window = self.window_element(wid)?;
                self.restart_notifications_after_animation(window);
                let pos = trace("position", window, || window.position())?;
                let size = trace("size", window, || window.size())?;
                self.send_event(Event::WindowMoved(wid, pos.to_icrate()));
                self.send_event(Event::WindowResized(wid, size.to_icrate()));
            }
            Request::Raise(wid, token) => {
                let window = self.window_element(wid)?;
                trace("raise", window, || window.raise())?;
                // This request could be handled out of order with respect to
                // later requests sent to other apps by the reactor. To avoid
                // raising ourselves after a later request was processed to
                // raise a different app, we check the last-raised pid while
                // holding a lock that ensures no other apps are executing a
                // raise request at the same time.
                //
                // The only way this can fail to provide eventual consistency is
                // if we time out on the set_frontmost request but the app
                // processes it later. For now we set a fairly long timeout to
                // mitigate this (but not too long, to avoid blocking all raise
                // requests on an unresponsive app). It's unlikely that an app
                // will be unresponsive for so long after responding to the
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
                        trace("set_timeout", &self.app, || {
                            self.app.set_messaging_timeout(0.5)
                        })?;
                        trace("set_frontmost", &self.app, || self.app.set_frontmost(true))?;
                        trace("set_timeout", &self.app, || {
                            self.app.set_messaging_timeout(0.0)
                        })?;
                        Ok(())
                    })
                    .unwrap_or(Ok(()))?;
            }
        }
        Ok(())
    }

    #[instrument(skip_all, fields(app = ?self.app, ?notif))]
    fn handle_notification(&mut self, elem: AXUIElement, notif: &str) {
        trace!("Got {notif:?} on {elem:?}");
        #[allow(non_upper_case_globals)]
        #[forbid(non_snake_case)]
        // TODO: Handle all of these.
        match notif {
            kAXApplicationActivatedNotification => {
                // Unfortunately, if the user clicks on a new main window to
                // activate this app, we get this notification before getting
                // the main window changed notification. To distinguish from the
                // case where the app was activated and the main window has
                // *not* changed, we read the main window and send it along with
                // the notification.
                let main = elem.main_window().ok().and_then(|w| self.id(w).ok());
                self.send_event(Event::ApplicationActivated(self.pid, main));
            }
            kAXApplicationDeactivatedNotification => {
                self.send_event(Event::ApplicationDeactivated(self.pid));
            }
            kAXMainWindowChangedNotification => {
                let main = self.id(elem).ok();
                self.send_event(Event::ApplicationMainWindowChanged(self.pid, main));
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
                let Some(idx) = self.windows.iter().position(|w| w.elem == elem) else {
                    return;
                };
                let wid = self.windows.remove(idx).wid;
                self.send_event(Event::WindowDestroyed(wid));
            }
            kAXWindowMovedNotification => {
                let Some(window) = self.windows.iter().find(|w| w.elem == elem) else {
                    return;
                };
                let Ok(pos) = window.elem.position() else {
                    return;
                };
                self.send_event(Event::WindowMoved(window.wid, pos.to_icrate()));
            }
            kAXWindowResizedNotification => {
                let Some(window) = self.windows.iter().find(|w| w.elem == elem) else {
                    return;
                };
                let Ok(size) = window.elem.size() else {
                    return;
                };
                self.send_event(Event::WindowResized(window.wid, size.to_icrate()));
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
        self.last_window_idx += 1;
        let wid = WindowId {
            pid: self.pid,
            idx: self.last_window_idx,
        };
        self.windows.push(WindowState { elem, wid });
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

    fn window_element(&self, wid: WindowId) -> Result<&AXUIElement, accessibility::Error> {
        assert_eq!(wid.pid, self.pid);
        let Some(window) = self.windows.iter().find(|w| w.wid == wid) else {
            return Err(accessibility::Error::NotFound);
        };
        Ok(&window.elem)
    }

    fn id(&self, elem: AXUIElement) -> Result<WindowId, accessibility::Error> {
        let Some(window) = self.windows.iter().find(|w| w.elem == elem) else {
            return Err(accessibility::Error::NotFound);
        };
        Ok(window.wid)
    }

    fn stop_notifications_for_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let res = self.observer.remove_notification(elem, notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!("Removing notification {notif:?} on {elem:?} failed with error {err}");
            }
        }
    }

    fn restart_notifications_after_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let res = self.observer.add_notification(elem, notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!("Adding notification {notif:?} on {elem:?} failed with error {err}");
            }
        }
    }
}

fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: Sender<(Span, Event)>) {
    let app = AXUIElement::application(pid);
    let (requests_tx, requests_rx) = channel();
    let Ok(observer) = Observer::new(pid) else {
        debug!("Making observer for pid {pid} failed; exiting app thread");
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
            app: app.clone(),
            windows: vec![],
            events_tx,
            requests_rx,
            pid,
            bundle_id: info.bundle_id.clone(),
            last_window_idx: 0,
            observer,
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
        let state = state.borrow();
        let State { bundle_id, pid, .. } = &*state;
        let bundle_id = bundle_id.as_deref().unwrap_or("None");
        while let Ok((span, request)) = state.requests_rx.try_recv() {
            let _guard = span.enter();
            debug!("Got request for {bundle_id}({pid}): {request:?}");
            match state.handle_request(request.clone()) {
                Ok(()) => (),
                Err(err) => {
                    error!("Error handling request for {bundle_id}({pid}): {request:?}: {err}");
                }
            }
        }
    }
}

impl TryFrom<&AXUIElement> for WindowInfo {
    type Error = accessibility::Error;
    fn try_from(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        Ok(reactor::WindowInfo {
            is_standard: element.role()? == kAXWindowRole
                && element.subrole()? == kAXStandardWindowSubrole,
            title: element.title()?.to_string(),
            frame: element.frame()?.to_icrate(),
        })
    }
}

impl From<&NSRunningApplication> for AppInfo {
    fn from(app: &NSRunningApplication) -> Self {
        AppInfo {
            bundle_id: app.bundle_id().as_deref().map(ToString::to_string),
            localized_name: app.localized_name().as_deref().map(ToString::to_string),
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
    trace!("{desc:12} took {:10?} on {elem:?}", end - start);
    if let Err(err) = &out {
        let app = elem.parent();
        debug!("{desc} failed with {err} for element {elem:#?} with parent {app:#?}");
    }
    out
}
