//! The app actor manages messaging to an application using the system
//! accessibility APIs.
//!
//! These APIs support reading and writing window states like position and size.

#[cfg_attr(test, allow(unused_imports))]
use std::{
    fmt::Debug,
    num::NonZeroU32,
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

use accessibility_sys::{
    kAXApplicationActivatedNotification, kAXApplicationDeactivatedNotification,
    kAXMainWindowChangedNotification, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
};
use core_foundation::{runloop::CFRunLoop, string::CFString};
use icrate::{
    AppKit::NSApplicationActivationOptions,
    Foundation::{CGPoint, CGRect},
};
use serde::{Deserialize, Serialize};
use tokio::sync::{
    mpsc::{
        unbounded_channel as channel, UnboundedReceiver as Receiver, UnboundedSender as Sender,
    },
    oneshot,
};
use tokio_stream::{wrappers::UnboundedReceiverStream, StreamExt};
use tracing::{debug, error, info, instrument, trace, warn, Span};

pub use crate::system::pid_t;
use crate::system::WindowServer;
#[allow(unused_imports)]
use crate::{
    actor::reactor::{self, Event, Requested, TransactionId},
    collections::HashMap,
    sys::{
        app::{running_apps, NSRunningApplicationExt},
        executor::Executor,
        geometry::{ToCGType, ToICrate},
        run_loop::WakeupHandle,
        window_server::WindowServerId,
    },
    system::sync::{
        atomic::{AtomicI32, Ordering},
        Mutex,
    },
    system::{prelude::*, AXAttribute, AXUIElement, Id, NSRunningApplication, Observer},
};

pub type AppInfo = crate::sys::app::AppInfo;
pub type WindowInfo = crate::system::WindowInfo;

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

#[derive(Clone)]
pub struct AppThreadHandle {
    requests_tx: Sender<(Span, Request)>,
}

impl AppThreadHandle {
    pub(crate) fn new_for_test(requests_tx: Sender<(Span, Request)>) -> Self {
        let this = AppThreadHandle { requests_tx };
        this
    }

    pub fn send(&self, req: Request) -> anyhow::Result<()> {
        self.requests_tx.send((Span::current(), req))?;
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

#[derive(Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize)]
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
        let _lock = trace_misc("RT lock", || self.0 .0.lock()).unwrap();
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

#[cfg(not(test))]
pub fn spawn_initial_app_threads(events_tx: reactor::Sender) {
    for (pid, info) in running_apps(None) {
        spawn_app_thread(pid, info, events_tx.clone());
    }
}

#[cfg(not(test))]
pub fn spawn_app_thread(pid: pid_t, info: AppInfo, events_tx: reactor::Sender) {
    thread::Builder::new()
        .name(format!(
            "{}({pid})",
            info.bundle_id.as_deref().unwrap_or("")
        ))
        .spawn(move || app_thread_main(pid, info, events_tx))
        .unwrap();
}

#[cfg_attr(test, allow(dead_code))]
struct State {
    pid: pid_t,
    bundle_id: Option<String>,
    running_app: Id<NSRunningApplication>,
    app: AXUIElement,
    observer: Observer,
    window_server: WindowServer,
    events_tx: reactor::Sender,
    windows: HashMap<WindowId, WindowState>,
    last_window_idx: u32,
    main_window: Option<WindowId>,
    last_activated: Option<(Instant, Quiet, Option<oneshot::Sender<()>>)>,
    is_frontmost: bool,
}

struct WindowState {
    elem: AXUIElement,
    last_seen_txid: TransactionId,
}

#[cfg_attr(test, allow(dead_code))]
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
    async fn run(
        mut self,
        info: AppInfo,
        requests_tx: Sender<(Span, Request)>,
        requests_rx: Receiver<(Span, Request)>,
        notifications_rx: Receiver<(AXUIElement, String)>,
    ) {
        let handle = AppThreadHandle { requests_tx };
        if !self.init(handle, info) {
            return;
        }

        pub enum Incoming {
            Notification((AXUIElement, String)),
            Request((Span, Request)),
        }

        let mut merged = StreamExt::merge(
            UnboundedReceiverStream::new(requests_rx).map(Incoming::Request),
            UnboundedReceiverStream::new(notifications_rx).map(Incoming::Notification),
        );

        while let Some(incoming) = merged.next().await {
            match incoming {
                Incoming::Request((span, mut request)) => {
                    let _guard = span.enter();
                    debug!(?self.bundle_id, ?self.pid, ?request, "Got request");
                    match self.handle_request(&mut request) {
                        Ok(should_terminate) if should_terminate => break,
                        Ok(_) => (),
                        Err(err) => {
                            error!(?self.bundle_id, ?self.pid, ?request, "Error handling request: {err}");
                        }
                    }
                }
                Incoming::Notification((elem, notif)) => {
                    self.handle_notification(elem, &notif);
                }
            }
        }
    }

    #[instrument(skip_all, fields(?info))]
    #[must_use]
    fn init(&mut self, handle: AppThreadHandle, info: AppInfo) -> bool {
        // Register for notifications on the application element.
        for notif in APP_NOTIFICATIONS {
            let res = self.observer.add_notification(self.app.inner(), notif);
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
        let mut wsids = Vec::with_capacity(initial_window_elements.len() as usize);
        for elem in initial_window_elements.iter() {
            let elem = elem.clone();
            let wsid = WindowServerId::try_from(&elem).ok();
            let Some((info, wid)) = self.register_window(elem) else {
                continue;
            };
            if let Some(wsid) = wsid {
                wsids.push(wsid.as_u32());
            }
            windows.push((wid, info));
        }
        let window_server_info = self.window_server.get_windows(&wsids);

        self.main_window = self.app.main_window().ok().and_then(|w| self.id(&w).ok());
        self.is_frontmost = self.app.frontmost().map(|b| b.into()).unwrap_or(false);

        // Send the ApplicationLaunched event.
        if self
            .events_tx
            .send((
                Span::current(),
                Event::ApplicationLaunched {
                    pid: self.pid,
                    handle,
                    info,
                    is_frontmost: self.is_frontmost,
                    main_window: self.main_window,
                    visible_windows: windows,
                    window_server_info,
                },
            ))
            .is_err()
        {
            debug!(pid = ?self.pid, "Failed to send ApplicationLaunched event, exiting thread");
            return false;
        };

        true
    }

    /// Handles a request. Returns whether the actor should terminate.
    #[instrument(skip_all, fields(app = ?self.app, ?request))]
    fn handle_request(&mut self, request: &mut Request) -> Result<bool, accessibility::Error> {
        match request {
            Request::Terminate => {
                CFRunLoop::get_current().stop();
                self.send_event(Event::ApplicationThreadTerminated(self.pid));
                return Ok(true);
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
                    if let Ok(id) = self.id(&elem) {
                        known_visible.push(id);
                        continue;
                    }
                    let Some((info, wid)) = self.register_window(elem) else {
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
                    // We don't need to check the mouse state since we know this
                    // change was requested by the reactor.
                    None,
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
                    None,
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
                    None,
                ));
            }
            &mut Request::Raise(wid, ref token, ref mut done, quiet) => {
                self.handle_raise_request(wid, token, done, quiet)?;
            }
        }
        Ok(false)
    }

    #[instrument(skip_all, fields(app = ?self.app, ?notif))]
    fn handle_notification(&mut self, elem: AXUIElement, notif: &str) {
        trace!(?notif, ?elem, "Got notification");
        #[allow(non_upper_case_globals)]
        #[forbid(non_snake_case)]
        // TODO: Handle all of these.
        match notif {
            kAXApplicationActivatedNotification | kAXApplicationDeactivatedNotification => {
                _ = self.on_activation_changed();
            }
            kAXMainWindowChangedNotification => {
                self.on_main_window_changed(None);
            }
            kAXWindowCreatedNotification => {
                if self.id(&elem).is_ok() {
                    // We already registered this window because of an earlier event.
                    return;
                }
                let Some((window, wid)) = self.register_window(elem) else {
                    return;
                };
                let window_server_info = self.window_server.get_window(wid.idx.into());
                self.send_event(Event::WindowCreated(
                    wid,
                    window,
                    window_server_info,
                    self.window_server.get_mouse_state(),
                ));
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
                    Some(self.window_server.get_mouse_state()),
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

    fn handle_raise_request(
        &mut self,
        wid: WindowId,
        token: &RaiseToken,
        done: &mut Option<oneshot::Sender<()>>,
        quiet: Quiet,
    ) -> Result<(), accessibility::Error> {
        let window = self.window(wid)?;
        trace("raise", &window.elem, || window.elem.raise())?;
        let quiet_if = (quiet == Quiet::Yes).then_some(wid);
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
        // We can hold the token until then, but will need to time out
        // just in case the activation silently fails for some reason.
        token
            .with(self.pid, || {
                // Check whether the app thinks it is frontmost. If it
                // does we won't get an activated event, so don't
                // bother activating it.
                //
                // We read the value directly instead of using the
                // cached value because it's possible the cache is
                // outdated and the app is no longer frontmost. If that
                // happens, it's important that we activate the app or
                // the window may never be raised.
                //
                // Note that it is possible for the app to be outdated
                // since the window server is the source of truth. For
                // now we don't handle this case. We could handle it
                // by looking at the window list and seeing whether
                // the requested window is indeed frontmost.
                if trace("is_frontmost", &self.app, || self.app.frontmost())?.into() {
                    trace!("App is already frontmost; skipping activation");
                    // Declare success.
                    _ = done.take().map(|s| s.send(()));
                    return Ok(());
                }
                // This option is deprecated, but there is no alternative.
                #[allow(non_upper_case_globals)]
                const NSApplicationActivateIgnoringOtherApps: NSApplicationActivationOptions =
                    1 << 1;
                // SAFETY: This method should be marked as safe.
                let success = trace_misc("activate", || unsafe {
                    self.running_app.activateWithOptions(NSApplicationActivateIgnoringOtherApps)
                });
                if success {
                    // Record the activation so we can match against its
                    // notification and correctly mark it as quiet.
                    self.last_activated = Some((Instant::now(), quiet, done.take()));
                } else {
                    warn!(?self.pid, "Failed to activate app");
                }
                Ok(())
            })
            .unwrap_or(Ok(()))
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
        // Often we get this event for new windows before the WindowCreated
        // notification. If that happens, register it and send the corresponding
        // event here.
        let wid = match self.id(&elem).ok() {
            Some(wid) => wid,
            None => {
                let Some((info, wid)) = self.register_window(elem) else {
                    warn!(?self.pid, "Got MainWindowChanged on unknown window");
                    return None;
                };
                let window_server_info = self.window_server.get_window(wid.idx.into());
                self.send_event(Event::WindowCreated(
                    wid,
                    info,
                    window_server_info,
                    self.window_server.get_mouse_state(),
                ));
                wid
            }
        };
        // Suppress redundant events. This is so we don't repeat an event that
        // was manufactured as a quiet event before.
        if self.main_window == Some(wid) {
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

    fn on_activation_changed(&mut self) -> Result<(), accessibility::Error> {
        // Regardless of the notification we received, read the current activation
        // and base our event on that. This has the effect of "collapsing" old
        // stale events, ensuring that they don't interfere with the matching
        // we do below.
        let is_frontmost: bool = trace("is_frontmost", &self.app, || self.app.frontmost())?.into();
        if is_frontmost == self.is_frontmost {
            return Ok(());
        }
        self.is_frontmost = is_frontmost;

        if is_frontmost {
            // Suppress events from our own activation by attempting to match up
            // the event with `self.last_activated`.
            //
            // Since it is possible for an activation to not happen for some reason,
            // we are stuck with using a timeout so we don't suppress real events
            // in the future.
            let quiet = match self.last_activated.take() {
                // Idea: Maybe we can control the timeout in client and use the send
                // result to decide whether `quiet` applies.
                Some((ts, quiet, done)) if ts.elapsed() < Duration::from_millis(1000) => {
                    // Initiated by us.
                    _ = done.map(|s| s.send(()));
                    quiet
                }
                _ => {
                    // Initiated by the user or system.
                    // Unfortunately, if the user clicks on a new main window to
                    // activate this app, we get this notification before getting
                    // the main window changed notification. First read the main
                    // window and send a notification if it changed.
                    self.on_main_window_changed(None);
                    Quiet::No
                }
            };
            self.send_event(Event::ApplicationActivated(self.pid, quiet));
        } else {
            self.send_event(Event::ApplicationDeactivated(self.pid));
        }
        Ok(())
    }

    #[must_use]
    fn register_window(&mut self, elem: AXUIElement) -> Option<(WindowInfo, WindowId)> {
        let Ok(mut info) = WindowInfo::try_from(&elem) else {
            return None;
        };

        // HACK: Ignore hotkey iTerm2 windows.
        // Obviously this should be done with some configurable feature.
        if self.bundle_id.as_deref() == Some("com.googlecode.iterm2")
            && elem
                .attribute(&AXAttribute::new(&CFString::from_static_string(
                    "AXTitleUIElement",
                )))
                .is_err()
        {
            info.is_standard = false;
        }

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
        return Some((info, wid));

        fn register_notifs(win: &AXUIElement, state: &State) -> bool {
            // Filter out elements that aren't regular windows.
            match win.role() {
                Ok(role) if role == kAXWindowRole => (),
                _ => return false,
            }
            for notif in WINDOW_NOTIFICATIONS {
                let res = state.observer.add_notification(win.inner(), notif);
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
        if let Ok(id) = WindowServerId::try_from(elem) {
            let wid = WindowId {
                pid: self.pid,
                idx: NonZeroU32::new(id.as_u32()).expect("Window server id was 0"),
            };
            if self.windows.contains_key(&wid) {
                return Ok(wid);
            }
        } else if let Some((&wid, _)) = self.windows.iter().find(|(_, w)| &w.elem == elem) {
            return Ok(wid);
        }
        Err(accessibility::Error::NotFound)
    }

    fn stop_notifications_for_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let res = self.observer.remove_notification(elem.inner(), notif);
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
            let res = self.observer.add_notification(elem.inner(), notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!(?notif, ?elem, "Adding notification failed with error {err}");
            }
        }
    }
}

#[cfg(not(test))]
fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: reactor::Sender) {
    let app = AXUIElement::application(pid);
    let Some(running_app) = NSRunningApplication::with_process_id(pid) else {
        debug!(
            ?pid,
            "Making NSRunningApplication failed; exiting app thread"
        );
        return;
    };

    // Set up the observer callback.
    let Ok(observer) = Observer::new(pid) else {
        debug!(?pid, "Making observer failed; exiting app thread");
        return;
    };
    let (notifications_tx, notifications_rx) = channel();
    let observer = observer
        .install(move |elem, notif| _ = notifications_tx.send((elem.into(), notif.to_owned())));

    // Create our app state.
    let state = State {
        pid,
        running_app,
        bundle_id: info.bundle_id.clone(),
        app: app.clone(),
        observer,
        window_server: WindowServer::new(),
        events_tx,
        windows: HashMap::default(),
        last_window_idx: 0,
        main_window: None,
        last_activated: None,
        is_frontmost: false,
    };

    let (requests_tx, requests_rx) = channel();
    Executor::run(state.run(info, requests_tx, requests_rx, notifications_rx));
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

fn trace_misc<T>(desc: &str, f: impl FnOnce() -> T) -> T {
    let start = Instant::now();
    let out = f();
    let end = Instant::now();
    trace!(time = ?(end - start), "{desc:12}");
    out
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use icrate::Foundation::CGSize;
    use reactor::{Reactor, Record};
    use test_log::test;

    use super::*;
    use crate::{
        actor::layout::LayoutManager,
        sys::screen::SpaceId,
        system::fake::{self, FakeNSRunningApplication, FakeObserver},
    };

    #[cfg(loom)]
    #[test]
    fn loom_test() {
        loom::model(|| {
            test_app_actor();
        });
    }

    #[cfg(loom)]
    #[test]
    fn loom_test2() {
        loom::model(|| {
            loom::thread::spawn(move || {
                loom::thread::park();
            });
        });
    }

    #[cfg(loom)]
    #[test]
    fn loom_test3() {
        loom::model(|| {
            use loom::sync::{Arc, Mutex};
            let m1 = Arc::new(Mutex::new(0));
            let m2 = Arc::new(Mutex::new(0));
            let (n1, n2) = (m1.clone(), m2.clone());
            loom::thread::spawn(move || {
                for _ in 0..3 {
                    let mut l1 = m1.lock().unwrap();
                    *l1 += 1;
                    *m2.lock().unwrap() += 1;
                }
            });
            loom::thread::spawn(move || {
                for _ in 0..3 {
                    let mut l1 = n2.lock().unwrap();
                    *l1 += 1;
                    *n1.lock().unwrap() += 1;
                }
            });
        });
    }

    #[cfg(loom)]
    #[test]
    fn loom_test4() {
        loom::model(|| {
            loom::thread::spawn(move || loom::future::block_on(std::future::pending::<()>()));
        });
    }

    #[cfg(loom)]
    #[test]
    fn loom_test_move_window() {
        println!("test");
        loom::model(test_move_window);
    }

    mod thread {
        #[cfg(not(loom))]
        pub use std::thread::spawn;

        #[cfg(loom)]
        pub fn spawn<T: Send + 'static>(
            f: impl FnOnce() -> T + Send + 'static,
        ) -> loom::thread::JoinHandle<T> {
            loom::thread::Builder::new().stack_size(0x2000).spawn(f).unwrap()
        }
    }

    #[test]
    fn test_move_window() {
        let pid = 1234;
        let (events_tx, events_rx) = channel();
        let (requests_tx, requests_rx) = channel();
        let (notifications_tx, notifications_rx) = channel();

        let server = fake::WindowServer::new();
        let observer = FakeObserver::new(notifications_tx);
        let app = fake::Application::new(pid, server.clone(), observer.clone());
        let running_app = Id::new(FakeNSRunningApplication);

        let app_actor = State {
            pid,
            running_app,
            bundle_id: Some("com.example.test".to_string()),
            app: app.clone().into(),
            observer: observer.clone(),
            window_server: server,
            events_tx: events_tx.clone(),
            windows: HashMap::default(),
            last_window_idx: 0,
            main_window: None,
            last_activated: None,
            is_frontmost: false,
        };

        let reactor_thread = thread::spawn(move || {
            let reactor = Reactor::new(LayoutManager::new());
            Executor::run(reactor.run(events_rx, Record::new(None)));
        });

        let screen_frame = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        _ = events_tx.send((
            Span::current(),
            Event::ScreenParametersChanged(vec![screen_frame], vec![Some(SpaceId::new(1))], vec![]),
        ));

        let win: AXUIElement = app.mk_window().into();

        let info = AppInfo {
            bundle_id: Some("dev.myapp".into()),
            localized_name: Some("MyApp".into()),
        };
        let rtx = requests_tx.clone();
        _ = thread::spawn(move || {
            Executor::run(app_actor.run(info, rtx, requests_rx, notifications_rx));
        });

        Executor::run(async {
            let mut updates = observer.updates_rx().unwrap();
            while let Some(()) = updates.recv().await {
                if win.frame().unwrap().to_icrate() == screen_frame {
                    break;
                }
            }
        });

        _ = requests_tx.send((Span::current(), Request::Terminate));
        drop(events_tx);
        reactor_thread.join().unwrap();

        assert_eq!(win.frame().unwrap().to_icrate(), screen_frame);
        // _ = events_tx.send((Span::current(), Event::Command(reactor::Command::Exit)));
    }

    #[test]
    fn test_app_actor() {
        let pid = 1234;
        let (events_tx, mut events_rx) = channel();
        let (notifications_tx, _notifications_rx) = channel();

        let server = fake::WindowServer::new();
        let observer = FakeObserver::new(notifications_tx);
        let app = fake::Application::new(pid, server.clone(), observer.clone());
        let running_app = Id::new(FakeNSRunningApplication);

        let state = Rc::new(RefCell::new(State {
            pid,
            running_app,
            bundle_id: Some("com.example.test".to_string()),
            app: app.clone().into(),
            observer,
            window_server: server,
            events_tx,
            windows: HashMap::default(),
            last_window_idx: 0,
            main_window: None,
            last_activated: None,
            is_frontmost: false,
        }));

        state.borrow_mut().handle_request(&mut Request::GetVisibleWindows).unwrap();
        if let Ok((_, event)) = events_rx.try_recv() {
            match event {
                Event::WindowsDiscovered {
                    pid: event_pid,
                    new,
                    known_visible,
                } => {
                    assert_eq!(event_pid, pid);
                    assert!(new.is_empty());
                    assert!(known_visible.is_empty());
                }
                _ => panic!("Unexpected event type"),
            }
        } else {
            panic!("No event received");
        }

        let win = app.mk_window();
        state.borrow_mut().handle_request(&mut Request::GetVisibleWindows).unwrap();
        if let Ok((_, event)) = events_rx.try_recv() {
            match event {
                Event::WindowsDiscovered {
                    pid: event_pid,
                    new,
                    known_visible,
                } => {
                    assert_eq!(event_pid, pid);
                    assert_eq!(new.len(), 1);
                    assert_eq!(new[0].0, WindowId::new(pid, win.id()));
                    assert!(known_visible.is_empty());
                }
                _ => panic!("Unexpected event type"),
            }
        } else {
            panic!("No event received");
        }
    }
}
