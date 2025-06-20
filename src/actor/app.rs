//! The app actor manages messaging to an application using the system
//! accessibility APIs.
//!
//! These APIs support reading and writing window states like position and size.

use std::fmt::Debug;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use accessibility::{AXAttribute, AXUIElement, AXUIElementActions, AXUIElementAttributes};
use accessibility_sys::{
    kAXApplicationActivatedNotification, kAXApplicationDeactivatedNotification,
    kAXMainWindowChangedNotification, kAXStandardWindowSubrole, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
};
use core_foundation::runloop::CFRunLoop;
use core_foundation::string::CFString;
use objc2::rc::Retained;
use objc2_app_kit::NSRunningApplication;
use objc2_core_foundation::{CGPoint, CGRect};
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::{
    UnboundedReceiver as Receiver, UnboundedSender as Sender, unbounded_channel as channel,
};
use tokio::sync::oneshot;
use tokio_stream::StreamExt;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tracing::{Span, debug, error, info, instrument, trace, warn};

use crate::actor::reactor::{self, Event, Requested, TransactionId};
use crate::collections::HashMap;
use crate::sys::app::NSRunningApplicationExt;
pub use crate::sys::app::{AppInfo, WindowInfo, pid_t};
use crate::sys::event;
use crate::sys::executor::Executor;
use crate::sys::geometry::{ToCGType, ToICrate};
use crate::sys::observer::Observer;
use crate::sys::window_server::{self, WindowServerId};

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
        let _lock = trace_misc("RT lock", || self.0.0.lock()).unwrap();
        if pid == self.0.1.load(Ordering::SeqCst) {
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
        self.0.1.store(pid, Ordering::SeqCst)
    }
}

pub fn spawn_app_thread(pid: pid_t, info: AppInfo, events_tx: reactor::Sender) {
    thread::Builder::new()
        .name(format!("{}({pid})", info.bundle_id.as_deref().unwrap_or("")))
        .spawn(move || app_thread_main(pid, info, events_tx))
        .unwrap();
}

struct State {
    pid: pid_t,
    bundle_id: Option<String>,
    #[expect(dead_code, reason = "unused for now")]
    running_app: Retained<NSRunningApplication>,
    app: AXUIElement,
    observer: Observer,
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
        let mut wsids = Vec::with_capacity(initial_window_elements.len() as usize);
        for elem in initial_window_elements.iter() {
            let elem = elem.clone();
            let wsid = WindowServerId::try_from(&elem).ok();
            let Some((info, wid)) = self.register_window(elem) else {
                continue;
            };
            if let Some(wsid) = wsid {
                wsids.push(wsid);
            }
            windows.push((wid, info));
        }
        let window_server_info = window_server::get_windows(&wsids);

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
                let window_server_info = window_server::get_window(WindowServerId(wid.idx.into()));
                self.send_event(Event::WindowCreated(
                    wid,
                    window,
                    window_server_info,
                    event::get_mouse_state(),
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
                    Some(event::get_mouse_state()),
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
                // does we won't get an activated event.
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
                let is_frontmost: bool =
                    trace("is_frontmost", &self.app, || self.app.frontmost())?.into();
                let window = self.window(wid)?;
                let is_standard =
                    window.elem.subrole().map(|s| s == kAXStandardWindowSubrole).unwrap_or(false);
                // Make this the key window regardless of is_frontmost. This
                // ensures that the window has focus and can receive keyboard events.
                let result = window_server::make_key_window(
                    self.pid,
                    WindowServerId::try_from(&self.window(wid)?.elem)?,
                );
                if result.is_err() {
                    warn!(?self.pid, "Failed to activate app");
                } else if !is_frontmost {
                    // We should be getting an activation event from make_key_window.
                    // Record the activation so we can match against its
                    // notification and correctly mark it as quiet.
                    //
                    // FIXME: We might not get the activation event, and
                    // this will deadlock the reactor.
                    //
                    // As a temporary workaround, don't expect activation events
                    // for non-standard windows or we will hit the deadlock
                    // mentioned above.
                    if is_standard {
                        self.last_activated = Some((Instant::now(), quiet, done.take()));
                    }
                }
                Ok(())
            })
            .unwrap_or(Ok(()))?;
        let window = self.window(wid)?;
        trace("raise", &window.elem, || window.elem.raise())?;
        let quiet_if = (quiet == Quiet::Yes).then_some(wid);
        let main_window = self.on_main_window_changed(quiet_if);
        if main_window != Some(wid) {
            warn!(
                "Raise request failed to raise {desired:?}; instead got {main_window:?}",
                desired = self.window(wid)?.elem
            );
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
        // Often we get this event for new windows before the WindowCreated
        // notification. If that happens, register it and send the corresponding
        // event here.
        // FIXME: This can happen ahead of a space change and result in us adding
        // a window to the wrong space.
        let wid = match self.id(&elem).ok() {
            Some(wid) => wid,
            None => {
                let Some((info, wid)) = self.register_window(elem) else {
                    warn!(?self.pid, "Got MainWindowChanged on unknown window");
                    return None;
                };
                let window_server_info = window_server::get_window(WindowServerId(wid.idx.into()));
                self.send_event(Event::WindowCreated(
                    wid,
                    info,
                    window_server_info,
                    event::get_mouse_state(),
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
        self.send_event(Event::ApplicationMainWindowChanged(self.pid, Some(wid), quiet));
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
            let res = self.observer.remove_notification(elem, notif);
            if let Err(err) = res {
                // There isn't much we can do here except log and keep going.
                debug!(?notif, ?elem, "Removing notification failed with error {err}");
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

fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: reactor::Sender) {
    let app = AXUIElement::application(pid);
    let Some(running_app) = NSRunningApplication::with_process_id(pid) else {
        debug!(?pid, "Making NSRunningApplication failed; exiting app thread");
        return;
    };

    // Set up the observer callback.
    let Ok(observer) = Observer::new(pid) else {
        debug!(?pid, "Making observer failed; exiting app thread");
        return;
    };
    let (notifications_tx, notifications_rx) = channel();
    let observer =
        observer.install(move |elem, notif| _ = notifications_tx.send((elem, notif.to_owned())));

    // Create our app state.
    let state = State {
        pid,
        running_app,
        bundle_id: info.bundle_id.clone(),
        app: app.clone(),
        observer,
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
