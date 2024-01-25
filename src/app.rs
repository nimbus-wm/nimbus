use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::c_void,
    fmt::Debug,
    ptr,
    rc::Rc,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
    time::Instant,
};

use accessibility::{AXUIElement, AXUIElementActions, AXUIElementAttributes};
use accessibility_sys::{
    kAXApplicationActivatedNotification, kAXApplicationDeactivatedNotification, kAXErrorSuccess,
    kAXMainWindowChangedNotification, kAXStandardWindowSubrole, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
    AXObserverAddNotification, AXObserverCreate, AXObserverGetRunLoopSource, AXObserverRef,
    AXObserverRemoveNotification, AXUIElementRef,
};
use core_foundation::{
    base::TCFType,
    runloop::{kCFRunLoopCommonModes, CFRunLoop, CFRunLoopAddSource, CFRunLoopGetCurrent},
    string::{CFString, CFStringRef},
};
use icrate::{
    objc2::{msg_send, rc::Id},
    AppKit::{NSRunningApplication, NSWorkspace},
    Foundation::{CGPoint, CGRect, NSString},
};
use log::{debug, error, trace};

use crate::{
    reactor::{self, AppInfo, AppState, Event, WindowInfo},
    run_loop::WakeupHandle,
    util::{ToCGType, ToICrate},
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

pub trait NSRunningApplicationExt {
    fn pid(&self) -> pid_t;
    fn bundle_id(&self) -> Option<Id<NSString>>;
    fn localized_name(&self) -> Option<Id<NSString>>;
}

impl NSRunningApplicationExt for NSRunningApplication {
    fn pid(&self) -> pid_t {
        unsafe { msg_send![self, processIdentifier] }
    }
    fn bundle_id(&self) -> Option<Id<NSString>> {
        unsafe { self.bundleIdentifier() }
    }
    fn localized_name(&self) -> Option<Id<NSString>> {
        unsafe { self.localizedName() }
    }
}

impl TryFrom<&AXUIElement> for reactor::WindowInfo {
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
    requests_tx: Sender<Request>,
    wakeup: WakeupHandle,
}

impl AppThreadHandle {
    #[cfg(test)]
    pub(crate) fn new_for_test() -> (Self, Receiver<Request>) {
        let (requests_tx, requests_rx) = channel();
        let this = AppThreadHandle {
            requests_tx,
            wakeup: WakeupHandle::for_current_thread(0, || {}),
        };
        (this, requests_rx)
    }

    pub fn send(&self, req: Request) -> Result<(), std::sync::mpsc::SendError<Request>> {
        self.requests_tx.send(req)?;
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

    Raise(WindowId),
}

pub fn spawn_initial_app_threads(events_tx: Sender<Event>) {
    for (pid, info) in running_apps(None) {
        spawn_app_thread(pid, info, events_tx.clone());
    }
}

pub fn spawn_app_thread(pid: pid_t, info: AppInfo, events_tx: Sender<Event>) {
    thread::spawn(move || app_thread_main(pid, info, events_tx));
}

type State = Rc<RefCell<StateInner>>;

struct StateInner {
    app: AXUIElement,
    windows: Vec<WindowState>,
    events_tx: Sender<Event>,
    requests_rx: Receiver<Request>,
    pid: pid_t,
    bundle_id: Option<String>,
    last_window_idx: i32,
    observer: AXObserverRef,
    this: *const State,
}

struct WindowState {
    wid: WindowId,
    elem: AXUIElement,
}

impl StateInner {
    #[must_use]
    fn register_window(&mut self, elem: AXUIElement, observer: AXObserverRef) -> Option<WindowId> {
        if !register_notifs(&elem, self.this, observer) {
            return None;
        }
        self.last_window_idx += 1;
        let wid = WindowId {
            pid: self.pid,
            idx: self.last_window_idx,
        };
        self.windows.push(WindowState { elem, wid });
        return Some(wid);

        fn register_notifs(
            win: &AXUIElement,
            state: *const State,
            observer: AXObserverRef,
        ) -> bool {
            // Filter out elements that aren't regular windows.
            match win.role() {
                Ok(role) if role == kAXWindowRole => (),
                _ => return false,
            }
            for notif in WINDOW_NOTIFICATIONS {
                let err = unsafe {
                    AXObserverAddNotification(
                        observer,
                        win.as_concrete_TypeRef(),
                        CFString::from_static_string(notif).as_concrete_TypeRef(),
                        state as *mut c_void,
                    )
                };
                if err != kAXErrorSuccess {
                    trace!(
                        "Watching failed with error {} on window {win:#?}",
                        accessibility_sys::error_string(err)
                    );
                    return false;
                }
            }
            true
        }
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
            let err = unsafe {
                AXObserverRemoveNotification(
                    self.observer,
                    elem.as_concrete_TypeRef(),
                    CFString::from_static_string(notif).as_concrete_TypeRef(),
                )
            };
            if err != kAXErrorSuccess {
                // There isn't much we can do here except log and keep going.
                debug!("Removing notification {notif:?} on {elem:?} failed with error {err}");
            }
        }
    }

    fn restart_notifications_after_animation(&self, elem: &AXUIElement) {
        for notif in WINDOW_ANIMATION_NOTIFICATIONS {
            let err = unsafe {
                AXObserverAddNotification(
                    self.observer,
                    elem.as_concrete_TypeRef(),
                    CFString::from_static_string(notif).as_concrete_TypeRef(),
                    self.this as *mut c_void,
                )
            };
            if err != kAXErrorSuccess {
                // There isn't much we can do here except log and keep going.
                debug!("Adding notification {notif:?} on {elem:?} failed with error {err}");
            }
        }
    }
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

const WINDOW_ANIMATION_NOTIFICATIONS: &[&str] = &[
    kAXUIElementDestroyedNotification,
    kAXWindowMovedNotification,
    kAXWindowResizedNotification,
    kAXWindowMiniaturizedNotification,
    kAXWindowDeminiaturizedNotification,
    kAXTitleChangedNotification,
];

fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: Sender<Event>) {
    let app = AXUIElement::application(pid);
    let (requests_tx, requests_rx) = channel();

    // SAFETY: Notifications can only be delivered inside this function, during
    // the call to CFRunLoopRun(). We are careful not to move `state`..
    // TODO: Wrap in a type that releases on drop.
    let mut observer: AXObserverRef = ptr::null_mut();
    unsafe {
        AXObserverCreate(pid, callback, &mut observer);
        let source = AXObserverGetRunLoopSource(observer);
        CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopCommonModes);
    }

    let state = Rc::new(RefCell::new(StateInner {
        app: app.clone(),
        windows: vec![],
        events_tx,
        requests_rx,
        pid,
        bundle_id: info.bundle_id.clone(),
        last_window_idx: 0,
        observer,
        this: ptr::null(),
    }));

    // Pin the state, which will be live as long as the run loop runs.
    let state = &state;
    let mut state_ref = state.borrow_mut();
    state_ref.this = state as *const State;

    // Register for notifications on the application element.
    for notif in APP_NOTIFICATIONS {
        unsafe {
            AXObserverAddNotification(
                observer,
                app.as_concrete_TypeRef(),
                CFString::from_static_string(notif).as_concrete_TypeRef(),
                state as *const State as *mut c_void,
            );
        }
    }

    // Now that we will observe new window events, read the list of windows.
    let Ok(initial_window_elements) = app.windows() else {
        // This is probably not a normal application, or it has exited.
        return;
    };

    // Process the list and register notifications on all windows.
    state_ref.windows.reserve(initial_window_elements.len() as usize);
    let mut windows = Vec::with_capacity(initial_window_elements.len() as usize);
    for elem in initial_window_elements.iter() {
        let elem = elem.clone();
        let Ok(info) = WindowInfo::try_from(&elem) else {
            continue;
        };
        let Some(wid) = state_ref.register_window(elem, observer) else {
            continue;
        };
        windows.push((wid, info));
    }

    // Set up our request handler.
    let st = state.clone();
    let wakeup = WakeupHandle::for_current_thread(0, move || handle_requests(&st));
    let handle = AppThreadHandle { requests_tx, wakeup };

    // Send the ApplicationLaunched event.
    let app_state = AppState {
        handle,
        info,
        main_window: app.main_window().ok().and_then(|w| state_ref.id(w).ok()),
        is_frontmost: app.frontmost().map(|b| b.into()).unwrap_or(false),
    };
    let Ok(()) = state_ref.events_tx.send(Event::ApplicationLaunched(pid, app_state, windows))
    else {
        debug!("Failed to send ApplicationLaunched event for {pid}, exiting thread");
        return;
    };

    // Finally, invoke the run loop to handle events.
    drop(state_ref);
    CFRunLoop::run_current();

    unsafe extern "C" fn callback(
        observer: AXObserverRef,
        elem: AXUIElementRef,
        notif: CFStringRef,
        data: *mut c_void,
    ) {
        let state_ref = unsafe { &*(data as *const State) };
        let notif = unsafe { CFString::wrap_under_get_rule(notif) };
        let notif = Cow::<str>::from(&notif);
        let elem = unsafe { AXUIElement::wrap_under_get_rule(elem) };
        trace!("Got {notif:?} on {elem:?}");

        let mut state = state_ref.borrow_mut();

        #[allow(non_upper_case_globals)]
        #[forbid(non_snake_case)]
        // TODO: Handle all of these.
        match &*notif {
            kAXApplicationActivatedNotification => {
                // Unfortunately, if the user clicks on a new main window to
                // activate this app, we get this notification before getting
                // the main window changed notification. To distinguish from the
                // case where the app was activated and the main window has
                // *not* changed, we read the main window and send it along with
                // the notification.
                let main = elem.main_window().ok().and_then(|w| state.id(w).ok());
                state.events_tx.send(Event::ApplicationActivated(state.pid, main)).unwrap();
            }
            kAXApplicationDeactivatedNotification => {
                state.events_tx.send(Event::ApplicationDeactivated(state.pid)).unwrap();
            }
            kAXMainWindowChangedNotification => {
                let main = state.id(elem).ok();
                state
                    .events_tx
                    .send(Event::ApplicationMainWindowChanged(state.pid, main))
                    .unwrap();
            }
            kAXWindowCreatedNotification => {
                let Ok(window) = WindowInfo::try_from(&elem) else {
                    return;
                };
                let Some(wid) = state.register_window(elem, observer) else {
                    return;
                };
                state.events_tx.send(Event::WindowCreated(wid, window)).unwrap();
            }
            kAXUIElementDestroyedNotification => {
                let Some(idx) = state.windows.iter().position(|w| w.elem == elem) else {
                    return;
                };
                let wid = state.windows.remove(idx).wid;
                state.events_tx.send(Event::WindowDestroyed(wid)).unwrap();
            }
            kAXWindowMovedNotification => {
                let Some(window) = state.windows.iter().find(|w| w.elem == elem) else {
                    return;
                };
                let Ok(pos) = window.elem.position() else {
                    return;
                };
                state.events_tx.send(Event::WindowMoved(window.wid, pos.to_icrate())).unwrap();
            }
            kAXWindowResizedNotification => {
                let Some(window) = state.windows.iter().find(|w| w.elem == elem) else {
                    return;
                };
                let Ok(size) = window.elem.size() else {
                    return;
                };
                state
                    .events_tx
                    .send(Event::WindowResized(window.wid, size.to_icrate()))
                    .unwrap();
            }
            kAXWindowMiniaturizedNotification => {}
            kAXWindowDeminiaturizedNotification => {}
            kAXTitleChangedNotification => {}
            _ => {
                error!("Unhandled notification {notif:?} on {elem:#?}");
            }
        }
    }

    fn handle_requests(state: &State) {
        // Multiple source wakeups can be collapsed into one, so we have to make
        // sure all pending events are handled eventually. For now just handle
        // them all.
        let state = state.borrow();
        while let Ok(request) = state.requests_rx.try_recv() {
            let StateInner { bundle_id, pid, .. } = &*state;
            let bundle_id = bundle_id.as_deref().unwrap_or("None");
            debug!("Got request for {bundle_id}({pid}): {request:?}");
            match handle_request(&state, request.clone()) {
                Ok(()) => (),
                Err(err) => {
                    let StateInner { bundle_id, pid, .. } = &*state;
                    let bundle_id = bundle_id.as_deref().unwrap_or("None");
                    error!("Error handling request for {bundle_id}({pid}): {request:?}: {err}");
                }
            }
        }
    }

    fn handle_request(
        state: &std::cell::Ref<'_, StateInner>,
        request: Request,
    ) -> Result<(), accessibility::Error> {
        match request {
            Request::SetWindowPos(wid, pos) => {
                let window = state.window_element(wid)?;
                trace("set_position", window, || {
                    window.set_position(pos.to_cgtype())
                })?;
            }
            Request::SetWindowFrame(wid, frame) => {
                let window = state.window_element(wid)?;
                trace("set_position", window, || {
                    window.set_position(frame.origin.to_cgtype())
                })?;
                trace("set_size", window, || {
                    window.set_size(frame.size.to_cgtype())
                })?;
            }
            Request::BeginWindowAnimation(wid) => {
                let window = state.window_element(wid)?;
                state.stop_notifications_for_animation(window);
            }
            Request::EndWindowAnimation(wid) => {
                let window = state.window_element(wid)?;
                state.restart_notifications_after_animation(window);
                let pos = trace("position", window, || window.position())?;
                let size = trace("size", window, || window.size())?;
                state.events_tx.send(Event::WindowMoved(wid, pos.to_icrate())).unwrap();
                state.events_tx.send(Event::WindowResized(wid, size.to_icrate())).unwrap();
            }
            Request::Raise(wid) => {
                let window = state.window_element(wid)?;
                trace("raise", window, || window.raise())?;
                // FIXME: This request could be handled out of order with
                // respect to later requests sent to other apps by the reactor.
                // This breaks eventual consistency!
                //
                // In keeping with the current architecture, we should probably
                // fix this by responding to the reactor with an event at this
                // point. The reactor acts as a serialization point and can
                // check for stale requests and then send a request to the main
                // thread to actually call the API to activate the app.
                //
                // This is of course not the most efficient method, but we
                // should avoid breaking architectual boundaries when it's
                // unclear that performance is important.
                trace("set_frontmost", &state.app, || {
                    state.app.set_frontmost(true)
                })?;
            }
        }
        Ok(())
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
        // trace!("for element {elem:#?}");
        if let Err(err) = &out {
            let app = elem.parent();
            debug!("{desc} failed with {err} for element {elem:#?} with parent {app:#?}");
        }
        out
    }
}
