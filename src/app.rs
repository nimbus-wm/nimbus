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

use accessibility::{AXUIElement, AXUIElementAttributes};
pub use accessibility_sys::pid_t;
use accessibility_sys::{
    kAXErrorSuccess, kAXMainWindowChangedNotification, kAXStandardWindowSubrole,
    kAXTitleChangedNotification, kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole,
    AXObserverAddNotification, AXObserverCreate, AXObserverGetRunLoopSource, AXObserverRef,
    AXUIElementRef,
};
use core_foundation::{
    base::TCFType,
    runloop::{kCFRunLoopCommonModes, CFRunLoop, CFRunLoopAddSource, CFRunLoopGetCurrent},
    string::{CFString, CFStringRef},
};
use core_graphics_types::geometry::CGRect;
use icrate::{
    objc2::{msg_send, rc::Id},
    AppKit::{NSRunningApplication, NSWorkspace},
    Foundation::NSString,
};
use log::{debug, error, trace};

use crate::{
    reactor::{self, Event, Window, WindowIdx},
    run_loop::WakeupHandle,
    Opt,
};

pub(crate) trait NSRunningApplicationExt {
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

impl TryFrom<&AXUIElement> for reactor::Window {
    type Error = accessibility::Error;
    fn try_from(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        Ok(reactor::Window {
            is_standard: element.role()? == kAXWindowRole
                && element.subrole()? == kAXStandardWindowSubrole,
            title: element.title()?.to_string(),
            frame: element.frame()?,
        })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct AppInfo {
    pub bundle_id: Option<String>,
    pub localized_name: Option<String>,
}

impl From<&NSRunningApplication> for AppInfo {
    fn from(app: &NSRunningApplication) -> Self {
        AppInfo {
            bundle_id: app.bundle_id().as_deref().map(ToString::to_string),
            localized_name: app.localized_name().as_deref().map(ToString::to_string),
        }
    }
}

pub(crate) fn running_apps(opt: &Opt) -> impl Iterator<Item = (pid_t, AppInfo)> {
    let bundle = opt.bundle.clone();
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

pub(crate) struct AppThreadHandle {
    requests_tx: Sender<Request>,
    wakeup: WakeupHandle,
}

impl AppThreadHandle {
    pub(crate) fn send(&self, req: Request) -> Result<(), std::sync::mpsc::SendError<Request>> {
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
pub(crate) enum Request {
    SetWindowFrame(WindowIdx, CGRect),
}

pub(crate) fn spawn_initial_app_threads(opt: &Opt, events_tx: Sender<Event>) {
    for (pid, info) in running_apps(opt) {
        spawn_app_thread(pid, info, events_tx.clone());
    }
}

pub(crate) fn spawn_app_thread(pid: pid_t, info: AppInfo, events_tx: Sender<Event>) {
    thread::spawn(move || app_thread_main(pid, info, events_tx));
}

fn app_thread_main(pid: pid_t, info: AppInfo, events_tx: Sender<Event>) {
    let app = AXUIElement::application(pid);

    type State = Rc<RefCell<StateInner>>;
    struct StateInner {
        window_elements: Vec<AXUIElement>,
        events_tx: Sender<Event>,
        requests_rx: Receiver<Request>,
        pid: pid_t,
        bundle_id: Option<String>,
    }
    let (requests_tx, requests_rx) = channel();
    let state = Rc::new(RefCell::new(StateInner {
        window_elements: vec![],
        events_tx,
        requests_rx,
        pid,
        bundle_id: info.bundle_id.clone(),
    }));
    let mut state_ref = state.borrow_mut();

    // SAFETY: Notifications can only be delivered inside this function, during
    // the call to CFRunLoopRun(). We are careful not to move `state`..
    // TODO: Wrap in a type that releases on drop. Pin the state.
    let mut observer: AXObserverRef = ptr::null_mut();
    unsafe {
        AXObserverCreate(pid, callback, &mut observer);
        let source = AXObserverGetRunLoopSource(observer);
        CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopCommonModes);
    }

    // Register for notifications on the application element.
    const GLOBAL_NOTIFICATIONS: &[&str] = &[
        kAXWindowCreatedNotification,
        kAXMainWindowChangedNotification,
    ];
    for notif in GLOBAL_NOTIFICATIONS {
        unsafe {
            AXObserverAddNotification(
                observer,
                app.as_concrete_TypeRef(),
                CFString::from_static_string(notif).as_concrete_TypeRef(),
                &state as *const State as *mut c_void,
            );
        }
    }

    // Now that we will observe new window events, read the list of windows.
    let Ok(initial_window_elements) = app.windows() else {
        // This is probably not a normal application, or it has exited.
        return;
    };

    // Process the list and register notifications on all windows.
    let mut window_elements = Vec::with_capacity(initial_window_elements.len() as usize);
    let mut windows = Vec::with_capacity(initial_window_elements.len() as usize);
    for elem in initial_window_elements.iter() {
        let elem = elem.clone();
        let Ok(window) = Window::try_from(&elem) else {
            continue;
        };
        if !register_window_notifs(&elem, &state, observer) {
            continue;
        }
        window_elements.push(elem);
        windows.push(window);
    }
    state_ref.window_elements = window_elements;

    // Set up our request handler.
    let st = state.clone();
    let wakeup = WakeupHandle::for_current_thread(0, move || handle_requests(&st));
    let handle = AppThreadHandle { requests_tx, wakeup };

    // Send the ApplicationLaunched event.
    let Ok(()) = state_ref.events_tx.send(Event::ApplicationLaunched(pid, info, handle, windows))
    else {
        debug!("Failed to send ApplicationLaunched event for {pid}, exiting thread");
        return;
    };

    // Finally, invoke the run loop to handle events.
    drop(state_ref);
    CFRunLoop::run_current();

    const WINDOW_NOTIFICATIONS: &[&str] = &[
        kAXUIElementDestroyedNotification,
        kAXWindowMovedNotification,
        kAXWindowResizedNotification,
        kAXWindowMiniaturizedNotification,
        kAXWindowDeminiaturizedNotification,
        kAXTitleChangedNotification,
    ];

    #[must_use]
    fn register_window_notifs(win: &AXUIElement, state: &State, observer: AXObserverRef) -> bool {
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
                    state as *const State as *mut c_void,
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
            kAXWindowCreatedNotification => {
                let Ok(window) = Window::try_from(&elem) else {
                    return;
                };
                if !register_window_notifs(&elem, state_ref, observer) {
                    return;
                }
                state.window_elements.push(elem);
                state.events_tx.send(Event::WindowCreated(state.pid, window)).unwrap();
            }
            kAXUIElementDestroyedNotification => {
                let Some(idx) = state.window_elements.iter().position(|w| w == &elem) else {
                    return;
                };
                state.window_elements.remove(idx);
                state
                    .events_tx
                    .send(Event::WindowDestroyed(state.pid, idx.try_into().unwrap()))
                    .unwrap();
            }
            kAXMainWindowChangedNotification => {}
            kAXWindowMovedNotification => {
                let Some(idx) = state.window_elements.iter().position(|w| w == &elem) else {
                    return;
                };
                let Ok(pos) = state.window_elements[idx].position() else {
                    return;
                };
                state
                    .events_tx
                    .send(Event::WindowMoved(state.pid, idx.try_into().unwrap(), pos))
                    .unwrap();
            }
            kAXWindowResizedNotification => {
                let Some(idx) = state.window_elements.iter().position(|w| w == &elem) else {
                    return;
                };
                let Ok(size) = state.window_elements[idx].size() else {
                    return;
                };
                state
                    .events_tx
                    .send(Event::WindowResized(
                        state.pid,
                        idx.try_into().unwrap(),
                        size,
                    ))
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
            Request::SetWindowFrame(idx, frame) => {
                let idx: usize = idx.try_into().unwrap();
                let window = &state.window_elements[idx];
                trace("set_position", window, || window.set_position(frame.origin))?;
                trace("set_size", window, || window.set_size(frame.size))?;
                let new_frame = state.window_elements[idx].frame()?;
                debug!("Frame after move: {new_frame:?}");
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
        trace!("{desc} took {:?}", end - start);
        trace!("for element {elem:#?}");
        if let Err(err) = &out {
            let app = elem.parent();
            debug!("{desc} failed with {err} for element {elem:#?} with parent {app:#?}");
        }
        out
    }
}
