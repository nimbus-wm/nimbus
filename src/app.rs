use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::c_void,
    fmt::Debug,
    ptr,
    rc::Rc,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::{
    kAXErrorSuccess, kAXMainWindowChangedNotification, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
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
use icrate::{
    objc2::msg_send,
    AppKit::{NSRunningApplication, NSWorkspace},
};
use log::{debug, error, trace};

use crate::{run_loop::WakeupHandle, Event, Opt, Window};

pub use accessibility_sys::pid_t;

pub(crate) trait NSRunningApplicationExt {
    #[allow(non_snake_case)]
    fn processIdentifier(&self) -> pid_t;
}
impl NSRunningApplicationExt for NSRunningApplication {
    #[allow(non_snake_case)]
    fn processIdentifier(&self) -> pid_t {
        unsafe { msg_send![self, processIdentifier] }
    }
}

pub(crate) fn running_apps(opt: &Opt) -> impl Iterator<Item = (pid_t, String)> {
    let bundle = opt.bundle.clone();
    unsafe { NSWorkspace::sharedWorkspace().runningApplications() }
        .into_iter()
        .flat_map(move |app| {
            let bundle_id = unsafe { app.bundleIdentifier() }?.to_string();
            if let Some(filter) = &bundle {
                if !bundle_id.contains(filter) {
                    return None;
                }
            }
            Some((app.processIdentifier(), bundle_id))
        })
}

pub(crate) struct ThreadHandle {
    requests_tx: Sender<Request>,
    wakeup: WakeupHandle,
}

impl ThreadHandle {
    pub(crate) fn send(&self, req: Request) -> Result<(), std::sync::mpsc::SendError<Request>> {
        self.requests_tx.send(req)?;
        self.wakeup.wake();
        Ok(())
    }
}

impl Debug for ThreadHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ThreadHandle").finish()
    }
}

#[derive(Debug)]
pub(crate) struct Request;

pub(crate) fn spawn_initial_app_threads(opt: &Opt, events_tx: Sender<Event>) {
    for (pid, _bundle_id) in running_apps(opt) {
        spawn_app_thread(pid, events_tx.clone());
    }
}

pub(crate) fn spawn_app_thread(pid: pid_t, events_tx: Sender<Event>) {
    thread::spawn(move || app_thread_main(pid, events_tx));
}

fn app_thread_main(pid: pid_t, events_tx: Sender<Event>) {
    let app = AXUIElement::application(pid);

    type State = Rc<RefCell<StateInner>>;
    struct StateInner {
        window_elements: Vec<AXUIElement>,
        events_tx: Sender<Event>,
        requests_rx: Receiver<Request>,
    }
    let (requests_tx, requests_rx) = channel();
    let state = Rc::new(RefCell::new(StateInner {
        window_elements: vec![],
        events_tx,
        requests_rx,
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
        let Ok(window) = Window::try_from_ui_element(&elem) else {
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
    let handle = ThreadHandle {
        requests_tx,
        wakeup,
    };

    // Send the ApplicationLaunched event.
    let Ok(()) = state_ref
        .events_tx
        .send(Event::ApplicationLaunched(pid, handle, windows))
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
        let state = unsafe { &*(data as *const State) };
        let notif = unsafe { CFString::wrap_under_get_rule(notif) };
        let notif = Cow::<str>::from(&notif);
        let elem = unsafe { AXUIElement::wrap_under_get_rule(elem) };
        trace!("Got {notif:?} on {elem:?}");

        #[allow(non_upper_case_globals)]
        #[forbid(non_snake_case)]
        // TODO: Handle all of these.
        match &*notif {
            kAXWindowCreatedNotification => {
                if register_window_notifs(&elem, state, observer) {
                    state.borrow_mut().window_elements.push(elem);
                }
            }
            kAXUIElementDestroyedNotification => {
                state.borrow_mut().window_elements.retain(|w| *w != elem);
            }
            kAXMainWindowChangedNotification => {}
            kAXWindowMovedNotification => {
                state.borrow().events_tx.send(Event::WindowMoved).unwrap();
            }
            kAXWindowResizedNotification => {}
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
            debug!("Got request: {request:?}");
        }
    }
}
