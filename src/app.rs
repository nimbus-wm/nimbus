use std::{borrow::Cow, cell::RefCell, ffi::c_void, ptr, sync::mpsc::Sender, thread};

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::{
    kAXErrorSuccess, kAXMainWindowChangedNotification, kAXTitleChangedNotification,
    kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole, pid_t,
    AXObserverAddNotification, AXObserverCreate, AXObserverGetRunLoopSource, AXObserverRef,
    AXUIElementRef,
};
use core_foundation::{
    base::TCFType,
    runloop::{kCFRunLoopCommonModes, CFRunLoopAddSource, CFRunLoopGetCurrent, CFRunLoopRun},
    string::{CFString, CFStringRef},
};
use icrate::{objc2::msg_send, AppKit::NSWorkspace};
use log::{error, trace};

use crate::{Event, Opt, Window};

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
            let pid: pid_t = unsafe { msg_send![&*app, processIdentifier] };
            Some((pid, bundle_id))
        })
}

pub(crate) fn spawn_app_threads(
    opt: &Opt,
    initial_windows_tx: Sender<Vec<Window>>,
    events_tx: Sender<Event>,
) {
    for (pid, bundle_id) in running_apps(opt) {
        let windows = initial_windows_tx.clone();
        let events = events_tx.clone();
        let _ = thread::spawn(move || app_thread_main(pid, bundle_id, windows, events));
    }
}

pub(crate) fn app_thread_main(
    pid: pid_t,
    bundle_id: String,
    initial_windows_tx: Sender<Vec<Window>>,
    events_tx: Sender<Event>,
) {
    let Ok(app) = AXUIElement::application_with_bundle(&bundle_id) else {
        return;
    };
    let Ok(window_elements) = app.windows() else {
        return;
    };

    type State = RefCell<StateInner>;
    struct StateInner {
        window_elements: Vec<AXUIElement>,
        events_tx: Sender<Event>,
    }
    let state = RefCell::new(StateInner {
        window_elements: window_elements.into_iter().map(|w| w.clone()).collect(),
        events_tx,
    });

    // SAFETY: Notifications can only be delivered inside this function, during
    // the call to CFRunLoopRun(). We are careful not to move `state`..
    // TODO: Wrap in a type that releases on drop. Pin the state.
    let mut observer: AXObserverRef = ptr::null_mut();
    unsafe {
        AXObserverCreate(pid, callback, &mut observer);
        let source = AXObserverGetRunLoopSource(observer);
        CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopCommonModes);
    }
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
    state
        .borrow_mut()
        .window_elements
        .retain(|win| register_window_notifs(win, &state, observer));

    let Ok(windows) = state
        .borrow()
        .window_elements
        .iter()
        .map(Window::try_from_ui_element)
        .collect()
    else {
        return;
    };
    initial_windows_tx.send(windows).unwrap();
    drop(initial_windows_tx);

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
        match &*notif {
            kAXWindowCreatedNotification => {
                if register_window_notifs(&elem, state, observer) {
                    state.borrow_mut().window_elements.push(elem);
                }
            }
            kAXUIElementDestroyedNotification => {
                state.borrow_mut().window_elements.retain(|w| *w != elem);
            }
            kAXWindowMovedNotification => {
                state
                    .borrow_mut()
                    .events_tx
                    .send(Event::WindowMoved)
                    .unwrap();
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

    unsafe {
        CFRunLoopRun();
    }
}
