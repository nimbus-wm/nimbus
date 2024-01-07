use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::c_void,
    future::Future,
    ptr,
    sync::{self, mpsc::Sender},
    thread,
    time::Instant,
};

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::{
    kAXErrorSuccess, kAXMainWindowChangedNotification, kAXResizedNotification,
    kAXTitleChangedNotification, kAXUIElementDestroyedNotification, kAXWindowCreatedNotification,
    kAXWindowDeminiaturizedNotification, kAXWindowMiniaturizedNotification,
    kAXWindowMovedNotification, pid_t, AXError, AXObserverAddNotification, AXObserverCreate,
    AXObserverGetRunLoopSource, AXObserverRef, AXUIElementRef,
};
use core_foundation::{
    array::CFArray,
    base::TCFType,
    dictionary::CFDictionaryRef,
    runloop::{kCFRunLoopCommonModes, CFRunLoopAddSource, CFRunLoopGetCurrent, CFRunLoopRun},
    string::{CFString, CFStringRef},
};
use core_graphics::{
    display::{CGDisplayBounds, CGMainDisplayID},
    window::CGWindowListCopyWindowInfo,
    window::{kCGNullWindowID, kCGWindowListOptionOnScreenOnly},
};
use core_graphics_types::geometry::CGRect;
use icrate::{
    objc2::{
        declare_class, msg_send, msg_send_id, mutability, rc::Allocated, rc::Id, sel, ClassType,
        DeclaredClass, Encode, Encoding,
    },
    AppKit::{self, NSApplication, NSWorkspace},
    Foundation::{MainThreadMarker, NSNotification, NSNotificationCenter, NSObject},
};
use structopt::StructOpt;
use tokio::sync::mpsc;

#[derive(StructOpt)]
pub struct Opt {
    pub bundle: Option<String>,
    pub resize: Option<String>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let opt = Opt::from_args();
    //time("accessibility serial", || get_windows_with_ax(&opt, true)).await;
    time("core-graphics", || get_windows_with_cg(&opt, true)).await;
    time("accessibility", || get_windows_with_ax(&opt, false, true)).await;
    time("core-graphics second time", || {
        get_windows_with_cg(&opt, false)
    })
    .await;
    time("accessibility second time", || {
        get_windows_with_ax(&opt, false, false)
    })
    .await;
    let (init, events) = spawn_event_handler(&opt);
    spawn_app_threads(&opt, init, events.clone());
    watch_for_notifications(events)
}

#[allow(dead_code)]
#[derive(Debug)]
struct Window {
    title: String,
    role: String,
    frame: CGRect,
}

impl Window {
    fn try_from_ui_element(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        Ok(Window {
            title: element.title()?.to_string(),
            role: element.role()?.to_string(),
            frame: element.frame()?,
        })
    }
}

async fn get_windows_with_cg(_opt: &Opt, print: bool) {
    let windows: CFArray<CFDictionaryRef> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly,
            kCGNullWindowID,
        ))
    };
    if print {
        println!("{windows:?}");
    }
    let display_id = unsafe { CGMainDisplayID() };
    let screen = unsafe { CGDisplayBounds(display_id) };
    println!("main display = {screen:?}");
}

fn running_apps(opt: &Opt) -> impl Iterator<Item = (pid_t, String)> {
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

async fn get_windows_with_ax(opt: &Opt, serial: bool, print: bool) {
    let (sender, mut receiver) = mpsc::unbounded_channel();
    for (pid, bundle_id) in running_apps(opt) {
        let sender = sender.clone();
        let task = move || {
            let app = AXUIElement::application(pid);
            let windows = get_windows_for_app(app);
            sender.send((bundle_id, windows)).unwrap()
        };
        if serial {
            task();
        } else {
            tokio::task::spawn_blocking(task);
        }
    }
    drop(sender);
    while let Some((bundle_id, windows)) = receiver.recv().await {
        //println!("{bundle_id}");
        match windows {
            Ok(windows) => {
                if print {
                    for win in windows {
                        println!("{win:?} from {bundle_id}");
                    }
                }
            }
            Err(_) => (), //println!("  * Error reading windows: {err:?}"),
        }
    }
}

fn get_windows_for_app(app: AXUIElement) -> Result<Vec<Window>, accessibility::Error> {
    let Ok(windows) = &app.windows() else {
        return Err(accessibility::Error::NotFound);
    };
    windows
        .into_iter()
        .map(|win| Window::try_from_ui_element(&*win))
        .collect()
}

async fn time<O, F: Future<Output = O>>(desc: &str, f: impl FnOnce() -> F) -> O {
    let start = Instant::now();
    let out = f().await;
    let end = Instant::now();
    println!("{desc} took {:?}", end - start);
    out
}

#[derive(Debug)]
enum Event {
    WindowMoved,
    ApplicationActivated,
    ApplicationLaunched(i32),
    ApplicationTerminated(i32),
    ScreenParametersChanged,
}

fn spawn_event_handler(_opt: &Opt) -> (Sender<Vec<Window>>, Sender<Event>) {
    let (initial_windows_tx, initial_windows) = sync::mpsc::channel();
    let (events_tx, events) = sync::mpsc::channel::<Event>();
    thread::spawn(move || {
        println!("\nInitial windows:");
        for windows in initial_windows {
            for window in windows {
                println!("- {window:?}");
            }
        }
        println!();

        for event in events {
            println!("Event {event:?}")
        }
    });
    (initial_windows_tx, events_tx)
}

fn spawn_app_threads(opt: &Opt, initial_windows_tx: Sender<Vec<Window>>, events_tx: Sender<Event>) {
    for (pid, bundle_id) in running_apps(opt) {
        let windows = initial_windows_tx.clone();
        let events = events_tx.clone();
        let _ = thread::spawn(move || app_thread_main(pid, bundle_id, windows, events));
    }
}

fn app_thread_main(
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
        kAXWindowMiniaturizedNotification,
        kAXWindowDeminiaturizedNotification,
        kAXWindowMovedNotification,
        kAXResizedNotification,
        kAXTitleChangedNotification,
    ];
    fn register_window_notifs(
        win: &AXUIElement,
        state: &State,
        observer: AXObserverRef,
    ) -> Result<(), AXError> {
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
                println!(
                    "Watching failed with error {} on window {win:#?}",
                    accessibility_sys::error_string(err)
                );
                return Err(err);
            }
        }
        Ok(())
    }
    state
        .borrow_mut()
        .window_elements
        .retain(|win| register_window_notifs(win, &state, observer).is_ok());

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
        println!("Got {notif:?} on {elem:?}");

        #[allow(non_upper_case_globals)]
        match &*notif {
            kAXWindowCreatedNotification => {
                if register_window_notifs(&elem, state, observer).is_ok() {
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
            _ => {
                // println!("Unexpected notification {notif:?} on {elem:#?}");
            }
        }
    }

    unsafe {
        CFRunLoopRun();
    }
}

fn watch_for_notifications(events_tx: Sender<Event>) {
    #[repr(C)]
    struct Instance {
        events_tx: &'static mut Sender<Event>,
    }

    unsafe impl Encode for Instance {
        const ENCODING: Encoding = Encoding::Object;
    }

    declare_class! {
        struct NotificationHandler;

        // SAFETY:
        // - The superclass NSObject does not have any subclassing requirements.
        // - Interior mutability is a safe default.
        // - `NotificationHandler` does not implement `Drop`.
        unsafe impl ClassType for NotificationHandler {
            type Super = NSObject;
            type Mutability = mutability::InteriorMutable;
            const NAME: &'static str = "NotificationHandler";
        }

        impl DeclaredClass for NotificationHandler {
            type Ivars = Box<Instance>;
        }

        // SAFETY: Each of these method signatures must match their invocations.
        unsafe impl NotificationHandler {
            #[method_id(initWith:)]
            fn init(this: Allocated<Self>, instance: Instance) -> Option<Id<Self>> {
                let this = this.set_ivars(Box::new(instance));
                unsafe { msg_send_id![super(this), init] }
            }

            #[method(handleActivated:)]
            fn handle_activated(&self, _notif: &NSNotification) {
                self.send_event(Event::ApplicationActivated);
            }

            #[method(handleLaunched:)]
            fn handle_launched(&self, _notif: &NSNotification) {
                // TODO: pid
                self.send_event(Event::ApplicationLaunched(0));
            }

            #[method(handleTerminated:)]
            fn handle_terminated(&self, _notif: &NSNotification) {
                // TODO: pid
                self.send_event(Event::ApplicationTerminated(0));
            }

            #[method(handleScreenChanged:)]
            fn handle_screen_changed(&self, _notif: &NSNotification) {
                self.send_event(Event::ScreenParametersChanged);
            }
        }
    }

    impl NotificationHandler {
        fn new(events_tx: Sender<Event>) -> Id<Self> {
            let events_tx = Box::leak(Box::new(events_tx));
            let instance = Instance { events_tx };
            unsafe { msg_send_id![Self::alloc(), initWith: instance] }
        }

        fn send_event(&self, event: Event) {
            if let Err(err) = self.ivars().events_tx.send(event) {
                eprintln!("Warning: Failed to send event: {err:?}");
            }
        }
    }

    let handler = NotificationHandler::new(events_tx);

    // SAFETY: Selector must have signature fn(&self, &NSNotification)
    let register_unsafe = |selector, notif_name, center: &Id<NSNotificationCenter>, object| unsafe {
        center.addObserver_selector_name_object(&handler, selector, Some(notif_name), Some(object));
    };

    let workspace = &unsafe { NSWorkspace::sharedWorkspace() };
    let workspace_center = &unsafe { workspace.notificationCenter() };
    let default_center = &unsafe { NSNotificationCenter::defaultCenter() };
    let shared_app = &NSApplication::sharedApplication(MainThreadMarker::new().unwrap());
    unsafe {
        use AppKit::*;
        register_unsafe(
            sel!(handleActivated:),
            NSWorkspaceDidActivateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(handleLaunched:),
            NSWorkspaceDidLaunchApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(handleTerminated:),
            NSWorkspaceDidTerminateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(handleScreenChanged:),
            NSApplicationDidChangeScreenParametersNotification,
            default_center,
            shared_app,
        );
    };

    unsafe {
        CFRunLoopRun();
    }
}

// Next:
// - Define a synchronous, long-lived task for each application.
// - Spawn each of these onto a thread pool. Ideally one thread per app.
// - Register AX observers on that thread's run loop.
// - Turn events into messages sent from the app threads and the main threads to
//   a single "wm logic" thread.
// - Bidirectional communication between this thread and the others becomes the
//   thing that async ops are built on (if we do that).
