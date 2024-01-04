use std::{future::Future, time::Instant};

use accessibility::{AXUIElement, AXUIElementAttributes};
use core_foundation::{
    array::CFArray, base::TCFType, dictionary::CFDictionaryRef, runloop::CFRunLoopRun,
};
use core_graphics::{
    display::{CGDisplayBounds, CGMainDisplayID},
    window::CGWindowListCopyWindowInfo,
    window::{kCGNullWindowID, kCGWindowListOptionOnScreenOnly},
};
use core_graphics_types::geometry::CGRect;
use icrate::{
    objc2::{
        declare_class, msg_send_id, mutability, rc::Allocated, rc::Id, sel, ClassType,
        DeclaredClass,
    },
    AppKit::{self, NSApplication, NSWorkspace},
    Foundation::{
        MainThreadMarker, NSNotification, NSNotificationCenter, NSNotificationName, NSObject,
    },
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
    watch_for_notifications()
}

#[allow(dead_code)]
#[derive(Debug)]
struct Window {
    title: String,
    role: String,
    frame: CGRect,
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

async fn get_windows_with_ax(opt: &Opt, serial: bool, print: bool) {
    let (sender, mut receiver) = mpsc::unbounded_channel();
    for app in unsafe { NSWorkspace::sharedWorkspace().runningApplications() } {
        let bundle_id = unsafe { app.bundleIdentifier() };
        let Some(bundle_id) = bundle_id else { continue };
        let bundle_str = bundle_id.to_string();
        if let Some(filter) = &opt.bundle {
            if &bundle_str != filter {
                continue;
            }
        }
        let sender = sender.clone();
        let task = move || {
            let windows = get_windows_for_app(&bundle_str);
            sender.send((bundle_str, windows)).unwrap()
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

fn get_windows_for_app(bundle_id: &str) -> Result<Vec<Window>, accessibility::Error> {
    // TODO: Can't access processIdentifier for some reason.
    let app = AXUIElement::application_with_bundle(&bundle_id).unwrap();
    let Ok(windows) = &app.windows() else {
        return Err(accessibility::Error::NotFound);
    };
    windows
        .into_iter()
        .map(|win| {
            Ok(Window {
                title: win.title()?.to_string(),
                role: win.role()?.to_string(),
                frame: win.frame()?,
            })
        })
        .collect()
}

async fn time<O, F: Future<Output = O>>(desc: &str, f: impl FnOnce() -> F) -> O {
    let start = Instant::now();
    let out = f().await;
    let end = Instant::now();
    println!("{desc} took {:?}", end - start);
    out
}

fn watch_for_notifications() {
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

        impl DeclaredClass for NotificationHandler {}

        unsafe impl NotificationHandler {
            #[method_id(init)]
            fn init(this: Allocated<Self>) -> Option<Id<Self>> {
                let this = this.set_ivars(());
                unsafe { msg_send_id![super(this), init] }
            }

            #[method(handle:)]
            fn handle(&self, notification: &NSNotification) {
                println!("Got notification: {notification:?}")
            }
        }
    }

    impl NotificationHandler {
        fn new() -> Id<Self> {
            unsafe { msg_send_id![Self::alloc(), init] }
        }
    }

    let handler = NotificationHandler::new();

    let workspace = unsafe { NSWorkspace::sharedWorkspace() };
    let workspace_center = unsafe { workspace.notificationCenter() };

    let workspace_notifs = unsafe {
        use AppKit::*;
        [
            NSWorkspaceDidActivateApplicationNotification,
            // NSWorkspaceDidDeactivateApplicationNotification,
            NSWorkspaceDidLaunchApplicationNotification,
            NSWorkspaceDidTerminateApplicationNotification,
        ]
    };
    for notif_name in workspace_notifs {
        unsafe {
            workspace_center.addObserver_selector_name_object(
                &handler,
                sel!(handle:),
                Some(notif_name),
                Some(&workspace),
            );
        }
    }

    let default_center = unsafe { NSNotificationCenter::defaultCenter() };
    let shared_app = NSApplication::sharedApplication(MainThreadMarker::new().unwrap());

    let shared_app_notifs = unsafe {
        use AppKit::*;
        [NSApplicationDidChangeScreenParametersNotification]
    };
    for notif_name in shared_app_notifs {
        unsafe {
            default_center.addObserver_selector_name_object(
                &handler,
                sel!(handle:),
                Some(notif_name),
                Some(&shared_app),
            );
            CFRunLoopRun();
        }
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
