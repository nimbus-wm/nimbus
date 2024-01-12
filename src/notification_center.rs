use std::{mem, sync::mpsc::Sender};

use core_foundation::runloop::CFRunLoop;
use icrate::{
    objc2::{
        declare_class, msg_send_id, mutability,
        rc::{Allocated, Id},
        sel, ClassType, DeclaredClass, Encode, Encoding,
    },
    AppKit::{
        self, NSApplication, NSRunningApplication, NSScreen, NSWorkspace, NSWorkspaceApplicationKey,
    },
    Foundation::{MainThreadMarker, NSNotification, NSNotificationCenter, NSObject},
};
use log::{trace, warn};

use crate::app::{self, AppInfo, NSRunningApplicationExt};
use crate::Event;

pub(crate) fn watch_for_notifications(events_tx: Sender<Event>) {
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

            #[method(recvApplication:)]
            fn recv_application(&self, notif: &NSNotification) {
                trace!("{notif:#?}");
                self.handle_application_event(notif);
            }

            #[method(recvScreenChanged:)]
            fn recv_screen_changed(&self, notif: &NSNotification) {
                trace!("{notif:#?}");
                self.handle_screen_changed_event(notif);
            }
        }
    }

    impl NotificationHandler {
        fn new(events_tx: Sender<Event>) -> Id<Self> {
            let events_tx = Box::leak(Box::new(events_tx));
            let instance = Instance { events_tx };
            unsafe { msg_send_id![Self::alloc(), initWith: instance] }
        }

        fn handle_application_event(&self, notif: &NSNotification) {
            use AppKit::*;
            let Some(app) = self.running_application(notif) else {
                return;
            };
            let pid = app.pid();
            let name = unsafe { &*notif.name() };
            if unsafe { NSWorkspaceDidLaunchApplicationNotification } == name {
                app::spawn_app_thread(pid, AppInfo::from(&*app), self.events_tx().clone());
            } else if unsafe { NSWorkspaceDidActivateApplicationNotification } == name {
                self.send_event(Event::ApplicationActivated(pid));
            } else if unsafe { NSWorkspaceDidTerminateApplicationNotification } == name {
                self.send_event(Event::ApplicationTerminated(pid));
            } else {
                unreachable!("Unexpected application event: {notif:?}");
            }
        }

        fn handle_screen_changed_event(&self, _notif: &NSNotification) {
            self.send_screen_parameters();
        }

        fn send_screen_parameters(&self) {
            let frame = NSScreen::mainScreen(MainThreadMarker::new().unwrap())
                .map(|screen| screen.visibleFrame());
            self.send_event(Event::ScreenParametersChanged(frame));
        }

        fn send_event(&self, event: Event) {
            if let Err(err) = self.events_tx().send(event) {
                warn!("Failed to send event: {err:?}");
            }
        }

        fn events_tx(&self) -> &Sender<Event> {
            self.ivars().events_tx
        }

        fn running_application(&self, notif: &NSNotification) -> Option<Id<NSRunningApplication>> {
            let info = unsafe { notif.userInfo() };
            let Some(info) = info else {
                warn!("Got app notification without user info: {notif:?}");
                return None;
            };
            let app = unsafe { info.valueForKey(NSWorkspaceApplicationKey) };
            let Some(app) = app else {
                warn!("Got app notification without app object: {notif:?}");
                return None;
            };
            assert!(app.class() == NSRunningApplication::class());
            let app: Id<NSRunningApplication> = unsafe { mem::transmute(app) };
            Some(app)
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
            sel!(recvApplication:),
            NSWorkspaceDidLaunchApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvApplication:),
            NSWorkspaceDidActivateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvApplication:),
            NSWorkspaceDidTerminateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvScreenChanged:),
            NSApplicationDidChangeScreenParametersNotification,
            default_center,
            shared_app,
        );
    };

    handler.send_screen_parameters();
    CFRunLoop::run_current();
}
