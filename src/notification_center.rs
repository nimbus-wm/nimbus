use std::{cell::RefCell, mem, sync::mpsc::Sender};

use core_foundation::runloop::CFRunLoop;
use icrate::{
    objc2::{
        declare_class, msg_send_id, mutability,
        rc::{Allocated, Id},
        sel, ClassType, DeclaredClass, Encode, Encoding,
    },
    AppKit::{self, NSApplication, NSRunningApplication, NSWorkspace, NSWorkspaceApplicationKey},
    Foundation::{MainThreadMarker, NSNotification, NSNotificationCenter, NSObject},
};
use log::{trace, warn};

use crate::{
    app::{self, NSRunningApplicationExt},
    reactor::{AppInfo, Event},
    screen::ScreenCache,
};

pub fn watch_for_notifications(events_tx: Sender<Event>) {
    #[repr(C)]
    struct Instance {
        events_tx: &'static mut Sender<Event>,
        screen_cache: RefCell<ScreenCache>,
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

            #[method(recvScreenChangedEvent:)]
            fn recv_screen_changed_event(&self, notif: &NSNotification) {
                trace!("{notif:#?}");
                self.handle_screen_changed_event(notif);
            }

            #[method(recvAppEvent:)]
            fn recv_app_event(&self, notif: &NSNotification) {
                trace!("{notif:#?}");
                self.handle_app_event(notif);
            }
        }
    }

    impl NotificationHandler {
        fn new(events_tx: Sender<Event>) -> Id<Self> {
            let events_tx = Box::leak(Box::new(events_tx));
            let instance = Instance {
                events_tx,
                screen_cache: RefCell::new(ScreenCache::new(MainThreadMarker::new().unwrap())),
            };
            unsafe { msg_send_id![Self::alloc(), initWith: instance] }
        }

        fn handle_screen_changed_event(&self, notif: &NSNotification) {
            use AppKit::*;
            let name = unsafe { &*notif.name() };
            if unsafe { NSWorkspaceActiveSpaceDidChangeNotification } == name {
                self.send_current_space();
            } else if unsafe { NSApplicationDidChangeScreenParametersNotification } == name {
                self.send_screen_parameters();
            } else {
                panic!("Unexpected screen changed event: {notif:?}");
            }
        }

        fn send_screen_parameters(&self) {
            let mut screen_cache = self.ivars().screen_cache.borrow_mut();
            let frames = screen_cache.update_screen_config();
            let spaces = screen_cache.get_screen_spaces();
            self.send_event(Event::ScreenParametersChanged(frames, spaces));
        }

        fn send_current_space(&self) {
            let spaces = self.ivars().screen_cache.borrow().get_screen_spaces();
            self.send_event(Event::SpaceChanged(spaces));
        }

        fn handle_app_event(&self, notif: &NSNotification) {
            use AppKit::*;
            let Some(app) = self.running_application(notif) else {
                return;
            };
            let pid = app.pid();
            let name = unsafe { &*notif.name() };
            if unsafe { NSWorkspaceDidLaunchApplicationNotification } == name {
                app::spawn_app_thread(pid, AppInfo::from(&*app), self.events_tx().clone());
            } else if unsafe { NSWorkspaceDidActivateApplicationNotification } == name {
                self.send_event(Event::ApplicationGloballyActivated(pid));
            } else if unsafe { NSWorkspaceDidDeactivateApplicationNotification } == name {
                self.send_event(Event::ApplicationGloballyDeactivated(pid));
            } else if unsafe { NSWorkspaceDidTerminateApplicationNotification } == name {
                self.send_event(Event::ApplicationTerminated(pid));
            } else if unsafe { NSWorkspaceActiveSpaceDidChangeNotification } == name {
                self.send_current_space();
            } else {
                panic!("Unexpected application event: {notif:?}");
            }
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
            sel!(recvScreenChangedEvent:),
            NSApplicationDidChangeScreenParametersNotification,
            default_center,
            shared_app,
        );
        register_unsafe(
            sel!(recvScreenChangedEvent:),
            NSWorkspaceActiveSpaceDidChangeNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvAppEvent:),
            NSWorkspaceDidLaunchApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvAppEvent:),
            NSWorkspaceDidActivateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvAppEvent:),
            NSWorkspaceDidDeactivateApplicationNotification,
            workspace_center,
            workspace,
        );
        register_unsafe(
            sel!(recvAppEvent:),
            NSWorkspaceDidTerminateApplicationNotification,
            workspace_center,
            workspace,
        );
    };

    handler.send_screen_parameters();
    handler.send_current_space();
    CFRunLoop::run_current();
}
