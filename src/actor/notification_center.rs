//! This actor manages the global notification queue, which tells us when an
//! application is launched or focused or the screen state changes.

use std::{cell::RefCell, mem};

use accessibility_sys::pid_t;
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
use tracing::{info_span, trace, warn};

use crate::{
    actor::app::AppInfo, actor::reactor::Event, sys::app::NSRunningApplicationExt,
    sys::screen::ScreenCache,
};

#[repr(C)]
struct Instance {
    screen_cache: RefCell<ScreenCache>,
    app_launch_cb: RefCell<Box<dyn FnMut(pid_t, AppInfo)>>,
    event_cb: RefCell<Box<dyn FnMut(Event)>>,
}

unsafe impl Encode for Instance {
    const ENCODING: Encoding = Encoding::Object;
}

declare_class! {
    struct NotificationHandlerInner;

    // SAFETY:
    // - The superclass NSObject does not have any subclassing requirements.
    // - Interior mutability is a safe default.
    // - `NotificationHandler` does not implement `Drop`.
    unsafe impl ClassType for NotificationHandlerInner {
        type Super = NSObject;
        type Mutability = mutability::InteriorMutable;
        const NAME: &'static str = "NotificationHandler";
    }

    impl DeclaredClass for NotificationHandlerInner {
        type Ivars = Box<Instance>;
    }

    // SAFETY: Each of these method signatures must match their invocations.
    unsafe impl NotificationHandlerInner {
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

impl NotificationHandlerInner {
    fn new(
        app_launch_cb: Box<dyn FnMut(pid_t, AppInfo)>,
        event_cb: Box<dyn FnMut(Event)>,
    ) -> Id<Self> {
        let instance = Instance {
            screen_cache: RefCell::new(ScreenCache::new(MainThreadMarker::new().unwrap())),
            app_launch_cb: RefCell::new(app_launch_cb),
            event_cb: RefCell::new(event_cb),
        };
        unsafe { msg_send_id![Self::alloc(), initWith: instance] }
    }

    fn handle_screen_changed_event(&self, notif: &NSNotification) {
        use AppKit::*;
        let name = unsafe { &*notif.name() };
        let span = info_span!("notification_center::handle_screen_changed_event", ?name);
        let _s = span.enter();
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
        let span = info_span!("notification_center::handle_app_event", ?name);
        let _guard = span.enter();
        if unsafe { NSWorkspaceDidLaunchApplicationNotification } == name {
            (self.ivars().app_launch_cb.borrow_mut())(pid, AppInfo::from(&*app));
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
        (self.ivars().event_cb.borrow_mut())(event);
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

pub struct NotificationHandler {
    #[allow(dead_code)]
    inner: Id<NotificationHandlerInner>,
}

impl NotificationHandler {
    pub fn new(
        app_launch_cb: Box<dyn FnMut(pid_t, AppInfo)>,
        event_cb: Box<dyn FnMut(Event)>,
    ) -> Self {
        let handler = NotificationHandlerInner::new(app_launch_cb, event_cb);

        // SAFETY: Selector must have signature fn(&self, &NSNotification)
        let register_unsafe = |selector, notif_name, center: &Id<NSNotificationCenter>, object| unsafe {
            center.addObserver_selector_name_object(
                &handler,
                selector,
                Some(notif_name),
                Some(object),
            );
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

        if let Some(app) = unsafe { workspace.frontmostApplication() } {
            handler.send_event(Event::ApplicationGloballyActivated(app.pid()));
        }

        NotificationHandler { inner: handler }
    }

    pub fn watch_for_notifications(&self) {
        CFRunLoop::run_current();
    }
}
