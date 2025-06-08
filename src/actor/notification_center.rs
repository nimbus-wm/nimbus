//! This actor manages the global notification queue, which tells us when an
//! application is launched or focused or the screen state changes.

use std::cell::RefCell;
use std::{future, mem};

use objc2::rc::{Allocated, Retained};
use objc2::{define_class, msg_send, sel, AnyThread, ClassType, DeclaredClass, Encode, Encoding};
use objc2_app_kit::{
    self, NSApplication, NSRunningApplication, NSWorkspace, NSWorkspaceApplicationKey,
};
use objc2_foundation::{MainThreadMarker, NSNotification, NSNotificationCenter, NSObject};
use tracing::{info_span, trace, warn, Span};

use super::wm_controller::{self, WmEvent};
use crate::actor::app::AppInfo;
use crate::sys::app::NSRunningApplicationExt;
use crate::sys::screen::ScreenCache;

#[repr(C)]
struct Instance {
    screen_cache: RefCell<ScreenCache>,
    events_tx: wm_controller::Sender,
}

unsafe impl Encode for Instance {
    const ENCODING: Encoding = Encoding::Object;
}

define_class! {
    // SAFETY:
    // - The superclass NSObject does not have any subclassing requirements.
    // - `NotificationHandler` does not implement `Drop`.
    #[unsafe(super(NSObject))]
    #[ivars = Box<Instance>]
    struct NotificationCenterInner;

    // SAFETY: Each of these method signatures must match their invocations.
    impl NotificationCenterInner {
        #[unsafe(method_id(initWith:))]
        fn init(this: Allocated<Self>, instance: Instance) -> Option<Retained<Self>> {
            let this = this.set_ivars(Box::new(instance));
            unsafe { msg_send![super(this), init] }
        }

        #[unsafe(method(recvScreenChangedEvent:))]
        fn recv_screen_changed_event(&self, notif: &NSNotification) {
            trace!("{notif:#?}");
            self.handle_screen_changed_event(notif);
        }

        #[unsafe(method(recvAppEvent:))]
        fn recv_app_event(&self, notif: &NSNotification) {
            trace!("{notif:#?}");
            self.handle_app_event(notif);
        }
    }
}

impl NotificationCenterInner {
    fn new(events_tx: wm_controller::Sender) -> Retained<Self> {
        let instance = Instance {
            screen_cache: RefCell::new(ScreenCache::new(MainThreadMarker::new().unwrap())),
            events_tx,
        };
        unsafe { msg_send![Self::alloc(), initWith: instance] }
    }

    fn handle_screen_changed_event(&self, notif: &NSNotification) {
        use objc2_app_kit::*;
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
        let (frames, ids, converter) = screen_cache.update_screen_config();
        let spaces = screen_cache.get_screen_spaces();
        self.send_event(WmEvent::ScreenParametersChanged(frames, ids, converter, spaces));
    }

    fn send_current_space(&self) {
        let spaces = self.ivars().screen_cache.borrow().get_screen_spaces();
        self.send_event(WmEvent::SpaceChanged(spaces));
    }

    fn handle_app_event(&self, notif: &NSNotification) {
        use objc2_app_kit::*;
        let Some(app) = self.running_application(notif) else {
            return;
        };
        let pid = app.pid();
        let name = unsafe { &*notif.name() };
        let span = info_span!("notification_center::handle_app_event", ?name);
        let _guard = span.enter();
        if unsafe { NSWorkspaceDidLaunchApplicationNotification } == name {
            self.send_event(WmEvent::AppLaunch(pid, AppInfo::from(&*app)));
        } else if unsafe { NSWorkspaceDidActivateApplicationNotification } == name {
            self.send_event(WmEvent::AppGloballyActivated(pid));
        } else if unsafe { NSWorkspaceDidDeactivateApplicationNotification } == name {
            self.send_event(WmEvent::AppGloballyDeactivated(pid));
        } else if unsafe { NSWorkspaceDidTerminateApplicationNotification } == name {
            self.send_event(WmEvent::AppTerminated(pid));
        } else if unsafe { NSWorkspaceActiveSpaceDidChangeNotification } == name {
            self.send_current_space();
        } else {
            panic!("Unexpected application event: {notif:?}");
        }
    }

    fn send_event(&self, event: WmEvent) {
        // Errors only happen during shutdown, so we can ignore them.
        _ = self.ivars().events_tx.send((Span::current().clone(), event));
    }

    fn running_application(
        &self,
        notif: &NSNotification,
    ) -> Option<Retained<NSRunningApplication>> {
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
        let app: Retained<NSRunningApplication> = unsafe { mem::transmute(app) };
        Some(app)
    }
}

pub struct NotificationCenter {
    #[allow(dead_code)]
    inner: Retained<NotificationCenterInner>,
}

impl NotificationCenter {
    pub fn new(events_tx: wm_controller::Sender) -> Self {
        let handler = NotificationCenterInner::new(events_tx);

        // SAFETY: Selector must have signature fn(&self, &NSNotification)
        let register_unsafe =
            |selector, notif_name, center: &Retained<NSNotificationCenter>, object| unsafe {
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
            use objc2_app_kit::*;
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

        NotificationCenter { inner: handler }
    }

    pub async fn watch_for_notifications(self) {
        let workspace = &unsafe { NSWorkspace::sharedWorkspace() };

        self.inner.send_screen_parameters();
        self.inner.send_event(WmEvent::AppEventsRegistered);
        if let Some(app) = unsafe { workspace.frontmostApplication() } {
            self.inner.send_event(WmEvent::AppGloballyActivated(app.pid()));
        }

        // All the work is done in callbacks dispatched by the run loop, which
        // we assume is running once this function is awaited.
        future::pending().await
    }
}
