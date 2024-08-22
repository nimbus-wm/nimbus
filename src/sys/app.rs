pub use accessibility_sys::pid_t;
use icrate::{
    objc2::{class, msg_send, msg_send_id, rc::Id},
    AppKit::{NSRunningApplication, NSWorkspace},
    Foundation::NSString,
};
use serde::{Deserialize, Serialize};

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

pub trait NSRunningApplicationExt {
    fn with_process_id(pid: pid_t) -> Option<Id<Self>>;
    fn pid(&self) -> pid_t;
    fn bundle_id(&self) -> Option<Id<NSString>>;
    fn localized_name(&self) -> Option<Id<NSString>>;
}

impl NSRunningApplicationExt for NSRunningApplication {
    fn with_process_id(pid: pid_t) -> Option<Id<Self>> {
        unsafe {
            // For some reason this binding isn't generated in icrate.
            msg_send_id![class!(NSRunningApplication), runningApplicationWithProcessIdentifier:pid]
        }
    }
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

#[derive(Serialize, Deserialize, Debug)]
#[allow(dead_code)]
pub struct AppInfo {
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
