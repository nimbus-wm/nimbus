use accessibility::{AXUIElement, AXUIElementAttributes};
pub use accessibility_sys::pid_t;
use accessibility_sys::{kAXStandardWindowSubrole, kAXWindowRole};
use icrate::{
    objc2::{msg_send, rc::Id},
    AppKit::{NSRunningApplication, NSWorkspace},
    Foundation::{CGRect, NSString},
};
use serde::{Deserialize, Serialize};

use super::{
    geometry::{CGRectDef, ToICrate},
    window_server::WindowServerId,
};

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

#[derive(Serialize, Deserialize, Debug)]
pub struct WindowInfo {
    pub is_standard: bool,
    pub title: String,
    #[serde(with = "CGRectDef")]
    pub frame: CGRect,
    pub sys_id: Option<WindowServerId>,
}

impl TryFrom<&AXUIElement> for WindowInfo {
    type Error = accessibility::Error;
    fn try_from(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        Ok(WindowInfo {
            is_standard: element.role()? == kAXWindowRole
                && element.subrole()? == kAXStandardWindowSubrole,
            title: element.title()?.to_string(),
            frame: element.frame()?.to_icrate(),
            sys_id: WindowServerId::try_from(element).ok(),
        })
    }
}
