use accessibility::{AXUIElement, AXUIElementAttributes};
pub use accessibility_sys::pid_t;
use accessibility_sys::{kAXStandardWindowSubrole, kAXWindowRole};
use objc2::rc::Retained;
use objc2::{class, msg_send};
use objc2_app_kit::{NSRunningApplication, NSWorkspace};
use objc2_core_foundation::CGRect;
use objc2_foundation::NSString;
use serde::{Deserialize, Serialize};

use super::geometry::{CGRectDef, ToICrate};
use super::window_server::WindowServerId;

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
    fn with_process_id(pid: pid_t) -> Option<Retained<Self>>;
    fn pid(&self) -> pid_t;
    fn bundle_id(&self) -> Option<Retained<NSString>>;
    fn localized_name(&self) -> Option<Retained<NSString>>;
}

impl NSRunningApplicationExt for NSRunningApplication {
    fn with_process_id(pid: pid_t) -> Option<Retained<Self>> {
        unsafe {
            // For some reason this binding isn't generated in icrate.
            msg_send![class!(NSRunningApplication), runningApplicationWithProcessIdentifier:pid]
        }
    }
    fn pid(&self) -> pid_t {
        unsafe { msg_send![self, processIdentifier] }
    }
    fn bundle_id(&self) -> Option<Retained<NSString>> {
        unsafe { self.bundleIdentifier() }
    }
    fn localized_name(&self) -> Option<Retained<NSString>> {
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
            title: element.title().map(|t| t.to_string()).unwrap_or_default(),
            frame: element.frame()?.to_icrate(),
            sys_id: WindowServerId::try_from(element).ok(),
        })
    }
}

pub struct ProcessInfo {
    pub is_xpc: bool,
}

impl ProcessInfo {
    pub fn for_pid(pid: pid_t) -> Result<Self, ()> {
        let psn = ProcessSerialNumber::for_pid(pid)?;

        let mut info = ProcessInfoRec::default();
        info.processInfoLength = size_of::<ProcessInfoRec>() as _;
        if unsafe { GetProcessInformation(&psn, &mut info) } != 0 {
            return Err(());
        }

        Ok(Self {
            is_xpc: info.processType.to_be_bytes() == *b"XPC!",
        })
    }
}

type FourCharCode = u32;
type OSType = FourCharCode;

#[allow(dead_code)]
#[allow(non_snake_case)]
#[repr(C, packed(2))]
#[derive(Default)]
struct ProcessInfoRec {
    processInfoLength: u32,
    processName: *const u8,
    processNumber: ProcessSerialNumber,
    processType: u32,
    processSignature: OSType,
    processMode: u32,
    processLocation: *const u8,
    processSize: u32,
    processFreeMem: u32,
    processLauncher: ProcessSerialNumber,
    processLaunchDate: u32,
    processActiveTime: u32,
    processAppRef: *const u8,
}
const _: () = if size_of::<ProcessInfoRec>() != 72 {
    panic!("unexpected size")
};

#[repr(C)]
#[derive(Default)]
pub(super) struct ProcessSerialNumber {
    high: u32,
    low: u32,
}

impl ProcessSerialNumber {
    pub(super) fn for_pid(pid: pid_t) -> Result<Self, ()> {
        let mut psn = ProcessSerialNumber::default();
        if unsafe { GetProcessForPID(pid, &mut psn) } == 0 {
            Ok(psn)
        } else {
            Err(())
        }
    }
}

type OSErr = i16;
type OSStatus = i32;

#[link(name = "ApplicationServices", kind = "framework")]
unsafe extern "C" {
    // Deprecated in macOS 10.9.
    fn GetProcessForPID(pid: pid_t, psn: *mut ProcessSerialNumber) -> OSStatus;

    // Deprecated in macOS 10.9.
    fn GetProcessInformation(psn: *const ProcessSerialNumber, info: *mut ProcessInfoRec) -> OSErr;
}
