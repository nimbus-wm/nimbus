//! Shims for interacting with the system.

#![cfg_attr(test, allow(unused_imports))]

mod attribute;
#[cfg(test)]
pub mod fake;

use attribute::AXUIElementAttributes as _;

use std::fmt::Debug;

use accessibility::AXUIElement as AXUIElementImpl;
use accessibility_sys::{kAXRaiseAction, kAXStandardWindowSubrole, kAXWindowRole};
use core_foundation::string::CFString;
use icrate::Foundation::CGRect;
use serde::{Deserialize, Serialize};

use crate::sys::{
    geometry::{CGRectDef, ToICrate},
    window_server::WindowServerId,
};

pub mod prelude {
    pub use super::attribute::AXUIElementAttributes as _;
    pub use super::attribute::Iterable;
    pub use super::AXUIElementActions as _;
    pub use crate::sys::app::NSRunningApplicationExt as _;
}

#[allow(non_camel_case_types)]
pub type pid_t = accessibility_sys::pid_t;
pub type Error = accessibility::Error;
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(not(test))]
pub type Id<T> = icrate::objc2::rc::Id<T>;
#[cfg(test)]
pub type Id<T> = std::sync::Arc<T>;

#[cfg(not(test))]
pub type NSRunningApplication = icrate::AppKit::NSRunningApplication;
#[cfg(test)]
pub type NSRunningApplication = fake::FakeNSRunningApplication;

#[cfg(not(test))]
pub type Observer = crate::sys::observer::Observer;
#[cfg(test)]
pub type Observer = fake::FakeObserver;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq)]
pub struct AXUIElement(AXUIElementInner);

#[cfg(test)]
type AXUIElementInner = fake::FakeAXUIElement;
#[cfg(not(test))]
type AXUIElementInner = AXUIElementImpl;

#[cfg(test)]
pub type AXAttribute<T> = attribute::FakeAXAttribute<T>;
#[cfg(not(test))]
pub type AXAttribute<T> = accessibility::AXAttribute<T>;

#[cfg(test)]
pub trait TCFType {}
#[cfg(test)]
impl<T> TCFType for T {}
#[cfg(not(test))]
use core_foundation::base::TCFType;

// impl Deref for AXUIElement {
//     type Target = AXUIElementInner;
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

impl Into<AXUIElement> for AXUIElementInner {
    fn into(self) -> AXUIElement {
        AXUIElement(self)
    }
}

impl AXUIElement {
    #[cfg(not(test))]
    pub fn application(pid: pid_t) -> Self {
        Self(AXUIElementInner::application(pid))
    }

    pub fn inner(&self) -> &AXUIElementInner {
        &self.0
    }

    pub fn attribute<T: TCFType + 'static>(&self, attribute: &AXAttribute<T>) -> Result<T> {
        self.0.attribute(attribute)
    }

    pub fn set_attribute<T: TCFType + 'static>(
        &self,
        attribute: &AXAttribute<T>,
        value: impl Into<T>,
    ) -> Result<()> {
        self.0.set_attribute(attribute, value)
    }

    pub fn perform_action(&self, name: &CFString) -> Result<()> {
        self.0.perform_action(name)
    }

    pub fn set_messaging_timeout(&self, timeout: f32) -> Result<()> {
        self.0.set_messaging_timeout(timeout)
    }
}

pub trait AXUIElementActions {
    fn raise(&self) -> Result<()>;
}

impl AXUIElementActions for AXUIElement {
    fn raise(&self) -> Result<()> {
        self.perform_action(&CFString::from_static_string(kAXRaiseAction))
    }
}

impl TryFrom<&AXUIElement> for WindowServerId {
    type Error = Error;
    fn try_from(element: &AXUIElement) -> Result<Self> {
        #[cfg(not(test))]
        {
            Self::try_from(&element.0)
        }
        #[cfg(test)]
        {
            element.0.window_id()
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
    fn try_from(element: &AXUIElement) -> Result<Self> {
        Ok(WindowInfo {
            is_standard: element.role()? == kAXWindowRole
                && element.subrole()? == kAXStandardWindowSubrole,
            title: element.title()?.to_string(),
            frame: element.frame()?.to_icrate(),
            sys_id: WindowServerId::try_from(element).ok(),
        })
    }
}
