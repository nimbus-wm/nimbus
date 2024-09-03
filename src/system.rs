//! Shims for interacting with the system.

#![cfg_attr(test, allow(unused_imports))]

#[cfg(test)]
pub mod fake;

use std::{fmt::Debug, mem::transmute, ops::Deref};

use accessibility::{AXUIElement as AXUIElementImpl, AXUIElementAttributes as _};
use accessibility_sys::{kAXStandardWindowSubrole, kAXWindowRole};
use core_foundation::{array::CFArray, base::ItemRef};
use icrate::Foundation::CGRect;
use serde::{Deserialize, Serialize};

use crate::sys::{
    geometry::{CGRectDef, ToICrate},
    window_server::WindowServerId,
};

pub mod prelude {
    pub use accessibility::{AXUIElementActions as _, AXUIElementAttributes as _};

    pub use super::iter::Iterable;
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

impl Deref for AXUIElement {
    type Target = AXUIElementInner;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Into<AXUIElement> for AXUIElementInner {
    fn into(self) -> AXUIElement {
        AXUIElement(self)
    }
}

impl AXUIElement {
    #[allow(dead_code)]
    pub fn application(pid: pid_t) -> Self {
        Self(AXUIElementInner::application(pid))
    }

    pub fn windows(&self) -> Result<impl iter::Iterable<Item = Self>> {
        self.0.windows()
    }

    pub fn main_window(&self) -> Result<Self> {
        self.0.main_window().map(Self)
    }

    pub fn parent(&self) -> Result<Self> {
        self.0.parent().map(Self)
    }
}

mod iter {
    use super::*;

    pub trait Iterable: Debug {
        type Item;
        fn iter(&self) -> impl Iterator<Item = impl Deref<Target = Self::Item>>;
        fn len(&self) -> usize;
    }

    impl Iterable for Vec<AXUIElementInner> {
        type Item = AXUIElement;
        #[allow(refining_impl_trait)]
        fn iter(&self) -> impl Iterator<Item = &'_ AXUIElement> {
            fn map_ref(inner: &AXUIElementInner) -> &AXUIElement {
                // SAFETY: repr(transparent)
                unsafe { transmute(inner) }
            }
            let slice: &[AXUIElementInner] = &*self;
            slice.iter().map(map_ref)
        }
        fn len(&self) -> usize {
            Vec::len(self)
        }
    }

    #[cfg(not(test))]
    impl Iterable for CFArray<AXUIElementInner> {
        type Item = AXUIElement;
        fn iter(&self) -> impl Iterator<Item = impl Deref<Target = AXUIElement>> {
            self.iter().map(|elem| {
                fn convert(elem: ItemRef<AXUIElementInner>) -> ItemRef<AXUIElement> {
                    // SAFETY: repr(transparent)
                    unsafe { transmute(elem) }
                }
                convert(elem)
            })
        }

        fn len(&self) -> usize {
            CFArray::len(self) as usize
        }
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
