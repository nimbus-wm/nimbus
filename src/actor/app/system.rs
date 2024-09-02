//! Shims for interacting with the system from the app actor.

#![cfg_attr(test, allow(unused_imports))]

use std::{mem::transmute, ops::Deref};

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
}

mod iter {
    use super::*;

    pub trait Iterable {
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

#[cfg(test)]
pub mod fake {
    use std::{
        cell::{Ref, RefCell, RefMut},
        fmt::Debug,
        sync::Arc,
    };

    use accessibility_sys::{
        kAXApplicationRole, kAXErrorActionUnsupported, kAXErrorAttributeUnsupported,
        kAXErrorCannotComplete, kAXErrorIllegalArgument, kAXErrorNoValue, kAXWindowRole,
    };
    use core_foundation::{boolean::CFBoolean, string::CFString};
    use core_graphics::display::{CGPoint, CGRect, CGSize};
    use icrate::AppKit::NSApplicationActivationOptions;

    use super::*;

    macro_rules! forward {
        ($(pub fn $name:ident(&self) -> $ret:ty;)*) => { $(
            pub fn $name(&self) -> $ret {
                self.elem.borrow().$name()
            }
        )* };
        ($(pub fn $name:ident(&mut self, $arg:ident: $argt:ty) -> $ret:ty;)*) => { $(
            pub fn $name(&self, $arg: $argt) -> $ret {
                self.elem.borrow_mut().$name($arg)
            }
        )* };
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct FakeAXUIElement {
        elem: Ptr<dyn Element>,
    }

    impl FakeAXUIElement {
        pub fn application(_pid: pid_t) -> Self {
            Self {
                elem: Ptr::new(Application {
                    main_window: None,
                    windows: Vec::new(),
                    frontmost_id: None,
                })
                .into(),
            }
        }
        pub fn role(&self) -> Result<CFString> {
            Ok(CFString::from_static_string(self.elem.borrow().role()))
        }
        forward! {
            pub fn subrole(&self) -> Result<CFString>;
            pub fn title(&self) -> Result<CFString>;
            pub fn frontmost(&self) -> Result<CFBoolean>;
            pub fn parent(&self) -> Result<FakeAXUIElement>;
            pub fn windows(&self) -> Result<Vec<FakeAXUIElement>>;
            pub fn main_window(&self) -> Result<FakeAXUIElement>;
            pub fn frame(&self) -> Result<CGRect>;
            pub fn raise(&self) -> Result<()>;
            pub fn window_id(&self) -> Result<WindowServerId>;
        }
        forward! {
            pub fn set_position(&mut self, pos: CGPoint) -> Result<()>;
            pub fn set_size(&mut self, size: CGSize) -> Result<()>;
        }
    }

    // Brilliant hack to get rust-analyzer completion working.
    impl Deref for FakeAXUIElement {
        type Target = ::accessibility::AXUIElement;
        #[track_caller]
        fn deref(&self) -> &Self::Target {
            panic!("Attempted to invoke an unfaked method on AXUIElement")
        }
    }

    #[derive(Debug, Default)]
    pub struct Application {
        #[allow(dead_code)]
        main_window: Option<FakeAXUIElement>,
        windows: Vec<Ptr<Window>>,
        frontmost_id: Option<WindowServerId>,
    }

    impl Application {
        pub fn new() -> Ptr<Self> {
            Ptr::new(Self::default())
        }
    }

    impl Ptr<Application> {
        pub fn mk_window(&self) -> Ptr<Window> {
            let mut this = self.borrow_mut();
            let win = Ptr::new(Window {
                parent: Arc::downgrade(&self.0),
                frame: CGRect::default(),
                id: WindowServerId::new(1),
            });
            this.windows.push(win.clone().into());
            win
        }
    }

    #[derive(Debug)]
    pub struct Window {
        parent: Weak<Application>,
        frame: CGRect,
        id: WindowServerId,
    }

    impl Ptr<Window> {
        pub fn id(&self) -> u32 {
            self.borrow().id.as_u32()
        }
    }

    macro_rules! provide_stubs {
        ($(fn $name:ident(&self) -> $ret:ty;)*) => {
            $(
                fn $name(&self) -> $ret {
                    Err(Error::Ax(kAXErrorAttributeUnsupported))
                }
            )*
        };
    }

    #[allow(unused_variables)]
    trait Element: Debug {
        fn role(&self) -> &'static str;
        provide_stubs! {
            fn subrole(&self) -> Result<CFString>;
            fn title(&self) -> Result<CFString>;
            fn frontmost(&self) -> Result<CFBoolean>;
            fn parent(&self) -> Result<FakeAXUIElement>;
            fn windows(&self) -> Result<Vec<FakeAXUIElement>>;
            fn main_window(&self) -> Result<FakeAXUIElement>;
            fn frame(&self) -> Result<CGRect>;
            fn window_id(&self) -> Result<WindowServerId>;
        }
        fn raise(&self) -> Result<()> {
            Err(Error::Ax(kAXErrorActionUnsupported))
        }
        fn set_position(&mut self, pos: CGPoint) -> Result<()> {
            Err(Error::Ax(kAXErrorIllegalArgument))
        }
        fn set_size(&mut self, pos: CGSize) -> Result<()> {
            Err(Error::Ax(kAXErrorIllegalArgument))
        }
    }

    impl Element for Application {
        fn role(&self) -> &'static str {
            kAXApplicationRole
        }

        fn main_window(&self) -> Result<FakeAXUIElement> {
            let Some(id) = self.frontmost_id else {
                return Err(Error::Ax(kAXErrorNoValue));
            };
            let Some(win) = self.windows.iter().find(|w| w.borrow().id == id) else {
                return Err(Error::Ax(kAXErrorNoValue));
            };
            Ok(win.clone().into())
        }

        fn windows(&self) -> Result<Vec<FakeAXUIElement>> {
            Ok(self.windows.iter().cloned().map(Into::into).collect())
        }
    }

    impl Element for Window {
        fn role(&self) -> &'static str {
            kAXWindowRole
        }

        fn subrole(&self) -> Result<CFString> {
            Ok(CFString::from_static_string(kAXStandardWindowSubrole))
        }

        fn parent(&self) -> Result<FakeAXUIElement> {
            self.parent
                .upgrade()
                .map(|kind| FakeAXUIElement { elem: Ptr(kind).into() })
                .ok_or(Error::Ax(kAXErrorCannotComplete))
        }

        fn title(&self) -> Result<CFString> {
            Ok(CFString::from_static_string(""))
        }

        fn frontmost(&self) -> Result<CFBoolean> {
            let Some(parent) = self.parent.upgrade() else {
                return Err(Error::Ax(kAXErrorCannotComplete));
            };
            let same = parent.borrow().frontmost_id == Some(self.id);
            Ok(same.into())
        }

        fn frame(&self) -> Result<CGRect> {
            Ok(self.frame)
        }

        fn set_position(&mut self, pos: CGPoint) -> Result<()> {
            self.frame.origin = pos;
            Ok(())
        }

        fn set_size(&mut self, size: CGSize) -> Result<()> {
            self.frame.size = size;
            Ok(())
        }

        fn raise(&self) -> Result<()> {
            let Some(parent) = self.parent.upgrade() else {
                return Err(Error::Ax(kAXErrorCannotComplete));
            };
            parent.borrow_mut().frontmost_id = Some(self.id);
            Ok(())
        }

        fn window_id(&self) -> Result<WindowServerId> {
            Ok(self.id)
        }
    }

    macro_rules! element_kind {
        ($ty:ident) => {
            impl From<Ptr<$ty>> for Ptr<dyn Element> {
                fn from(inner: Ptr<$ty>) -> Ptr<dyn Element> {
                    Ptr(inner.0)
                }
            }

            impl From<Ptr<$ty>> for FakeAXUIElement {
                fn from(inner: Ptr<$ty>) -> FakeAXUIElement {
                    FakeAXUIElement { elem: inner.into() }
                }
            }

            impl From<Ptr<$ty>> for AXUIElement {
                fn from(inner: Ptr<$ty>) -> AXUIElement {
                    AXUIElement(FakeAXUIElement::from(inner))
                }
            }
        };
    }
    element_kind!(Application);
    element_kind!(Window);

    #[derive(Debug)]
    pub struct Ptr<T: ?Sized>(Arc<RefCell<T>>);
    type Weak<T> = std::sync::Weak<RefCell<T>>;

    impl<T> Ptr<T> {
        fn new(inner: T) -> Self {
            Self(Arc::new(RefCell::new(inner)))
        }
    }
    impl<T: ?Sized> Ptr<T> {
        fn borrow(&self) -> Ref<T> {
            self.0.borrow()
        }
        fn borrow_mut(&self) -> RefMut<T> {
            self.0.borrow_mut()
        }
    }

    impl<T: ?Sized> Clone for Ptr<T> {
        fn clone(&self) -> Self {
            Self(Arc::clone(&self.0))
        }
    }

    impl<T: ?Sized> PartialEq for Ptr<T> {
        fn eq(&self, other: &Self) -> bool {
            Arc::ptr_eq(&self.0, &other.0)
        }
    }

    pub struct FakeObserver;
    impl FakeObserver {
        pub fn add_notification(
            &self,
            _elem: &AXUIElement,
            _notification: &'static str,
        ) -> Result<()> {
            // TODO
            Ok(())
        }
        pub fn remove_notification(
            &self,
            _elem: &AXUIElement,
            _notification: &'static str,
        ) -> Result<()> {
            // TODO
            Ok(())
        }
    }

    pub struct FakeNSRunningApplication;
    impl FakeNSRunningApplication {
        #[allow(non_snake_case)]
        pub unsafe fn activateWithOptions(&self, _options: NSApplicationActivationOptions) -> bool {
            todo!()
        }
    }
}
