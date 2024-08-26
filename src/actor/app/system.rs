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
        sync::Arc,
    };

    use accessibility_sys::{
        kAXApplicationRole, kAXErrorCannotComplete, kAXErrorIllegalArgument, kAXErrorNoValue,
        kAXWindowRole,
    };
    use core_foundation::{boolean::CFBoolean, string::CFString};
    use core_graphics::display::{CGPoint, CGRect, CGSize};
    use icrate::AppKit::NSApplicationActivationOptions;

    use super::*;

    #[derive(Debug, PartialEq)]
    pub struct Ptr<T>(Arc<RefCell<T>>);
    type Weak<T> = std::sync::Weak<RefCell<T>>;

    impl<T> Ptr<T> {
        fn new(inner: T) -> Self {
            Self(Arc::new(RefCell::new(inner)))
        }
        fn borrow(&self) -> Ref<T> {
            self.0.borrow()
        }
        fn borrow_mut(&self) -> RefMut<T> {
            self.0.borrow_mut()
        }
    }

    impl<T> Clone for Ptr<T> {
        fn clone(&self) -> Self {
            Self(Arc::clone(&self.0))
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    enum ElementKind {
        Application(Ptr<Application>),
        Window(Ptr<Window>),
    }

    macro_rules! element_kind {
        ($ty:ident) => {
            // These are always wrapped in an Arc, so comparing pointers is useful.
            impl PartialEq for $ty {
                fn eq(&self, other: &Self) -> bool {
                    self as *const $ty == other as *const $ty
                }
            }

            impl From<Ptr<$ty>> for ElementKind {
                fn from(inner: Ptr<$ty>) -> ElementKind {
                    ElementKind::$ty(inner)
                }
            }

            impl From<Ptr<$ty>> for FakeAXUIElement {
                fn from(inner: Ptr<$ty>) -> FakeAXUIElement {
                    FakeAXUIElement { kind: ElementKind::from(inner) }
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

    #[derive(Debug, Default)]
    pub struct Application {
        #[allow(dead_code)]
        main_window: Option<FakeAXUIElement>,
        windows: Vec<FakeAXUIElement>,
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

    impl Window {
        fn parent(&self) -> Result<FakeAXUIElement> {
            self.parent
                .upgrade()
                .map(|kind| FakeAXUIElement { kind: Ptr(kind).into() })
                .ok_or(Error::Ax(kAXErrorCannotComplete))
        }
    }

    impl Ptr<Window> {
        pub fn id(&self) -> u32 {
            self.borrow().id.as_u32()
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct FakeAXUIElement {
        kind: ElementKind,
    }

    impl FakeAXUIElement {
        pub fn application(_pid: pid_t) -> Self {
            Self {
                kind: ElementKind::Application(Ptr::new(Application {
                    main_window: None,
                    windows: Vec::new(),
                    frontmost_id: None,
                })),
            }
        }

        fn as_window(&self) -> Result<Ptr<Window>> {
            match &self.kind {
                ElementKind::Window(inner) => Ok(Ptr::clone(inner)),
                _ => Err(Error::Ax(kAXErrorNoValue)),
            }
        }

        fn as_application(&self) -> Result<Ptr<Application>> {
            match &self.kind {
                ElementKind::Application(inner) => Ok(Ptr::clone(inner)),
                _ => Err(Error::Ax(kAXErrorNoValue)),
            }
        }

        pub fn role(&self) -> Result<CFString> {
            let role = match &self.kind {
                ElementKind::Application { .. } => kAXApplicationRole,
                ElementKind::Window { .. } => kAXWindowRole,
            };
            Ok(CFString::from_static_string(role))
        }

        pub fn subrole(&self) -> Result<CFString> {
            let _ = self.as_window()?;
            Ok(CFString::from_static_string(kAXStandardWindowSubrole))
        }

        pub fn title(&self) -> Result<CFString> {
            let _ = self.as_window()?;
            Ok(CFString::from_static_string(""))
        }

        pub fn frontmost(&self) -> Result<CFBoolean> {
            let window = self.as_window()?;
            let parent = self.parent()?;
            let parent = parent.as_application().expect("Wrong parent type");
            let same = parent.borrow().frontmost_id == Some(window.borrow().id);
            Ok(same.into())
        }

        pub fn raise(&self) -> Result<()> {
            let window = self.as_window().map_err(|_| Error::Ax(kAXErrorIllegalArgument))?;
            let parent = self.parent()?;
            let parent = parent.as_application().expect("Wrong parent type");
            parent.borrow_mut().frontmost_id = Some(window.borrow().id);
            Ok(())
        }

        pub fn parent(&self) -> Result<Self> {
            self.as_window()?.borrow().parent()
        }

        pub fn windows(&self) -> Result<Vec<Self>> {
            Ok(self.as_application()?.borrow().windows.clone())
        }

        pub fn main_window(&self) -> Result<Self> {
            let app = self.as_application()?;
            let app = app.borrow();
            let Some(id) = app.frontmost_id else {
                return Err(Error::Ax(kAXErrorNoValue));
            };
            let Some(win) = app
                .windows
                .iter()
                .find(|w| w.as_window().map(|w| w.borrow().id == id).unwrap_or(false))
            else {
                return Err(Error::Ax(kAXErrorNoValue));
            };
            Ok(win.clone())
        }

        pub fn frame(&self) -> Result<CGRect> {
            Ok(self.as_window()?.borrow().frame)
        }

        pub fn set_position(&self, pos: CGPoint) -> Result<()> {
            self.as_window()?.borrow_mut().frame.origin = pos;
            Ok(())
        }

        pub fn set_size(&self, size: CGSize) -> Result<()> {
            self.as_window()?.borrow_mut().frame.size = size;
            Ok(())
        }

        pub fn window_id(&self) -> Result<WindowServerId> {
            Ok(self.as_window()?.borrow().id)
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
