use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::Debug,
    ops::Deref,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use accessibility_sys::{
    kAXApplicationRole, kAXErrorActionUnsupported, kAXErrorAttributeUnsupported,
    kAXErrorCannotComplete, kAXErrorIllegalArgument, kAXErrorNoValue, kAXStandardWindowSubrole,
    kAXWindowRole,
};
use core_foundation::{boolean::CFBoolean, string::CFString};
use core_graphics::display::{CGPoint, CGRect, CGSize};
use icrate::AppKit::NSApplicationActivationOptions;

use super::{pid_t, AXUIElement, Error, Result, WindowServerId};

macro_rules! forward {
    ($(pub fn $name:ident(&self) -> $ret:ty;)*) => { $(
        pub fn $name(&self) -> $ret {
            let this = self.elem.borrow();
            this.connection().check()?;
            this.$name()
        }
    )* };
    ($(pub fn $name:ident(&mut self, $arg:ident: $argt:ty) -> $ret:ty;)*) => { $(
        pub fn $name(&self, $arg: $argt) -> $ret {
            let mut this = self.elem.borrow_mut();
            this.connection().check()?;
            this.$name($arg)
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
            elem: Application::new().into(),
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

#[derive(Debug)]
pub struct Application {
    #[allow(dead_code)]
    main_window: Option<FakeAXUIElement>,
    windows: Vec<Ptr<Window>>,
    frontmost_id: Option<WindowServerId>,
    connection: Connection,
}

impl Application {
    pub fn new() -> Ptr<Self> {
        Ptr::new(Application {
            main_window: None,
            windows: Vec::new(),
            frontmost_id: None,
            connection: Connection::new(),
        })
    }
}

impl Ptr<Application> {
    pub fn mk_window(&self) -> Ptr<Window> {
        let mut this = self.borrow_mut();
        let win = Ptr::new(Window {
            parent: Arc::downgrade(&self.0),
            frame: CGRect::default(),
            id: WindowServerId::new(1),
            connection: this.connection.clone(),
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
    connection: Connection,
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
    // Required methods.
    fn connection(&self) -> &Connection;
    fn role(&self) -> &'static str;

    // Provided stubs.
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
    fn connection(&self) -> &Connection {
        &self.connection
    }

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
    fn connection(&self) -> &Connection {
        &self.connection
    }

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

/// Simulates a connection to an app.
#[derive(Debug, Clone)]
struct Connection {
    connected: Arc<AtomicBool>,
}

impl Connection {
    fn new() -> Self {
        Connection {
            connected: Arc::new(AtomicBool::new(true)),
        }
    }

    fn is_connected(&self) -> bool {
        self.connected.load(Ordering::Relaxed)
    }

    fn check(&self) -> Result<()> {
        if self.is_connected() {
            Ok(())
        } else {
            Err(Error::Ax(kAXErrorCannotComplete))
        }
    }
}

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
    pub fn add_notification(&self, _elem: &AXUIElement, _notification: &'static str) -> Result<()> {
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
