use std::{
    any::Any,
    borrow,
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::Deref,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use accessibility::value::AXValue;
use accessibility_sys::{
    kAXApplicationRole, kAXErrorActionUnsupported, kAXErrorAttributeUnsupported,
    kAXErrorCannotComplete, kAXErrorIllegalArgument, kAXErrorNoValue, kAXFrameAttribute,
    kAXFrontmostAttribute, kAXMainWindowAttribute, kAXParentAttribute, kAXPositionAttribute,
    kAXRaiseAction, kAXRoleAttribute, kAXSizeAttribute, kAXStandardWindowSubrole,
    kAXSubroleAttribute, kAXTitleAttribute, kAXWindowCreatedNotification, kAXWindowRole,
    kAXWindowsAttribute,
};
use core_foundation::{base::TCFType, boolean::CFBoolean, string::CFString};
use core_graphics::display::{CGPoint, CGRect, CGSize};
use icrate::AppKit::NSApplicationActivationOptions;

use super::{pid_t, AXAttribute, AXUIElement, Error, Result, WindowServerId};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FakeAXUIElement {
    elem: Ptr<dyn Element>,
}

// trait Any: std::any::Any + 'static {
//     fn type_name(&self) -> &'static str {
//         std::any::type_name::<Self>()
//     }
// }
// impl<T: std::any::Any> Any for T {}

// impl Box<dyn Any> {
//     fn downcast<T>(self) -> std::result::Result<Box<T>, Box<dyn Any>> {
//         if self.is::<T>() {
//             <Self
//         }
//     }
// }

impl FakeAXUIElement {
    pub fn attribute<T: 'static>(&self, attribute: &AXAttribute<T>) -> Result<T> {
        let this = self.elem.borrow();
        this.connection().check()?;
        let val = this.attribute(attribute.as_str())?;
        Ok(*val
            .downcast()
            .unwrap_or_else(|_| panic!("wrong type: expected {}", std::any::type_name::<T>(),)))
    }

    pub fn set_attribute<T: 'static>(
        &self,
        attribute: &AXAttribute<T>,
        value: impl Into<T>,
    ) -> Result<()> {
        let mut this = self.elem.borrow_mut();
        this.connection().check()?;
        this.set_attribute(attribute.as_str(), Box::new(value.into()))
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
    pub fn new(observer: FakeObserver) -> Ptr<Self> {
        Ptr::new(Application {
            main_window: None,
            windows: Vec::new(),
            frontmost_id: None,
            connection: Connection::new(observer),
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
        this.windows.push(win.clone());
        this.connection
            .observer
            .notify(self, kAXWindowCreatedNotification, &win.clone().into());
        win
    }

    pub fn terminate_abruptly(&self) {
        self.borrow().connection.terminate()
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

    fn attribute(&self, attribute: &'static str) -> Result<Box<dyn Any>>;
    fn set_attribute(&mut self, attribute: &'static str, _value: Box<dyn Any>) -> Result<()>;
    fn perform_action(&mut self, action: &'static str) -> Result<()>;

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

    fn attribute(&self, attribute: &'static str) -> Result<Box<dyn Any>> {
        #[allow(non_upper_case_globals)]
        Ok(match attribute {
            kAXRoleAttribute => Box::new(CFString::from_static_string(self.role())),
            kAXMainWindowAttribute => Box::new(self.main_window()?),
            kAXWindowsAttribute => Box::new(self.windows()?),
            _ => todo!(),
        })
    }

    fn set_attribute(&mut self, _attribute: &'static str, _value: Box<dyn Any>) -> Result<()> {
        todo!()
    }

    fn perform_action(&mut self, _action: &'static str) -> Result<()> {
        todo!()
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

    fn attribute(&self, attribute: &'static str) -> Result<Box<dyn Any>> {
        #[allow(non_upper_case_globals)]
        Ok(match attribute {
            kAXRoleAttribute => Box::new(CFString::from_static_string(self.role())),
            kAXSubroleAttribute => Box::new(self.subrole()?),
            kAXParentAttribute => Box::new(self.parent()?),
            kAXTitleAttribute => Box::new(self.title()?),
            kAXFrontmostAttribute => Box::new(self.frontmost()?),
            kAXFrameAttribute => Box::new(AXValue::new(&self.frame()?).unwrap()),
            _ => todo!(),
        })
    }

    fn set_attribute(&mut self, attribute: &'static str, value: Box<dyn Any>) -> Result<()> {
        #[allow(non_upper_case_globals)]
        match attribute {
            kAXPositionAttribute => self.set_position(*value.downcast().expect("wrong type")),
            kAXSizeAttribute => self.set_size(*value.downcast().expect("wrong type")),
            _ => todo!(),
        }
    }

    fn perform_action(&mut self, action: &'static str) -> Result<()> {
        #[allow(non_upper_case_globals)]
        match action {
            kAXRaiseAction => self.raise(),
            _ => todo!(),
        }
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
    observer: FakeObserver,
}

impl Connection {
    fn new(observer: FakeObserver) -> Self {
        Connection {
            connected: Arc::new(AtomicBool::new(true)),
            observer,
        }
    }

    fn terminate(&self) {
        self.connected.store(false, Ordering::Relaxed)
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
impl<T: ?Sized> Eq for Ptr<T> {}

impl<T: ?Sized> Hash for Ptr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(Arc::as_ptr(&self.0) as *const u8 as usize)
    }
}

/// Helper to make it easier to use elements as keys of a map.
#[repr(transparent)]
struct ElementKey;

impl<T: ?Sized> borrow::Borrow<ElementKey> for Ptr<T> {
    fn borrow(&self) -> &ElementKey {
        // SAFETY: ElementKey is a ZST and the pointer does not dangle.
        unsafe { &*(Arc::as_ptr(&self.0) as *const ElementKey) }
    }
}

impl borrow::Borrow<ElementKey> for FakeAXUIElement {
    fn borrow(&self) -> &ElementKey {
        borrow::Borrow::borrow(&self.elem)
    }
}

impl PartialEq for ElementKey {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}
impl Eq for ElementKey {}

impl Hash for ElementKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const Self as usize)
    }
}

#[derive(Debug, Clone)]
pub struct FakeObserver(Arc<RefCell<FakeObserverInner>>);

#[derive(Default, Debug)]
struct FakeObserverInner {
    subscriptions: HashMap<FakeAXUIElement, HashSet<&'static str>>,
    pending_notifs: Vec<(AXUIElement, &'static str)>,
}

impl FakeObserver {
    pub fn new() -> Self {
        Self(Arc::new(RefCell::new(FakeObserverInner::default())))
    }

    pub fn add_notification(
        &self,
        elem: &FakeAXUIElement,
        notification: &'static str,
    ) -> Result<()> {
        self.0.borrow_mut().subscriptions_for(elem.clone()).insert(notification);
        Ok(())
    }

    pub fn remove_notification(
        &self,
        elem: &FakeAXUIElement,
        notification: &'static str,
    ) -> Result<()> {
        self.0.borrow_mut().subscriptions_for(elem.clone()).remove(notification);
        Ok(())
    }

    pub fn pending_notifications(&self) -> Vec<(AXUIElement, &'static str)> {
        std::mem::take(&mut self.0.borrow_mut().pending_notifs)
    }

    fn notify(
        &self,
        elem: &impl borrow::Borrow<ElementKey>,
        notification: &'static str,
        target: &FakeAXUIElement,
    ) {
        let mut this = self.0.borrow_mut();
        if let Some(subscriptions) = this.subscriptions.get(borrow::Borrow::borrow(elem)) {
            if subscriptions.contains(&notification) {
                this.pending_notifs.push((target.clone().into(), notification));
            }
        }
    }
}

impl FakeObserverInner {
    fn subscriptions_for(&mut self, elem: FakeAXUIElement) -> &mut HashSet<&'static str> {
        self.subscriptions.entry(elem).or_default()
    }
}

// pub struct FakeWindowServer {
//     main_window: HashMap<pid_t, Option<WindowServerId>>,
//     focused_app: Option<pid_t>,
// }

pub struct FakeNSRunningApplication;
impl FakeNSRunningApplication {
    #[allow(non_snake_case)]
    pub unsafe fn activateWithOptions(&self, _options: NSApplicationActivationOptions) -> bool {
        todo!()
    }
}

mod tests {
    use super::*;
    use crate::system::prelude::*;

    #[track_caller]
    fn assert_err<T: Debug>(res: Result<T>, code: i32) {
        assert!(
            matches!(
                res,
                Err(Error::Ax(e)) if e == code
            ),
            "Expected error with code {code}, got {res:?}"
        );
    }

    #[test]
    fn mk_window() {
        let observer = FakeObserver::new();
        let app = Application::new(observer.clone());
        let app_elem: AXUIElement = app.clone().into();
        observer
            .add_notification(app_elem.inner(), kAXWindowCreatedNotification)
            .unwrap();
        assert_eq!(app_elem.windows().unwrap().len(), 0);
        assert_err(app_elem.main_window(), kAXErrorNoValue);
        assert!(observer.pending_notifications().is_empty());
        let win = app.mk_window();
        let win_elem: AXUIElement = win.clone().into();
        assert_eq!(app_elem.windows().unwrap().len(), 1);
        assert_err(app_elem.main_window(), kAXErrorNoValue);
        assert_eq!(win_elem.parent().expect("no parent"), app_elem);
        assert_eq!(
            observer.pending_notifications(),
            vec![(win_elem, kAXWindowCreatedNotification)]
        );
    }

    #[test]
    fn terminate_abruptly() {
        let observer = FakeObserver::new();
        let app = Application::new(observer.clone());
        let app_elem: AXUIElement = app.clone().into();
        let win = app.mk_window();
        let win_elem: AXUIElement = win.clone().into();
        app.terminate_abruptly();
        assert_err(app_elem.windows(), kAXErrorCannotComplete);
        assert_err(app_elem.main_window(), kAXErrorCannotComplete);
        assert_err(win_elem.parent(), kAXErrorCannotComplete);
    }
}
