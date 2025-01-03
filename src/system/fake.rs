use std::{
    any::Any,
    backtrace::Backtrace,
    borrow,
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::Deref,
    panic::Location,
    sync::Arc,
};

use accessibility::value::{AXValue, AXValueKind};
use accessibility_sys::{
    kAXApplicationRole, kAXErrorActionUnsupported, kAXErrorAttributeUnsupported,
    kAXErrorCannotComplete, kAXErrorIllegalArgument, kAXErrorNoValue, kAXFrameAttribute,
    kAXFrontmostAttribute, kAXMainWindowAttribute, kAXParentAttribute, kAXPositionAttribute,
    kAXRaiseAction, kAXRoleAttribute, kAXSizeAttribute, kAXStandardWindowSubrole,
    kAXSubroleAttribute, kAXTitleAttribute, kAXWindowCreatedNotification,
    kAXWindowMovedNotification, kAXWindowResizedNotification, kAXWindowRole, kAXWindowsAttribute,
};
use core_foundation::{base::TCFType, boolean::CFBoolean, string::CFString};
use core_graphics::display::{CGPoint, CGRect, CGSize};
use icrate::AppKit::NSApplicationActivationOptions;
use tokio::sync::mpsc::{
    self, unbounded_channel as channel, UnboundedReceiver as Receiver, UnboundedSender as Sender,
};
use tracing::{trace, Span};

use crate::sys::{event::MouseState, geometry::ToICrate};
use crate::system::sync::{
    atomic::{AtomicBool, Ordering},
    Mutex, MutexGuard,
};

use super::{
    pid_t, window_server::WindowServerInfo, AXAttribute, AXUIElement, Error, Result, WindowServerId,
};

pub type FakeWindowServer = Ptr<WindowServer>;

pub const FLUSH_NOTIFICATION: &'static str = "Flush";

#[derive(Default, Debug)]
pub struct WindowServer {
    apps: HashMap<pid_t, Weak<Application>>,
    windows: HashMap<u32, Weak<Window>>,
    last_id: u32,
}

impl WindowServer {
    pub fn new() -> Ptr<Self> {
        Ptr::new(WindowServer::default())
    }

    fn next_id(&mut self) -> u32 {
        self.last_id += 1;
        self.last_id
    }

    fn insert_window(&mut self, window: Weak<Window>) -> u32 {
        let id = self.next_id();
        assert!(self.windows.insert(id, window).is_none());
        id
    }
}

impl FakeWindowServer {
    pub fn get_window(&self, id: u32) -> Option<WindowServerInfo> {
        let win = self.lock().windows.get(&id)?.upgrade()?;
        let win = win.lock();
        Some(WindowServerInfo {
            id: win.wsid,
            pid: win.parent.upgrade()?.lock().pid,
            layer: 0,
            frame: win.frame.to_icrate(),
        })
    }

    pub fn get_windows(&self, ids: &[u32]) -> Vec<WindowServerInfo> {
        ids.iter().copied().flat_map(|id| self.get_window(id)).collect()
    }

    pub fn get_mouse_state(&self) -> MouseState {
        MouseState::Up
    }

    pub fn flush(&self, pid: pid_t, tx: Sender<pid_t>) {
        self.lock().apps[&pid].upgrade().unwrap().flush(tx)
    }
}

macro_rules! forward {
    ($(pub fn $name:ident(&self) -> $ret:ty;)*) => { $(
        pub fn $name(&self) -> $ret {
            let this = self.elem.lock();
            this.connection().check()?;
            this.$name()
        }
    )* };
    ($(pub fn $name:ident(&mut self, $arg:ident: $argt:ty) -> $ret:ty;)*) => { $(
        pub fn $name(&self, $arg: $argt) -> $ret {
            let mut this = self.elem.lock();
            this.connection().check()?;
            this.$name($arg)
        }
    )* };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FakeAXUIElement {
    elem: Ptr<dyn Element>,
}

trait AnyExt {
    fn expect_cast<T: 'static>(self) -> T;
    fn expect_value_cast<T: AXValueKind + 'static>(self) -> T;
}

impl AnyExt for Box<dyn Any> {
    #[track_caller]
    fn expect_cast<T: 'static>(self) -> T {
        *self
            .downcast()
            .unwrap_or_else(|_| panic!("wrong type; expected {}", std::any::type_name::<T>()))
    }

    #[track_caller]
    fn expect_value_cast<T: AXValueKind + 'static>(self) -> T {
        let val: AXValue<T> = self.expect_cast();
        val.value().unwrap()
    }
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
        let this = self.elem.lock();
        this.connection().check()?;
        let val = this.attribute(attribute.as_str())?;
        Ok(val.expect_cast())
    }

    pub fn set_attribute<T: 'static>(
        &self,
        attribute: &AXAttribute<T>,
        value: impl Into<T>,
    ) -> Result<()> {
        let mut this = self.elem.lock();
        this.connection().check()?;
        this.set_attribute(attribute.as_str(), Box::new(value.into()), &self)
    }

    pub fn role(&self) -> Result<CFString> {
        Ok(CFString::from_static_string(self.elem.lock().role()))
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
    pid: pid_t,
    last_id: i32,
    // TODO: Arc cycle
    #[expect(dead_code)]
    main_window: Option<FakeAXUIElement>,
    windows: Vec<Ptr<Window>>,
    frontmost_id: Option<WindowServerId>,
    connection: Connection,
}

impl Application {
    pub fn new(pid: pid_t, server: Ptr<WindowServer>, observer: FakeObserver) -> Ptr<Self> {
        let this = Ptr::new(Application {
            pid,
            last_id: 0,
            main_window: None,
            windows: Vec::new(),
            frontmost_id: None,
            connection: Connection::new(server.clone(), observer),
        });
        server.lock().apps.insert(pid, Ptr::downgrade(&this));
        this
    }
}

impl Ptr<Application> {
    pub fn mk_window(&self) -> Ptr<Window> {
        let mut this = self.lock();
        this.last_id += 1;
        let win = Ptr::new_cyclic(|weak_self| Window {
            parent: Ptr::downgrade(self),
            frame: CGRect::default(),
            wsid: WindowServerId::new(
                this.connection.server.lock().insert_window(weak_self.clone()),
            ),
            id: ElementId(this.pid, this.last_id),
            connection: this.connection.clone(),
        });
        this.windows.push(win.clone());
        this.connection
            .observer
            .notify(&*this, kAXWindowCreatedNotification, &win.clone().into());
        win
    }

    pub fn terminate_abruptly(&self) {
        self.lock().connection.terminate()
    }

    pub fn flush(&self, tx: Sender<pid_t>) {
        let this = self.lock();
        this.connection.observer.flush(tx, &self.clone().into())
    }
}

#[derive(Debug)]
pub struct Window {
    parent: Weak<Application>,
    frame: CGRect,
    wsid: WindowServerId,
    id: ElementId,
    connection: Connection,
}

impl Ptr<Window> {
    pub fn id(&self) -> u32 {
        self.lock().wsid.as_u32()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementId(pid_t, i32);

#[allow(unused_variables)]
trait Element: Debug + Send {
    // Required methods.
    fn connection(&self) -> &Connection;
    fn role(&self) -> &'static str;
    fn id(&self) -> ElementId;

    fn attribute(&self, attribute: &str) -> Result<Box<dyn Any>>;
    fn set_attribute(
        &mut self,
        attribute: &str,
        value: Box<dyn Any>,
        this: &FakeAXUIElement,
    ) -> Result<()>;
    #[expect(dead_code)]
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

    fn id(&self) -> ElementId {
        ElementId(self.pid, 0)
    }

    fn main_window(&self) -> Result<FakeAXUIElement> {
        let Some(id) = self.frontmost_id else {
            return Err(Error::Ax(kAXErrorNoValue));
        };
        let Some(win) = self.windows.iter().find(|w| w.lock().wsid == id) else {
            return Err(Error::Ax(kAXErrorNoValue));
        };
        Ok(win.clone().into())
    }

    fn windows(&self) -> Result<Vec<FakeAXUIElement>> {
        Ok(self.windows.iter().cloned().map(Into::into).collect())
    }

    fn attribute(&self, attribute: &str) -> Result<Box<dyn Any>> {
        #[allow(non_upper_case_globals)]
        Ok(match attribute {
            kAXRoleAttribute => Box::new(CFString::from_static_string(self.role())),
            kAXMainWindowAttribute => Box::new(self.main_window()?),
            kAXWindowsAttribute => Box::new(self.windows()?),
            // TODO
            kAXFrontmostAttribute => Box::new(CFBoolean::false_value()),
            other => todo!("{other}"),
        })
    }

    fn set_attribute(
        &mut self,
        _attribute: &str,
        _value: Box<dyn Any>,
        _this: &FakeAXUIElement,
    ) -> Result<()> {
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

    fn id(&self) -> ElementId {
        self.id
    }

    fn subrole(&self) -> Result<CFString> {
        Ok(CFString::from_static_string(kAXStandardWindowSubrole))
    }

    fn parent(&self) -> Result<FakeAXUIElement> {
        self.parent
            .upgrade()
            .map(|kind| FakeAXUIElement { elem: kind.into() })
            .ok_or(Error::Ax(kAXErrorCannotComplete))
    }

    fn title(&self) -> Result<CFString> {
        Ok(CFString::from_static_string(""))
    }

    fn frontmost(&self) -> Result<CFBoolean> {
        let Some(parent) = self.parent.upgrade() else {
            return Err(Error::Ax(kAXErrorCannotComplete));
        };
        let same = parent.lock().frontmost_id == Some(self.wsid);
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
        parent.lock().frontmost_id = Some(self.wsid);
        Ok(())
    }

    fn window_id(&self) -> Result<WindowServerId> {
        Ok(self.wsid)
    }

    fn attribute(&self, attribute: &str) -> Result<Box<dyn Any>> {
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

    fn set_attribute(
        &mut self,
        attribute: &str,
        value: Box<dyn Any>,
        this: &FakeAXUIElement,
    ) -> Result<()> {
        #[allow(non_upper_case_globals)]
        match attribute {
            kAXPositionAttribute => {
                self.frame.origin = value.expect_value_cast();
                self.connection.observer.notify(self, kAXWindowMovedNotification, this.into());
            }
            kAXSizeAttribute => {
                self.frame.size = value.expect_value_cast();
                self.connection.observer.notify(self, kAXWindowResizedNotification, this.into());
            }
            _ => todo!(),
        }
        Ok(())
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
    server: Ptr<WindowServer>,
}

impl Connection {
    fn new(server: Ptr<WindowServer>, observer: FakeObserver) -> Self {
        Connection {
            connected: Arc::new(AtomicBool::new(true)),
            observer,
            server,
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
pub struct Ptr<T: ?Sized>(Arc<Mutex<T>>);
#[derive(Debug)]
struct Weak<T: ?Sized>(std::sync::Weak<Mutex<T>>);

impl<T> Ptr<T> {
    fn new(inner: T) -> Self {
        Self(Arc::new(Mutex::new(inner)))
    }
    fn new_cyclic(f: impl FnOnce(&Weak<T>) -> T) -> Self {
        Self(Arc::new_cyclic(|weak| Mutex::new(f(&Weak(weak.clone())))))
    }
}

impl<T: ?Sized> Ptr<T> {
    fn lock(&self) -> MutexGuard<'_, T> {
        self.0.lock().unwrap()
    }

    fn downgrade(this: &Self) -> Weak<T> {
        Weak(Arc::downgrade(&this.0))
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

impl<T> Weak<T> {
    fn upgrade(&self) -> Option<Ptr<T>> {
        self.0.upgrade().map(Ptr)
    }
}

impl<T: ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        Self(std::sync::Weak::clone(&self.0))
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
pub struct FakeObserver(Arc<Mutex<FakeObserverInner>>);

#[derive(Debug)]
struct FakeObserverInner {
    // TODO: Arc cycle
    subscriptions: HashMap<ElementId, HashSet<&'static str>>,
    notifications_tx: Sender<(
        AXUIElement,
        String,
        Option<mpsc::UnboundedSender<pid_t>>,
        Option<Span>,
    )>,
    updates_tx: Sender<()>,
    updates_rx: Option<Receiver<()>>,
}

impl FakeObserver {
    pub fn new(
        notifications_tx: Sender<(
            AXUIElement,
            String,
            Option<mpsc::UnboundedSender<pid_t>>,
            Option<Span>,
        )>,
    ) -> Self {
        let (updates_tx, updates_rx) = channel();
        Self(Arc::new(Mutex::new(FakeObserverInner {
            subscriptions: HashMap::default(),
            notifications_tx,
            updates_tx,
            updates_rx: Some(updates_rx),
        })))
    }

    pub fn add_notification(
        &self,
        elem: &FakeAXUIElement,
        notification: &'static str,
    ) -> Result<()> {
        self.lock().subscriptions_for(elem.clone()).insert(notification);
        Ok(())
    }

    pub fn remove_notification(
        &self,
        elem: &FakeAXUIElement,
        notification: &'static str,
    ) -> Result<()> {
        self.lock().subscriptions_for(elem.clone()).remove(notification);
        Ok(())
    }

    pub fn updates_rx(&self) -> Option<Receiver<()>> {
        self.lock().updates_rx.take()
    }

    fn notify(&self, elem: &impl Element, notification: &'static str, target: &FakeAXUIElement) {
        let this = self.lock();
        if let Some(subscriptions) = this.subscriptions.get(&elem.id()) {
            if subscriptions.contains(&notification) {
                trace!("Sending {notification}");
                _ = this.notifications_tx.send((
                    target.clone().into(),
                    notification.to_owned(),
                    None,
                    Some(Span::current()),
                ));
            } else {
                trace!(
                    ?subscriptions,
                    "Skipping notification {notification} because it is not subscribed on the element",
                );
            }
        } else {
            trace!("Skipping notification {notification} because there are no subscriptions for the element");
        }
        _ = this.updates_tx.send(());
    }

    fn flush(&self, tx: Sender<pid_t>, target: &FakeAXUIElement) {
        let this = self.lock();
        _ = this.notifications_tx.send((
            target.clone().into(),
            FLUSH_NOTIFICATION.to_owned(),
            Some(tx),
            Some(Span::current()),
        ));
    }

    fn lock(&self) -> MutexGuard<'_, FakeObserverInner> {
        self.0.lock().unwrap()
    }

    fn pending_notifications(
        &self,
        notifications_rx: &mut Receiver<(
            AXUIElement,
            String,
            Option<mpsc::UnboundedSender<pid_t>>,
            Option<Span>,
        )>,
    ) -> Vec<(AXUIElement, String)> {
        let mut notifs = Vec::new();
        while let Ok((elem, notif, _, _)) = notifications_rx.try_recv() {
            notifs.push((elem, notif))
        }
        notifs
    }
}

impl FakeObserverInner {
    fn subscriptions_for(&mut self, elem: FakeAXUIElement) -> &mut HashSet<&'static str> {
        self.subscriptions.entry(elem.elem.lock().id()).or_default()
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
        let (notifications_tx, mut notifications_rx) = channel();
        let server = WindowServer::new();
        let observer = FakeObserver::new(notifications_tx);
        let app = Application::new(123, server, observer.clone());
        let app_elem: AXUIElement = app.clone().into();
        observer
            .add_notification(app_elem.inner(), kAXWindowCreatedNotification)
            .unwrap();
        assert_eq!(app_elem.windows().unwrap().len(), 0);
        assert_err(app_elem.main_window(), kAXErrorNoValue);
        assert!(observer.pending_notifications(&mut notifications_rx).is_empty());
        let win = app.mk_window();
        let win_elem: AXUIElement = win.clone().into();
        assert_eq!(app_elem.windows().unwrap().len(), 1);
        assert_err(app_elem.main_window(), kAXErrorNoValue);
        assert_eq!(win_elem.parent().expect("no parent"), app_elem);
        assert_eq!(
            observer.pending_notifications(&mut notifications_rx),
            vec![(win_elem, kAXWindowCreatedNotification.to_owned())]
        );
    }

    #[test]
    fn terminate_abruptly() {
        let (notifications_tx, _notifications_rx) = channel();
        let server = WindowServer::new();
        let observer = FakeObserver::new(notifications_tx);
        let app = Application::new(123, server, observer.clone());
        let app_elem: AXUIElement = app.clone().into();
        let win = app.mk_window();
        let win_elem: AXUIElement = win.clone().into();
        app.terminate_abruptly();
        assert_err(app_elem.windows(), kAXErrorCannotComplete);
        assert_err(app_elem.main_window(), kAXErrorCannotComplete);
        assert_err(win_elem.parent(), kAXErrorCannotComplete);
    }
}
