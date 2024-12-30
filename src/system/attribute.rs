use accessibility::value::AXValue;
#[cfg(test)]
use accessibility_sys::{
    kAXAllowedValuesAttribute, kAXChildrenAttribute, kAXContentsAttribute, kAXDescriptionAttribute,
    kAXElementBusyAttribute, kAXEnabledAttribute, kAXFocusedAttribute, kAXFocusedWindowAttribute,
    kAXFrameAttribute, kAXFrontmostAttribute, kAXHelpAttribute, kAXIdentifierAttribute,
    kAXLabelValueAttribute, kAXMainAttribute, kAXMainWindowAttribute, kAXMaxValueAttribute,
    kAXMinValueAttribute, kAXMinimizedAttribute, kAXParentAttribute, kAXPlaceholderValueAttribute,
    kAXPositionAttribute, kAXRoleAttribute, kAXRoleDescriptionAttribute,
    kAXSelectedChildrenAttribute, kAXSizeAttribute, kAXSubroleAttribute, kAXTitleAttribute,
    kAXTopLevelUIElementAttribute, kAXValueAttribute, kAXValueDescriptionAttribute,
    kAXValueIncrementAttribute, kAXVisibleChildrenAttribute, kAXWindowAttribute,
    kAXWindowsAttribute,
};
use core_foundation::base::ItemRef;
use core_foundation::{array::CFArray, base::CFType, boolean::CFBoolean, string::CFString};
use core_graphics_types::geometry::{CGPoint, CGRect, CGSize};
use std::fmt::Debug;
#[cfg(test)]
use std::marker::PhantomData;
use std::ops::Deref;

use super::AXUIElementInner;
use super::{AXAttribute, AXUIElement, Error};

#[cfg(test)]
#[derive(Clone)]
pub struct FakeAXAttribute<T>(&'static str, PhantomData<*const T>);

#[cfg(test)]
impl FakeAXAttribute<CFType> {
    pub fn new(_name: &CFString) -> Self {
        todo!()
    }
}

#[cfg(test)]
impl<T> Debug for FakeAXAttribute<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
impl<T> FakeAXAttribute<T> {
    // In the real type this method has return type &CFString, but that's dumb
    #[allow(non_snake_case)]
    pub fn as_CFString(&self) -> CFString {
        CFString::from_static_string(self.0)
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

#[cfg(test)]
macro_rules! constructor {
    ($name:ident, AXUIElement, $const:ident $(,$setter:ident)?) => {
        pub fn $name() -> FakeAXAttribute<AXUIElementInner> {
            FakeAXAttribute($const, PhantomData)
        }
    };
    ($name:ident, impl Iterable<Item = AXUIElement>, $const:ident $(,$setter:ident)?) => {
        pub fn $name() -> FakeAXAttribute<Vec<AXUIElementInner>> {
            FakeAXAttribute($const, PhantomData)
        }
    };
    ($name:ident, $typ:ty, $const:ident $(,$setter:ident)?) => {
        pub fn $name() -> FakeAXAttribute<$typ> {
            FakeAXAttribute($const, PhantomData)
        }
    };
}

macro_rules! accessor {
    (@decl $name:ident, AXValue<$typ:ty>, $const:ident, $setter:ident) => {
        accessor!(@decl $name, AXValue<$typ>, $const);
        fn $setter(&self, value: impl Into<$typ>) -> Result<(), Error>;
    };
    (@decl $name:ident, $typ:ty, $const:ident, $setter:ident) => {
        accessor!(@decl $name, $typ, $const);
        fn $setter(&self, value: impl Into<$typ>) -> Result<(), Error>;
    };
    (@decl $name:ident, AXValue<$typ:ty>, $const:ident) => {
        fn $name(&self) -> Result<$typ, Error>;
    };
    (@decl $name:ident, $typ:ty, $const:ident) => {
        fn $name(&self) -> Result<$typ, Error>;
    };
    (@impl $name:ident, AXValue<$typ:ty>, $const:ident, $setter:ident) => {
        accessor!(@impl $name, AXValue<$typ>, $const);
        fn $setter(&self, value: impl Into<$typ>) -> Result<(), Error> {
            self.set_attribute(&AXAttribute::$name(), AXValue::new(&value.into()).expect("wrong type"))
        }
    };
    (@impl $name:ident, $typ:ty, $const:ident, $setter:ident) => {
        accessor!(@impl $name, $typ, $const);
        fn $setter(&self, value: impl Into<$typ>) -> Result<(), Error> {
            self.set_attribute(&AXAttribute::$name(), value)
        }
    };
    (@impl $name:ident, AXValue<$typ:ty>, $const:ident) => {
        fn $name(&self) -> Result<$typ, Error> {
            self.attribute(&AXAttribute::$name()).map(|v| v.value().expect("wrong type"))
        }
    };
    (@impl $name:ident, AXUIElement, $const:ident) => {
        fn $name(&self) -> Result<AXUIElement, Error> {
            self.attribute(&AXAttribute::$name()).map(AXUIElement)
        }
    };
    (@impl $name:ident, $typ:ty, $const:ident) => {
        fn $name(&self) -> Result<$typ, Error> {
            self.attribute(&AXAttribute::$name())
        }
    };
}

/*
macro_rules! debug_field {
    ($self:ident, $fmt:ident, $name:ident, AXUIElement, $($rest:tt)*) => {
        debug_field!(@short $self, $fmt, $name, AXUIElement, $long_name);
    };
    ($self:ident, $fmt:ident, $name:ident, CFArray<AXUIElement>, $($rest:tt)*) => {
            let $name = $self.$name();
            if let Ok(value) = &$name {
                $fmt.field(stringify!($name), &NoAlternate(Forward(&value)));
            }
    };
    ($self:ident, $fmt:ident, $name:ident, CFBoolean, $($rest:tt)*) => {
            let $name = $self.$name();
            if let Ok(value) = &$name {
                let value: bool = value.clone().into();
                $fmt.field(stringify!($name), &value);
            }
    };
    (@short $self:ident, $fmt:ident, $name:ident, $type:ty, $($rest:tt)*) => {
        let $name = $self.$name();
        if let Ok(value) = &$name {
            $fmt.field(stringify!($name), &NoAlternate(value));
        }
    };
    ($self:ident, $fmt:ident, $name:ident, $type:ty, $($rest:tt)*) => {
        let $name = $self.$name();
        if let Ok(value) = &$name {
            $fmt.field(stringify!($name), &value);
        }
    };
}

struct NoAlternate<T>(T);
impl<T: Debug> Debug for NoAlternate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

struct Forward<'a>(&'a CFArray<AXUIElement>);
impl<'a> Debug for Forward<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for elem in self.0 {
            let elem: &AXUIElement = &*elem;
            list.entry(elem);
        }
        list.finish()
    }
}
*/

macro_rules! define_attributes {
    (@get_sys_name $name:ident, $type:ty, $sys_name:ident $(, $rest:tt)*) => {
        $sys_name
    };

    ($(($($args:tt)*)),*,) => {
        #[cfg(test)]
        impl FakeAXAttribute<()> {
            $(constructor!($($args)*);)*
        }

        pub trait AXUIElementAttributes {
            $(accessor!(@decl $($args)*);)*
        }

        impl AXUIElementAttributes for AXUIElement {
            $(accessor!(@impl $($args)*);)*
        }

        /*
        impl AXUIElement {
            pub(crate) fn debug_all(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut fmt = f.debug_struct("AXUIElement");

                // $(debug_field!(self, fmt, $($args)*);)*

                let Ok(attr_names) = self.attribute_names() else {
                    return fmt.finish();
                };
                let attr_names: Vec<CFString> = attr_names.iter().filter(|name| {
                    $(**name != define_attributes!(@get_sys_name $($args)*) &&)* true
                }).map(|n| n.clone()).collect();
                for name in attr_names {
                    let attr = AXAttribute(name, PhantomData);
                    if let Ok(val) = self.attribute::<CFType>(&attr) {
                        fmt.field(&attr.as_CFString().to_string(), &val);
                    }
                }
                fmt.finish()
            }
        }
        */
    }
}

define_attributes![
    // These we want to appear first in debug output.
    (role, CFString, kAXRoleAttribute),
    (subrole, CFString, kAXSubroleAttribute),
    // The rest are in alphabetical order.
    (allowed_values, CFArray<CFType>, kAXAllowedValuesAttribute),
    (
        children,
        impl Iterable<Item = AXUIElement>,
        kAXChildrenAttribute
    ),
    (contents, AXUIElement, kAXContentsAttribute),
    (description, CFString, kAXDescriptionAttribute),
    (element_busy, CFBoolean, kAXElementBusyAttribute),
    (enabled, CFBoolean, kAXEnabledAttribute),
    (focused, CFBoolean, kAXFocusedAttribute),
    (focused_window, AXUIElement, kAXFocusedWindowAttribute),
    (frontmost, CFBoolean, kAXFrontmostAttribute, set_frontmost),
    (frame, AXValue<CGRect>, kAXFrameAttribute),
    (help, CFString, kAXHelpAttribute),
    (identifier, CFString, kAXIdentifierAttribute),
    (label_value, CFString, kAXLabelValueAttribute),
    (main, CFBoolean, kAXMainAttribute, set_main),
    (main_window, AXUIElement, kAXMainWindowAttribute),
    (max_value, CFType, kAXMaxValueAttribute),
    (min_value, CFType, kAXMinValueAttribute),
    (minimized, CFBoolean, kAXMinimizedAttribute),
    (parent, AXUIElement, kAXParentAttribute),
    (placeholder_value, CFString, kAXPlaceholderValueAttribute),
    (
        position,
        AXValue<CGPoint>,
        kAXPositionAttribute,
        set_position
    ),
    (role_description, CFString, kAXRoleDescriptionAttribute),
    (
        selected_children,
        impl Iterable<Item = AXUIElement>,
        kAXSelectedChildrenAttribute
    ),
    (size, AXValue<CGSize>, kAXSizeAttribute, set_size),
    (title, CFString, kAXTitleAttribute),
    (
        top_level_ui_element,
        AXUIElement,
        kAXTopLevelUIElementAttribute
    ),
    (value, CFType, kAXValueAttribute, set_value),
    (value_description, CFString, kAXValueDescriptionAttribute),
    (value_increment, CFType, kAXValueIncrementAttribute),
    (
        visible_children,
        impl Iterable<Item = AXUIElement>,
        kAXVisibleChildrenAttribute
    ),
    (window, AXUIElement, kAXWindowAttribute),
    (
        windows,
        impl Iterable<Item = AXUIElement>,
        kAXWindowsAttribute
    ),
];

pub trait Iterable: Debug + 'static {
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
            unsafe { std::mem::transmute(inner) }
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
                unsafe { std::mem::transmute(elem) }
            }
            convert(elem)
        })
    }

    fn len(&self) -> usize {
        CFArray::len(self) as usize
    }
}
