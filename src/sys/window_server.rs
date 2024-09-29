use accessibility::AXUIElement;
use accessibility_sys::{kAXErrorSuccess, pid_t, AXError, AXUIElementRef};
use core_foundation::{
    array::CFArray,
    base::{CFType, ItemRef, TCFType},
    dictionary::CFDictionary,
    number::CFNumber,
    string::{CFString, CFStringRef},
};
use core_graphics::{
    display::{
        kCGNullWindowID, kCGWindowListOptionOnScreenOnly, CGWindowID, CGWindowListCopyWindowInfo,
    },
    window::{
        kCGWindowBounds, kCGWindowLayer, kCGWindowListOptionExcludeDesktopElements,
        kCGWindowNumber, kCGWindowOwnerPID, CGWindowListCreateDescriptionFromArray,
    },
};
use icrate::Foundation::CGRect;
use serde::{Deserialize, Serialize};

use super::geometry::{CGRectDef, ToICrate};

/// The window ID used by the window server.
///
/// Obtaining this from AXUIElement uses a private API and is *not* guaranteed.
/// Any functionality depending on this should have a backup plan in case it
/// breaks in the future.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct WindowServerId(CGWindowID);

impl WindowServerId {
    #[cfg(test)]
    pub fn new(id: CGWindowID) -> Self {
        WindowServerId(id)
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl Into<u32> for WindowServerId {
    fn into(self) -> u32 {
        self.0
    }
}

impl TryFrom<&AXUIElement> for WindowServerId {
    type Error = accessibility::Error;
    fn try_from(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        let mut id = 0;
        let res = unsafe { _AXUIElementGetWindow(element.as_concrete_TypeRef(), &mut id) };
        if res != kAXErrorSuccess {
            return Err(accessibility::Error::Ax(res));
        }
        Ok(WindowServerId(id))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(unused)]
pub struct WindowServerInfo {
    pub id: WindowServerId,
    pub pid: pid_t,
    pub layer: i32,
    #[serde(with = "CGRectDef")]
    pub frame: CGRect,
}

/// Returns a list of windows visible on the screen, in order starting with the
/// frontmost.
pub fn get_visible_windows_with_layer(layer: Option<i32>) -> Vec<WindowServerInfo> {
    get_visible_windows_raw()
        .iter()
        .filter_map(|win| make_info(win, layer))
        .collect::<Vec<_>>()
}

/// Returns a list of windows visible on the screen, in order starting with the
/// frontmost.
pub fn get_visible_windows_raw() -> CFArray<CFDictionary<CFString, CFType>> {
    // Note that the ordering is not documented. But
    // NSWindow::windowNumbersWithOptions *is* documented to return the windows
    // in order, so we could always combine their information if the behavior
    // changed.
    unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly | kCGWindowListOptionExcludeDesktopElements,
            kCGNullWindowID,
        ))
    }
}

fn make_info(
    win: ItemRef<CFDictionary<CFString, CFType>>,
    layer_filter: Option<i32>,
) -> Option<WindowServerInfo> {
    let layer = get_num(&win, unsafe { kCGWindowLayer })?.try_into().ok()?;
    if !(layer_filter.is_none() || layer_filter == Some(layer)) {
        return None;
    }
    let id = get_num(&win, unsafe { kCGWindowNumber })?;
    let pid = get_num(&win, unsafe { kCGWindowOwnerPID })?;
    let frame: CFDictionary = win.find(unsafe { kCGWindowBounds })?.downcast()?;
    let frame = core_graphics_types::geometry::CGRect::from_dict_representation(&frame)?;
    Some(WindowServerInfo {
        id: WindowServerId(id.try_into().ok()?),
        pid: pid.try_into().ok()?,
        layer,
        frame: frame.to_icrate(),
    })
}

pub fn get_windows(ids: &[CGWindowID]) -> Vec<WindowServerInfo> {
    if ids.is_empty() {
        return Vec::new();
    }
    get_windows_inner(ids).iter().flat_map(|w| make_info(w, None)).collect()
}

pub fn get_window(id: CGWindowID) -> Option<WindowServerInfo> {
    get_windows_inner(&[id]).iter().next().and_then(|w| make_info(w, None))
}

fn get_windows_inner(ids: &[CGWindowID]) -> CFArray<CFDictionary<CFString, CFType>> {
    let array = CFArray::from_copyable(ids);
    unsafe {
        CFArray::wrap_under_create_rule(CGWindowListCreateDescriptionFromArray(
            array.as_concrete_TypeRef(),
        ))
    }
}

fn get_num(dict: &CFDictionary<CFString, CFType>, key: CFStringRef) -> Option<i64> {
    let item: CFNumber = dict.find(key)?.downcast()?;
    Some(item.to_i64()?)
}

extern "C" {
    fn _AXUIElementGetWindow(elem: AXUIElementRef, wid: *mut CGWindowID) -> AXError;
}
