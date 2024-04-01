use accessibility::AXUIElement;
use accessibility_sys::{kAXErrorSuccess, AXError, AXUIElementRef};
use core_foundation::{
    array::CFArray,
    base::{CFType, TCFType},
    dictionary::CFDictionary,
    number::CFNumber,
    string::{CFString, CFStringRef},
};
use core_graphics::{
    display::{
        kCGNullWindowID, kCGWindowListOptionOnScreenOnly, CGWindowID, CGWindowListCopyWindowInfo,
    },
    window::{kCGWindowLayer, kCGWindowListOptionExcludeDesktopElements, kCGWindowNumber},
};

/// The window ID used by the window server.
///
/// Obtaining this from AXUIElement uses a private API and is *not* guaranteed.
/// Any functionality depending on this should have a backup plan in case it
/// breaks in the future.
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct WindowServerId(CGWindowID);

impl WindowServerId {
    #[cfg(test)]
    pub fn new(id: CGWindowID) -> Self {
        WindowServerId(id)
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

/// Returns a list of windows visible on the screen, in order starting with the
/// frontmost.
#[allow(dead_code)]
pub fn get_visible_windows() -> Vec<WindowServerId> {
    // Note that the ordering is not documented. But
    // NSWindow::windowNumbersWithOptions *is* documented to return the windows
    // in order, so we could always combine their information if the behavior
    // changed.
    let windows: CFArray<CFDictionary<CFString, CFType>> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly | kCGWindowListOptionExcludeDesktopElements,
            kCGNullWindowID,
        ))
    };
    windows
        .iter()
        .filter(|win| {
            // We only care about windows on layer zero.
            let Some(layer) = get_num(&win, unsafe { kCGWindowLayer }) else {
                return false;
            };
            layer == 0
        })
        .filter_map(|win| {
            let id = get_num(&win, unsafe { kCGWindowNumber })?;
            Some(WindowServerId(id.try_into().ok()?))
        })
        .collect::<Vec<_>>()
}

fn get_num(dict: &CFDictionary<CFString, CFType>, key: CFStringRef) -> Option<i64> {
    let item: CFNumber = dict.find(key)?.downcast()?;
    Some(item.to_i64()?)
}

extern "C" {
    fn _AXUIElementGetWindow(elem: AXUIElementRef, wid: *mut CGWindowID) -> AXError;
}
