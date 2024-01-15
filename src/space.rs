use std::{ffi::c_int, mem::MaybeUninit};

use bitflags::bitflags;
use core_foundation::{
    array::{CFArray, CFArrayRef},
    base::TCFType,
    string::{CFString, CFStringRef},
};
use core_graphics::display::{CGDisplayBounds, CGGetActiveDisplayList};
use core_graphics_types::base::{kCGErrorSuccess, CGError};
use icrate::{
    objc2::{msg_send, ClassType},
    AppKit::NSScreen,
    Foundation::{ns_string, CGPoint, CGRect, CGSize, MainThreadMarker, NSNumber},
};
use log::{debug, warn};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SpaceId(u64);

pub struct ScreenCache<S: System = Actual> {
    system: S,
    uuids: Vec<CFString>,
}

#[allow(dead_code)]
impl ScreenCache<Actual> {
    pub fn new(mtm: MainThreadMarker) -> Self {
        Self::new_with(Actual { mtm })
    }
}

#[allow(dead_code)]
impl<S: System> ScreenCache<S> {
    fn new_with(system: S) -> ScreenCache<S> {
        ScreenCache { uuids: vec![], system }
    }

    /// Returns a list of screen frames and updates the internal cache.
    ///
    /// Note that there may be no screens. If there are, the main screen is
    /// always first.
    #[forbid(unsafe_code)] // called from test
    pub fn screen_frames(&mut self) -> Result<Vec<CGRect>, CGError> {
        let mut cg_screens = self.system.cg_screens()?;
        debug!("cg_screens={cg_screens:?}");
        if cg_screens.is_empty() {
            return Ok(vec![]);
        };

        // Ensure that the main screen is always first.
        let main_screen_idx = cg_screens
            .iter()
            .position(|s| s.bounds.origin == CGPoint::ZERO)
            .expect("Could not find the main screen");
        cg_screens.swap(0, main_screen_idx);

        self.uuids = cg_screens
            .iter()
            .map(|screen| self.system.uuid_for_rect(screen.bounds))
            .collect();

        // We want to get the visible_frame of the NSScreenInfo, but in CG's
        // top-left coordinates from NSScreen's bottom-left.
        let ns_screens = self.system.ns_screens();
        debug!("ns_screens={ns_screens:?}");

        // The main screen has origin (0, 0) in both coordinate systems.
        let ns_origin_y = cg_screens[0].bounds.max().y;

        let visible_frames = cg_screens
            .iter()
            .flat_map(|&CGScreenInfo { cg_id, .. }| {
                let Some(ns_screen) = ns_screens.iter().find(|s| s.cg_id == cg_id) else {
                    warn!("Can't find NSScreen corresponding to screen number {cg_id}");
                    return None;
                };
                let converted = CGRect {
                    origin: CGPoint {
                        x: ns_screen.visible_frame.origin.x,
                        // Take the original origin, in converted coordinates,
                        // and move up to the top-left of the visible frame.
                        // This is the converted origin of the visible frame.
                        y: ns_origin_y - ns_screen.visible_frame.max().y,
                    },
                    size: ns_screen.visible_frame.size,
                };
                Some(converted)
            })
            .collect();
        Ok(visible_frames)
    }

    /// Returns a list of the active spaces. The order corresponds to the
    /// screens returned by `screen_frames`.
    pub fn screen_spaces(&self) -> Vec<SpaceId> {
        self.uuids
            .iter()
            .map(|screen| unsafe {
                CGSManagedDisplayGetCurrentSpace(
                    CGSMainConnectionID(),
                    screen.as_concrete_TypeRef(),
                )
            })
            .map(SpaceId)
            .collect()
    }
}

#[allow(private_interfaces)]
pub trait System {
    fn cg_screens(&self) -> Result<Vec<CGScreenInfo>, CGError>;
    fn uuid_for_rect(&self, rect: CGRect) -> CFString;
    fn ns_screens(&self) -> Vec<NSScreenInfo>;
}

#[derive(Debug, Clone)]
struct CGScreenInfo {
    cg_id: CGDirectDisplayID,
    bounds: CGRect,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct NSScreenInfo {
    frame: CGRect,
    visible_frame: CGRect,
    cg_id: CGDirectDisplayID,
}

pub struct Actual {
    mtm: MainThreadMarker,
}
#[allow(private_interfaces)]
impl System for Actual {
    fn cg_screens(&self) -> Result<Vec<CGScreenInfo>, CGError> {
        const MAX_SCREENS: usize = 64;
        let mut ids: MaybeUninit<[CGDirectDisplayID; MAX_SCREENS]> = MaybeUninit::uninit();
        let mut count: u32 = 0;
        let ids = unsafe {
            let err = CGGetActiveDisplayList(
                MAX_SCREENS as u32,
                ids.as_mut_ptr() as *mut CGDirectDisplayID,
                &mut count,
            );
            if err != kCGErrorSuccess {
                return Err(err);
            }
            std::slice::from_raw_parts(ids.as_ptr() as *const u32, count as usize)
        };
        Ok(ids
            .iter()
            .map(|&cg_id| CGScreenInfo {
                cg_id,
                bounds: unsafe { CGDisplayBounds(cg_id).to_icrate() },
            })
            .collect())
    }

    fn uuid_for_rect(&self, rect: CGRect) -> CFString {
        unsafe {
            CFString::wrap_under_create_rule(CGSCopyBestManagedDisplayForRect(
                CGSMainConnectionID(),
                rect,
            ))
        }
    }

    fn ns_screens(&self) -> Vec<NSScreenInfo> {
        NSScreen::screens(self.mtm)
            .iter()
            .flat_map(|s| {
                let desc = s.deviceDescription();
                let cg_id = match desc.get(ns_string!("NSScreenNumber")) {
                    Some(val) if unsafe { msg_send![val, isKindOfClass:NSNumber::class() ] } => {
                        let number: &NSNumber = unsafe { std::mem::transmute(val) };
                        number.as_u32()
                    }
                    val => {
                        warn!(
                            "Could not get NSScreenNumber for screen with name {:?}: {:?}",
                            unsafe { s.localizedName() },
                            val,
                        );
                        return None;
                    }
                };
                Some(NSScreenInfo {
                    frame: s.frame(),
                    visible_frame: s.visibleFrame(),
                    cg_id,
                })
            })
            .collect()
    }
}

trait ToICrate<T> {
    fn to_icrate(&self) -> T;
}

impl ToICrate<CGPoint> for core_graphics_types::geometry::CGPoint {
    fn to_icrate(&self) -> CGPoint {
        CGPoint { x: self.x, y: self.y }
    }
}

impl ToICrate<CGSize> for core_graphics_types::geometry::CGSize {
    fn to_icrate(&self) -> CGSize {
        CGSize {
            width: self.width,
            height: self.height,
        }
    }
}

impl ToICrate<CGRect> for core_graphics_types::geometry::CGRect {
    fn to_icrate(&self) -> CGRect {
        CGRect {
            origin: self.origin.to_icrate(),
            size: self.size.to_icrate(),
        }
    }
}

pub fn cur_space() -> SpaceId {
    SpaceId(unsafe { CGSGetActiveSpace(CGSMainConnectionID()) })
}

#[allow(dead_code)]
pub fn visible_spaces() -> CFArray<SpaceId> {
    unsafe {
        let arr = CGSCopySpaces(CGSMainConnectionID(), CGSSpaceMask::ALL_VISIBLE_SPACES);
        CFArray::wrap_under_create_rule(arr)
    }
}

#[allow(dead_code)]
pub fn all_spaces() -> CFArray<SpaceId> {
    unsafe {
        let arr = CGSCopySpaces(CGSMainConnectionID(), CGSSpaceMask::ALL_SPACES);
        CFArray::wrap_under_create_rule(arr)
    }
}

type CGDirectDisplayID = u32;

pub fn managed_displays() -> CFArray {
    unsafe { CFArray::wrap_under_create_rule(CGSCopyManagedDisplays(CGSMainConnectionID())) }
}

pub fn managed_display_spaces() -> CFArray<SpaceId> {
    unsafe { CFArray::wrap_under_create_rule(CGSCopyManagedDisplaySpaces(CGSMainConnectionID())) }
}

// Based on https://github.com/asmagill/hs._asm.undocumented.spaces/blob/master/CGSSpace.h.
// Also see https://github.com/koekeishiya/yabai/blob/d55a647913ab72d8d8b348bee2d3e59e52ce4a5d/src/misc/extern.h.

#[link(name = "CoreGraphics", kind = "framework")]
extern "C" {
    fn CGSMainConnectionID() -> c_int;
    fn CGSGetActiveSpace(cid: c_int) -> u64;
    fn CGSCopySpaces(cid: c_int, mask: CGSSpaceMask) -> CFArrayRef;
    fn CGSCopyManagedDisplays(cid: c_int) -> CFArrayRef;
    fn CGSCopyManagedDisplaySpaces(cid: c_int) -> CFArrayRef;
    fn CGSManagedDisplayGetCurrentSpace(cid: c_int, uuid: CFStringRef) -> u64;
    fn CGSCopyBestManagedDisplayForRect(cid: c_int, rect: CGRect) -> CFStringRef;
}

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    #[repr(transparent)]
    struct CGSSpaceMask: c_int {
        const INCLUDE_CURRENT = 1 << 0;
        const INCLUDE_OTHERS  = 1 << 1;

        const INCLUDE_USER    = 1 << 2;
        const INCLUDE_OS      = 1 << 3;

        const VISIBLE         = 1 << 16;

        const CURRENT_SPACES = Self::INCLUDE_USER.bits() | Self::INCLUDE_CURRENT.bits();
        const OTHER_SPACES = Self::INCLUDE_USER.bits() | Self::INCLUDE_OTHERS.bits();
        const ALL_SPACES =
            Self::INCLUDE_USER.bits() | Self::INCLUDE_OTHERS.bits() | Self::INCLUDE_CURRENT.bits();

        const ALL_VISIBLE_SPACES = Self::ALL_SPACES.bits() | Self::VISIBLE.bits();

        const CURRENT_OS_SPACES = Self::INCLUDE_OS.bits() | Self::INCLUDE_CURRENT.bits();
        const OTHER_OS_SPACES = Self::INCLUDE_OS.bits() | Self::INCLUDE_OTHERS.bits();
        const ALL_OS_SPACES =
            Self::INCLUDE_OS.bits() | Self::INCLUDE_OTHERS.bits() | Self::INCLUDE_CURRENT.bits();
    }
}

#[cfg(test)]
mod test {
    use core_foundation::string::CFString;
    use icrate::Foundation::{CGPoint, CGRect, CGSize};

    use super::{CGScreenInfo, NSScreenInfo, ScreenCache, System};

    struct Stub {
        cg_screens: Vec<CGScreenInfo>,
        ns_screens: Vec<NSScreenInfo>,
    }
    impl System for Stub {
        fn cg_screens(&self) -> Result<Vec<CGScreenInfo>, core_graphics_types::base::CGError> {
            Ok(self.cg_screens.clone())
        }
        fn ns_screens(&self) -> Vec<NSScreenInfo> {
            self.ns_screens.clone()
        }
        fn uuid_for_rect(&self, _rect: CGRect) -> CFString {
            CFString::new("stub")
        }
    }

    #[test]
    fn it_calculates_the_visible_frame() {
        println!("test");
        let stub = Stub {
            cg_screens: vec![
                CGScreenInfo {
                    cg_id: 1,
                    bounds: CGRect::new(CGPoint::new(3840.0, 1080.0), CGSize::new(1512.0, 982.0)),
                },
                CGScreenInfo {
                    cg_id: 3,
                    bounds: CGRect::new(CGPoint::new(0.0, 0.0), CGSize::new(3840.0, 2160.0)),
                },
            ],
            ns_screens: vec![
                NSScreenInfo {
                    cg_id: 3,
                    frame: CGRect::new(CGPoint::new(0.0, 0.0), CGSize::new(3840.0, 2160.0)),
                    visible_frame: CGRect::new(
                        CGPoint::new(0.0, 76.0),
                        CGSize::new(3840.0, 2059.0),
                    ),
                },
                NSScreenInfo {
                    cg_id: 1,
                    frame: CGRect::new(CGPoint::new(3840.0, 98.0), CGSize::new(1512.0, 982.0)),
                    visible_frame: CGRect::new(
                        CGPoint::new(3840.0, 98.0),
                        CGSize::new(1512.0, 950.0),
                    ),
                },
            ],
        };
        let mut sc = ScreenCache::new_with(stub);
        assert_eq!(
            vec![
                CGRect::new(CGPoint::new(0.0, 25.0), CGSize::new(3840.0, 2059.0)),
                CGRect::new(CGPoint::new(3840.0, 1112.0), CGSize::new(1512.0, 950.0)),
            ],
            sc.screen_frames().unwrap()
        );
    }
}
