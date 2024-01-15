use std::{ffi::c_int, mem::MaybeUninit};

use bitflags::bitflags;
use core_foundation::{
    array::{CFArray, CFArrayRef},
    base::TCFType,
};
use core_graphics::display::CGGetActiveDisplayList;
use core_graphics_types::base::{kCGErrorSuccess, CGError};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SpaceId(u64);

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

pub fn screens() -> Result<Vec<CGDirectDisplayID>, CGError> {
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
    Ok(ids.iter().copied().collect())
}

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
