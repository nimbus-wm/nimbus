use std::ffi::c_int;

use bitflags::bitflags;
use core_foundation::{
    array::{CFArray, CFArrayRef},
    base::TCFType,
};

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

// Based on https://github.com/asmagill/hs._asm.undocumented.spaces/blob/master/CGSSpace.h.

#[link(name = "CoreGraphics", kind = "framework")]
extern "C" {
    fn CGSMainConnectionID() -> c_int;
    fn CGSGetActiveSpace(cid: c_int) -> u64;
    fn CGSCopySpaces(cid: c_int, mask: CGSSpaceMask) -> CFArrayRef;
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
