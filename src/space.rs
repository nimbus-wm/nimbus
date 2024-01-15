use std::ffi::c_int;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SpaceId(u64);

pub fn cur_space() -> SpaceId {
    SpaceId(unsafe { CGSGetActiveSpace(CGSMainConnectionID()) })
}

// Based on https://github.com/asmagill/hs._asm.undocumented.spaces/blob/master/CGSSpace.h.

#[link(name = "CoreGraphics", kind = "framework")]
extern "C" {
    fn CGSMainConnectionID() -> c_int;
    fn CGSGetActiveSpace(cid: c_int) -> u64;
}
