//! Space management APIs for macOS Spaces (Mission Control).
//!
//! This module provides bindings to the Core Graphics Services (CGS) private APIs
//! for managing spaces on macOS. These APIs are used by Mission Control and allow
//! for programmatic access to desktop spaces.

use std::ffi::c_int;

use core_foundation::array::{CFArray, CFArrayRef};
use core_foundation::base::TCFType;
use core_foundation::dictionary::{CFDictionary, CFDictionaryRef};
use core_foundation::number::CFNumber;
use core_foundation::string::{CFString, CFStringRef};
use core_graphics::base::CGError;
use core_graphics_types::geometry::CGAffineTransform;
use objc2_core_foundation::CGRect;

// Re-export SpaceId for easier access
pub use super::screen::SpaceId;

/// Representations of the possible types of spaces the system can create.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum SpaceType {
    /// User-created desktop spaces.
    User = 0,
    /// Fullscreen spaces.
    Fullscreen = 1,
    /// System spaces e.g. Dashboard.
    System = 2,
}

/// Space management mode for displays.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum SpaceManagementMode {
    /// Each display manages a single contiguous space.
    None = 0,
    /// Each display manages a separate stack of spaces.
    PerDesktop = 1,
}

/// Helper function to convert CFArray<CFNumber> to Vec<SpaceId>
pub fn cfarray_to_space_ids(cf_array: &CFArray<CFNumber>) -> Vec<SpaceId> {
    (0..cf_array.len())
        .filter_map(|i| {
            cf_array.get(i).and_then(|num_ref| {
                let num = num_ref.to_i64()?;
                SpaceId::from_u64(num as u64)
            })
        })
        .collect()
}

/// Helper function to get spaces as Vec<SpaceId>
pub fn get_spaces_as_vec(mask: super::screen::CGSSpaceMask) -> Vec<SpaceId> {
    unsafe {
        let array_ref = CGSCopySpaces(main_connection_id(), mask);
        let cf_array: CFArray<CFNumber> = CFArray::wrap_under_create_rule(array_ref);
        cfarray_to_space_ids(&cf_array)
    }
}

/// Get all spaces as a Vec<SpaceId>
pub fn get_all_spaces() -> Vec<SpaceId> {
    get_spaces_as_vec(super::screen::CGSSpaceMask::ALL_SPACES)
}

/// Get visible spaces as a Vec<SpaceId>
pub fn get_visible_spaces() -> Vec<SpaceId> {
    get_spaces_as_vec(super::screen::CGSSpaceMask::ALL_VISIBLE_SPACES)
}

/// Gets the connection ID to the window server.
pub fn main_connection_id() -> c_int {
    unsafe { CGSMainConnectionID() }
}

/// Gets the ID of the space currently visible to the user.
pub fn get_active_space() -> Option<SpaceId> {
    let space_id = unsafe { CGSGetActiveSpace(main_connection_id()) };
    SpaceId::from_u64(space_id)
}

/// Gets the type of a space.
pub fn get_space_type(space_id: SpaceId) -> SpaceType {
    let space_type = unsafe { CGSSpaceGetType(main_connection_id(), space_id.as_u64()) };
    match space_type {
        0 => SpaceType::User,
        1 => SpaceType::Fullscreen,
        2 => SpaceType::System,
        _ => SpaceType::User, // Default to user for unknown types
    }
}

/// Gets the current space management mode.
///
/// This method reflects whether the "Displays have separate Spaces" option is
/// enabled in Mission Control system preference.
pub fn get_space_management_mode() -> SpaceManagementMode {
    let mode = unsafe { CGSGetSpaceManagementMode(main_connection_id()) };
    match mode {
        0 => SpaceManagementMode::None,
        1 => SpaceManagementMode::PerDesktop,
        _ => SpaceManagementMode::None, // Default to None for unknown modes
    }
}

/// Sets the current space management mode.
pub fn set_space_management_mode(mode: SpaceManagementMode) -> Result<(), CGError> {
    let error = unsafe { CGSSetSpaceManagementMode(main_connection_id(), mode as c_int) };
    if error == 0 { Ok(()) } else { Err(error) }
}

/// Gets the human-readable name of a space.
pub fn copy_space_name(space_id: SpaceId) -> Option<CFString> {
    let name_ref = unsafe { CGSSpaceCopyName(main_connection_id(), space_id.as_u64()) };
    if name_ref.is_null() {
        None
    } else {
        Some(unsafe { CFString::wrap_under_create_rule(name_ref) })
    }
}

/// Sets the human-readable name of a space.
pub fn set_space_name(space_id: SpaceId, name: &str) -> Result<(), CGError> {
    let name_string = CFString::new(name);
    let error = unsafe {
        CGSSpaceSetName(
            main_connection_id(),
            space_id.as_u64(),
            name_string.as_concrete_TypeRef(),
        )
    };
    if error == 0 { Ok(()) } else { Err(error) }
}

/// Gets the affine transform of a space.
pub fn get_space_transform(space_id: SpaceId) -> CGAffineTransform {
    unsafe { CGSSpaceGetTransform(main_connection_id(), space_id.as_u64()) }
}

/// Sets the affine transform of a space.
pub fn set_space_transform(space_id: SpaceId, transform: CGAffineTransform) {
    unsafe { CGSSpaceSetTransform(main_connection_id(), space_id.as_u64(), transform) }
}

/// Creates a new space with the given options.
///
/// Valid options dictionary keys include:
/// - "type": CFNumber representing the space type
/// - "uuid": CFString for the space UUID
pub fn create_space(options: Option<CFDictionary<CFString, CFNumber>>) -> Option<SpaceId> {
    let options_ref = options.map(|dict| dict.as_concrete_TypeRef()).unwrap_or(std::ptr::null());
    let space_id =
        unsafe { CGSSpaceCreate(main_connection_id(), std::ptr::null_mut(), options_ref) };
    SpaceId::from_u64(space_id)
}

/// Removes and destroys the space corresponding to the given space ID.
pub fn destroy_space(space_id: SpaceId) {
    unsafe { CGSSpaceDestroy(main_connection_id(), space_id.as_u64()) }
}

/// Returns an array of PIDs of applications that have ownership of a given space.
pub fn copy_space_owners(space_id: SpaceId) -> Vec<i32> {
    unsafe {
        let array_ref = CGSSpaceCopyOwners(main_connection_id(), space_id.as_u64());
        let cf_array: CFArray<CFNumber> = CFArray::wrap_under_create_rule(array_ref);
        (0..cf_array.len())
            .filter_map(|i| cf_array.get(i).and_then(|num_ref| num_ref.to_i32()))
            .collect()
    }
}

/// Given an array of space IDs, each space is shown to the user.
pub fn show_spaces(space_ids: &[SpaceId]) {
    let space_ids_u64: Vec<u64> = space_ids.iter().map(|id| id.as_u64()).collect();
    let spaces_array = CFArray::from_CFTypes(
        &space_ids_u64
            .iter()
            .map(|&id| core_foundation::number::CFNumber::from(id as i64))
            .collect::<Vec<_>>(),
    );
    unsafe { CGSShowSpaces(main_connection_id(), spaces_array.as_concrete_TypeRef()) }
}

/// Given an array of space IDs, each space is hidden from the user.
pub fn hide_spaces(space_ids: &[SpaceId]) {
    let space_ids_u64: Vec<u64> = space_ids.iter().map(|id| id.as_u64()).collect();
    let spaces_array = CFArray::from_CFTypes(
        &space_ids_u64
            .iter()
            .map(|&id| core_foundation::number::CFNumber::from(id as i64))
            .collect::<Vec<_>>(),
    );
    unsafe { CGSHideSpaces(main_connection_id(), spaces_array.as_concrete_TypeRef()) }
}

/// Changes the active space for the main display.
pub fn set_current_space(space_id: SpaceId) {
    let main_display =
        unsafe { CFString::wrap_under_create_rule(kCGSPackagesMainDisplayIdentifier) };
    unsafe {
        CGSManagedDisplaySetCurrentSpace(
            main_connection_id(),
            main_display.as_concrete_TypeRef(),
            space_id.as_u64(),
        )
    }
}

/// Connection-local data in a given space.
pub fn copy_space_values(space_id: SpaceId) -> Option<CFDictionary> {
    let dict_ref = unsafe { CGSSpaceCopyValues(main_connection_id(), space_id.as_u64()) };
    if dict_ref.is_null() {
        None
    } else {
        Some(unsafe { CFDictionary::wrap_under_create_rule(dict_ref) })
    }
}

/// Sets connection-local data in a given space.
pub fn set_space_values(space_id: SpaceId, values: CFDictionary) -> Result<(), CGError> {
    let error = unsafe {
        CGSSpaceSetValues(
            main_connection_id(),
            space_id.as_u64(),
            values.as_concrete_TypeRef(),
        )
    };
    if error == 0 { Ok(()) } else { Err(error) }
}

/// Removes connection-local data for the given keys in a space.
pub fn remove_space_values_for_keys(
    space_id: SpaceId,
    keys: CFArray<CFString>,
) -> Result<(), CGError> {
    let error = unsafe {
        CGSSpaceRemoveValuesForKeys(
            main_connection_id(),
            space_id.as_u64(),
            keys.as_concrete_TypeRef(),
        )
    };
    if error == 0 { Ok(()) } else { Err(error) }
}

/// Gets the absolute level of a space.
pub fn get_space_absolute_level(space_id: SpaceId) -> i32 {
    unsafe { CGSSpaceGetAbsoluteLevel(main_connection_id(), space_id.as_u64()) }
}

/// Sets the absolute level of a space.
pub fn set_space_absolute_level(space_id: SpaceId, level: i32) {
    unsafe { CGSSpaceSetAbsoluteLevel(main_connection_id(), space_id.as_u64(), level) }
}

/// Gets the compatibility ID of a space.
pub fn get_space_compat_id(space_id: SpaceId) -> i32 {
    unsafe { CGSSpaceGetCompatID(main_connection_id(), space_id.as_u64()) }
}

/// Sets the compatibility ID of a space.
pub fn set_space_compat_id(space_id: SpaceId, compat_id: i32) {
    unsafe { CGSSpaceSetCompatID(main_connection_id(), space_id.as_u64(), compat_id) }
}

/// Sets the type of a space.
pub fn set_space_type(space_id: SpaceId, space_type: SpaceType) {
    unsafe { CGSSpaceSetType(main_connection_id(), space_id.as_u64(), space_type as c_int) }
}

/// Copies the managed display for a space.
pub fn copy_managed_display_for_space(space_id: SpaceId) -> Option<CFString> {
    let display_ref =
        unsafe { CGSCopyManagedDisplayForSpace(main_connection_id(), space_id.as_u64()) };
    if display_ref.is_null() {
        None
    } else {
        Some(unsafe { CFString::wrap_under_create_rule(display_ref) })
    }
}

/// Copies the best managed display for a rectangle.
pub fn copy_best_managed_display_for_rect(rect: CGRect) -> Option<CFString> {
    let display_ref = unsafe { CGSCopyBestManagedDisplayForRect(main_connection_id(), rect) };
    if display_ref.is_null() {
        None
    } else {
        Some(unsafe { CFString::wrap_under_create_rule(display_ref) })
    }
}

/// Copies managed display spaces.
pub fn copy_managed_display_spaces() -> Vec<SpaceId> {
    unsafe {
        let array_ref = CGSCopyManagedDisplaySpaces(main_connection_id());
        let cf_array: CFArray<CFNumber> = CFArray::wrap_under_create_rule(array_ref);
        cfarray_to_space_ids(&cf_array)
    }
}

/// Checks if a managed display is animating.
pub fn managed_display_is_animating(display: &CFString) -> bool {
    unsafe { CGSManagedDisplayIsAnimating(main_connection_id(), display.as_concrete_TypeRef()) }
}

/// Sets whether a managed display is animating.
pub fn set_managed_display_is_animating(display: &CFString, is_animating: bool) {
    unsafe {
        CGSManagedDisplaySetIsAnimating(
            main_connection_id(),
            display.as_concrete_TypeRef(),
            is_animating,
        )
    }
}

/// Returns the IDs of the spaces that the given windows lie on.
pub fn copy_spaces_for_windows(windows: &[u32]) -> Vec<SpaceId> {
    let window_numbers: Vec<CFNumber> = windows.iter().map(|&w| CFNumber::from(w as i64)).collect();
    let windows_array = CFArray::from_CFTypes(&window_numbers);

    unsafe {
        let array_ref = CGSCopySpacesForWindows(
            main_connection_id(),
            super::screen::CGSSpaceMask::ALL_SPACES,
            windows_array.as_concrete_TypeRef(),
        );
        let cf_array: CFArray<CFNumber> = CFArray::wrap_under_create_rule(array_ref);
        cfarray_to_space_ids(&cf_array)
    }
}

/// Adds windows to spaces.
pub fn add_windows_to_spaces(windows: &[u32], spaces: &[SpaceId]) {
    let window_numbers: Vec<CFNumber> = windows.iter().map(|&w| CFNumber::from(w as i64)).collect();
    let windows_array = CFArray::from_CFTypes(&window_numbers);

    let space_numbers: Vec<CFNumber> =
        spaces.iter().map(|&s| CFNumber::from(s.as_u64() as i64)).collect();
    let spaces_array = CFArray::from_CFTypes(&space_numbers);

    unsafe {
        CGSAddWindowsToSpaces(
            main_connection_id(),
            windows_array.as_concrete_TypeRef(),
            spaces_array.as_concrete_TypeRef(),
        );
    }
}

/// Removes windows from spaces.
pub fn remove_windows_from_spaces(windows: &[u32], spaces: &[SpaceId]) {
    let window_numbers: Vec<CFNumber> = windows.iter().map(|&w| CFNumber::from(w as i64)).collect();
    let windows_array = CFArray::from_CFTypes(&window_numbers);

    let space_numbers: Vec<CFNumber> =
        spaces.iter().map(|&s| CFNumber::from(s.as_u64() as i64)).collect();
    let spaces_array = CFArray::from_CFTypes(&space_numbers);

    unsafe {
        CGSRemoveWindowsFromSpaces(
            main_connection_id(),
            windows_array.as_concrete_TypeRef(),
            spaces_array.as_concrete_TypeRef(),
        );
    }
}

// Raw CGS API bindings
#[link(name = "CoreGraphics", kind = "framework")]
unsafe extern "C" {
    fn CGSMainConnectionID() -> c_int;
    fn CGSGetActiveSpace(cid: c_int) -> u64;
    fn CGSCopySpaces(cid: c_int, mask: super::screen::CGSSpaceMask) -> CFArrayRef;
    fn CGSSpaceGetType(cid: c_int, space_id: u64) -> c_int;
    fn CGSGetSpaceManagementMode(cid: c_int) -> c_int;
    fn CGSSetSpaceManagementMode(cid: c_int, mode: c_int) -> CGError;
    fn CGSSpaceCopyName(cid: c_int, space_id: u64) -> CFStringRef;
    fn CGSSpaceSetName(cid: c_int, space_id: u64, name: CFStringRef) -> CGError;
    fn CGSSpaceGetTransform(cid: c_int, space_id: u64) -> CGAffineTransform;
    fn CGSSpaceSetTransform(cid: c_int, space_id: u64, transform: CGAffineTransform);
    fn CGSSpaceCreate(cid: c_int, null: *mut std::ffi::c_void, options: CFDictionaryRef) -> u64;
    fn CGSSpaceDestroy(cid: c_int, space_id: u64);
    fn CGSSpaceCopyOwners(cid: c_int, space_id: u64) -> CFArrayRef;
    fn CGSShowSpaces(cid: c_int, spaces: CFArrayRef);
    fn CGSHideSpaces(cid: c_int, spaces: CFArrayRef);
    fn CGSManagedDisplaySetCurrentSpace(cid: c_int, display: CFStringRef, space_id: u64);
    fn CGSSpaceCopyValues(cid: c_int, space_id: u64) -> CFDictionaryRef;
    fn CGSSpaceSetValues(cid: c_int, space_id: u64, values: CFDictionaryRef) -> CGError;
    fn CGSSpaceRemoveValuesForKeys(cid: c_int, space_id: u64, keys: CFArrayRef) -> CGError;

    // Additional functions
    fn CGSSpaceGetAbsoluteLevel(cid: c_int, space_id: u64) -> i32;
    fn CGSSpaceSetAbsoluteLevel(cid: c_int, space_id: u64, level: i32);
    fn CGSSpaceGetCompatID(cid: c_int, space_id: u64) -> i32;
    fn CGSSpaceSetCompatID(cid: c_int, space_id: u64, compat_id: i32);
    fn CGSSpaceSetType(cid: c_int, space_id: u64, space_type: c_int);
    fn CGSCopyBestManagedDisplayForRect(cid: c_int, rect: CGRect) -> CFStringRef;
    fn CGSCopyManagedDisplayForSpace(cid: c_int, space_id: u64) -> CFStringRef;
    fn CGSCopyManagedDisplaySpaces(cid: c_int) -> CFArrayRef;
    fn CGSManagedDisplayIsAnimating(cid: c_int, display: CFStringRef) -> bool;
    fn CGSManagedDisplaySetIsAnimating(cid: c_int, display: CFStringRef, is_animating: bool);
    fn CGSCopySpacesForWindows(
        cid: c_int,
        mask: super::screen::CGSSpaceMask,
        windows: CFArrayRef,
    ) -> CFArrayRef;
    fn CGSAddWindowsToSpaces(cid: c_int, windows: CFArrayRef, spaces: CFArrayRef);
    fn CGSRemoveWindowsFromSpaces(cid: c_int, windows: CFArrayRef, spaces: CFArrayRef);

    static kCGSPackagesMainDisplayIdentifier: CFStringRef;
}
