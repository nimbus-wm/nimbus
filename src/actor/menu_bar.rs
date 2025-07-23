//! Menu bar icon management for displaying the current space ID.

use objc2::AnyThread;
use objc2::rc::Retained;
use objc2_app_kit::{NSImage, NSStatusBar, NSStatusItem};
use objc2_foundation::{MainThreadMarker, NSData, NSString};
use tracing::{debug, warn};

use crate::sys::screen::SpaceId;

/// Manages a menu bar icon that displays the current space ID.
pub struct MenuBarManager {
    status_item: Option<Retained<NSStatusItem>>,
    mtm: MainThreadMarker,
}

impl MenuBarManager {
    /// Creates a new menu bar manager.
    pub fn new(mtm: MainThreadMarker) -> Self {
        Self { status_item: None, mtm }
    }

    /// Initializes the menu bar icon.
    pub fn initialize(&mut self) {
        debug!("Initializing menu bar icon");

        unsafe {
            let status_bar = NSStatusBar::systemStatusBar();
            let status_item = status_bar.statusItemWithLength(-1.0); // NSVariableStatusItemLength

            // Create parachute icon
            if let Some(parachute_image) = self.create_parachute_icon() {
                if let Some(button) = status_item.button(self.mtm) {
                    button.setImage(Some(&parachute_image));
                }
            }

            self.status_item = Some(status_item);
        }

        // Set initial title
        self.update_space_id(None);
        debug!("Menu bar icon initialized");
    }

    /// Updates the menu bar icon to show the current space ID.
    pub fn update_space_id(&mut self, space_id: Option<SpaceId>) {
        let Some(ref status_item) = self.status_item else {
            warn!("Attempted to update menu bar before initialization");
            return;
        };

        let title = if let Some(space_id) = space_id {
            format!("{}", space_id.as_u64())
        } else {
            "?".to_string()
        };

        let ns_title = NSString::from_str(&title);

        unsafe {
            if let Some(button) = status_item.button(self.mtm) {
                button.setTitle(&ns_title);
                debug!("Updated menu bar title to: {}", title);
            } else {
                warn!("Could not get button from status item");
            }
        }
    }

    /// Removes the menu bar icon.
    pub fn cleanup(&mut self) {
        if let Some(status_item) = self.status_item.take() {
            debug!("Removing menu bar icon");
            unsafe {
                let status_bar = NSStatusBar::systemStatusBar();
                status_bar.removeStatusItem(&status_item);
            }
        }
    }

    /// Creates the parachute icon from the SVG file
    fn create_parachute_icon(&self) -> Option<Retained<NSImage>> {
        unsafe {
            // Load the SVG file
            let svg_data = include_str!("../../site/src/assets/parachute-small.svg");
            let ns_data = NSData::dataWithBytes_length(
                svg_data.as_ptr() as *const std::ffi::c_void,
                svg_data.len(),
            );

            if let Some(image) = NSImage::initWithData(NSImage::alloc(), &ns_data) {
                // Set the image size to be appropriate for menu bar (16x16 points)
                image.setSize(objc2_core_foundation::CGSize { width: 16.0, height: 16.0 });
                // Set as template image so it follows system appearance
                // image.setTemplate(true);
                return Some(image);
            }

            // Fallback to SF Symbol if SVG loading fails
            let symbol_string = NSString::from_str("parachute");
            if let Some(image) =
                NSImage::imageWithSystemSymbolName_accessibilityDescription(&symbol_string, None)
            {
                image.setTemplate(true);
                return Some(image);
            }

            None
        }
    }
}

impl Drop for MenuBarManager {
    fn drop(&mut self) {
        self.cleanup();
    }
}
