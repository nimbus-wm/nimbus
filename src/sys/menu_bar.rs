//! Menu bar icon for displaying the current space ID.

use std::ffi::c_void;

use objc2::AnyThread;
use objc2::rc::Retained;
use objc2_app_kit::{NSImage, NSStatusBar, NSStatusItem, NSVariableStatusItemLength};
use objc2_core_foundation::CGSize;
use objc2_foundation::{MainThreadMarker, NSData, NSString};
use tracing::{debug, warn};

use crate::sys::screen::SpaceId;

/// Manages a menu bar icon that displays the current space ID.
pub struct StatusIcon {
    status_item: Option<Retained<NSStatusItem>>,
    mtm: MainThreadMarker,
}

impl StatusIcon {
    /// Creates a new menu bar manager.
    pub fn new(mtm: MainThreadMarker) -> Self {
        let mut this = Self { status_item: None, mtm };
        this.initialize();
        this
    }

    /// Initializes the menu bar icon.
    fn initialize(&mut self) {
        unsafe {
            let status_bar = NSStatusBar::systemStatusBar();
            let status_item = status_bar.statusItemWithLength(NSVariableStatusItemLength);

            // Create parachute icon
            if let Some(button) = status_item.button(self.mtm)
                && let Some(parachute_image) = self.create_parachute_icon()
            {
                button.setImage(Some(&parachute_image));
            }

            self.status_item = Some(status_item);
        }

        // Set initial title
        self.update_space_id(None);
    }

    /// Updates the menu bar icon to show the current space ID.
    pub fn update_space_id(&mut self, space_id: Option<SpaceId>) {
        let Some(ref status_item) = self.status_item else {
            warn!("Attempted to update menu bar before initialization");
            return;
        };

        let title = if let Some(space_id) = space_id {
            format!("{:?}", space_id)
        } else {
            "".to_string()
        };
        let ns_title = NSString::from_str(&title);

        unsafe {
            if let Some(button) = status_item.button(self.mtm) {
                button.setTitle(&ns_title);
            } else {
                warn!("Could not get button from status item");
            }
        }
    }

    /// Removes the menu bar icon.
    fn cleanup(&mut self) {
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
        // Load the SVG file
        let svg_data = include_str!("../../site/src/assets/parachute-small.svg");
        let ns_data = unsafe {
            NSData::dataWithBytes_length(svg_data.as_ptr() as *const c_void, svg_data.len())
        };

        let Some(image) = NSImage::initWithData(NSImage::alloc(), &ns_data) else {
            return None;
        };

        unsafe {
            // Set the image size to be appropriate for menu bar (16x16 points)
            image.setSize(CGSize { width: 16.0, height: 16.0 });
            // Set as template image so it follows system appearance
            // image.setTemplate(true);
        }

        Some(image)
    }
}

impl Drop for StatusIcon {
    fn drop(&mut self) {
        self.cleanup();
    }
}
