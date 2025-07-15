//! Menu bar icon management for displaying the current space ID.

use objc2::rc::Retained;
use objc2_app_kit::{NSStatusBar, NSStatusItem};
use objc2_foundation::{MainThreadMarker, NSString};
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
            format!("Space {}", space_id.as_u64())
        } else {
            "Space ?".to_string()
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
}

impl Drop for MenuBarManager {
    fn drop(&mut self) {
        self.cleanup();
    }
}
