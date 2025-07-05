/// The set of layouts for a given space, keyed by screen size.
///
/// These layouts all track approximately the same windows, but we save one per
/// screen size so that users can change how their windows are laid out in
/// different configurations.
///
/// To keep the number of configurations manageable, we only save a unique
/// layout for a screen size if it was modified by user commands under that
/// screen size.
#[derive(Serialize, Deserialize, Debug)]
pub struct SpaceLayoutMapping {
    configurations: HashMap<Size, LayoutId>,
    active_size: Size,
    /// The last layout for this space that has had any structural changes made.
    last_saved: Option<LayoutId>,
    /// All the layouts owned by this space.
    layouts: Vec<LayoutId>,
}

impl SpaceLayoutMapping {
    pub fn new(size: CGSize) -> Self {
        SpaceLayoutMapping {
            active_size: size.into(),
            configurations: Default::default(),
            last_saved: None,
            layouts: vec![],
        }
    }

    pub fn create_or_activate_layout(&mut self, size: CGSize, tree: &mut LayoutTree) {
        let size = size.into();
        debug!(?self, ?size, "create_or_activate_layout");
        let mut unchanged = None;
        if self.active_layout() != self.last_saved {
            unchanged = self.configurations.remove(&self.active_size);
        }
        self.active_size = size;
        let layout = match self.configurations.entry(size) {
            Entry::Vacant(entry) => *entry.insert(if let Some(source) = unchanged.take() {
                debug!("Reusing unchanged layout {source:?}");
                source
            } else if let Some(source) = self.last_saved {
                debug!("Cloning layout {source:?}");
                tree.clone_layout(source)
            } else {
                debug!("Creating new layout");
                tree.create_layout()
            }),
            Entry::Occupied(entry) => {
                // Mark the preexisting entry as used.
                self.last_saved = Some(*entry.get());
                *entry.get()
            }
        };
        if let Some(removed) = unchanged {
            tree.remove_layout(removed);
        }
        debug!("Using layout {layout:?}");
    }

    pub fn active_layout(&self) -> Option<LayoutId> {
        self.configurations.get(&self.active_size).copied()
    }

    pub fn mark_changed(&mut self) {
        let Some(active) = self.active_layout() else {
            warn!("Could not find active layout to mark changed");
            return;
        };
        self.last_saved.replace(active);
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
struct Size {
    width: i32,
    height: i32,
}

impl From<CGSize> for Size {
    fn from(value: CGSize) -> Self {
        Self {
            width: value.width.round() as i32,
            height: value.height.round() as i32,
        }
    }
}

use objc2_core_foundation::CGSize;
use serde::{Deserialize, Serialize};
use tracing::{debug, warn};

use crate::collections::{HashMap, hash_map::Entry};
use crate::model::{LayoutId, LayoutTree};
