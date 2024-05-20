use std::{
    collections::HashMap,
    fs::{self, File},
    io::{Read, Write},
    path::PathBuf,
};

use icrate::Foundation::CGRect;
use serde::{Deserialize, Serialize};
use tracing::{debug, error};

use crate::{
    actor::app::{pid_t, WindowId},
    model::{Direction, LayoutId, LayoutKind, LayoutTree, Orientation},
    sys::screen::SpaceId,
};

/// Actor that manages the layout tree.
///
/// This actor receives commands and (cleaned up) events from the Reactor,
/// converts them into layout operations, and calculates the desired position
/// and size of each window.
#[derive(Serialize, Deserialize)]
pub struct LayoutManager {
    tree: LayoutTree,
    spaces: HashMap<SpaceId, LayoutId>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LayoutCommand {
    Shuffle,
    NextWindow,
    PrevWindow,
    MoveFocus(Direction),
    Ascend,
    Descend,
    MoveNode(Direction),
    Split(Orientation),
    Group(Orientation),
    Ungroup,
    Debug,
    Serialize,
    SaveAndExit(PathBuf),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutEvent {
    WindowsOnScreenUpdated(SpaceId, pid_t, Vec<WindowId>),
    AppClosed(pid_t),
    WindowAdded(SpaceId, WindowId),
    WindowRemoved(WindowId),
    WindowRaised(SpaceId, Option<WindowId>),
    WindowResized {
        space: SpaceId,
        wid: WindowId,
        old_frame: CGRect,
        new_frame: CGRect,
        screen: CGRect,
    },
}

#[must_use]
#[derive(Debug, Clone, Default)]
pub struct EventResponse {
    pub raise_window: Option<WindowId>,
}

impl LayoutManager {
    pub fn new() -> Self {
        LayoutManager {
            tree: LayoutTree::new(),
            spaces: Default::default(),
        }
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        debug!(?event);
        match event {
            LayoutEvent::WindowsOnScreenUpdated(space, pid, windows) => {
                // The windows may already be in the layout if we restored a saved state, so
                // make sure not to duplicate or erase them here.
                let layout = self.layout(space);
                self.tree.set_windows_for_app(layout, pid, windows);
            }
            LayoutEvent::AppClosed(pid) => {
                self.tree.retain_windows(|w| w.pid != pid);
            }
            LayoutEvent::WindowAdded(space, wid) => {
                let layout = self.layout(space);
                self.tree.add_window(layout, self.tree.root(layout), wid);
            }
            LayoutEvent::WindowRemoved(wid) => {
                self.tree.retain_windows(|&w| w != wid);
            }
            LayoutEvent::WindowRaised(space, wid) => {
                if let Some(wid) = wid {
                    let layout = self.layout(space);
                    if let Some(node) = self.tree.window_node(layout, wid) {
                        self.tree.select(node);
                    }
                }
            }
            LayoutEvent::WindowResized {
                space,
                wid,
                old_frame,
                new_frame,
                screen,
            } => {
                let layout = self.layout(space);
                if let Some(node) = self.tree.window_node(layout, wid) {
                    self.tree.set_frame_from_resize(node, old_frame, new_frame, screen);
                }
            }
        }
        EventResponse::default()
    }

    pub fn handle_command(&mut self, space: SpaceId, command: LayoutCommand) -> EventResponse {
        let layout = self.layout(space);
        debug!("Tree:\n{}", self.tree.draw_tree(layout).trim());
        debug!(selection = ?self.tree.selection(layout));
        match command {
            LayoutCommand::Shuffle => {
                // TODO
                // self.window_order.shuffle(&mut rand::thread_rng());
                EventResponse::default()
            }
            LayoutCommand::NextWindow => {
                // TODO
                self.handle_command(space, LayoutCommand::MoveFocus(Direction::Left))
            }
            LayoutCommand::PrevWindow => {
                // TODO
                self.handle_command(space, LayoutCommand::MoveFocus(Direction::Right))
            }
            LayoutCommand::MoveFocus(direction) => {
                let new = self
                    .tree
                    .traverse(self.tree.selection(layout), direction)
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
            LayoutCommand::Ascend => {
                self.tree.ascend_selection(layout);
                EventResponse::default()
            }
            LayoutCommand::Descend => {
                self.tree.descend_selection(layout);
                EventResponse::default()
            }
            LayoutCommand::MoveNode(direction) => {
                let selection = self.tree.selection(layout);
                self.tree.move_node(layout, selection, direction);
                EventResponse::default()
            }
            LayoutCommand::Split(orientation) => {
                let selection = self.tree.selection(layout);
                self.tree.nest_in_container(layout, selection, LayoutKind::from(orientation));
                EventResponse::default()
            }
            LayoutCommand::Group(orientation) => {
                if let Some(parent) = self.tree.selection(layout).parent(self.tree.map()) {
                    self.tree.set_layout(parent, LayoutKind::group(orientation));
                }
                EventResponse::default()
            }
            LayoutCommand::Ungroup => {
                if let Some(parent) = self.tree.selection(layout).parent(self.tree.map()) {
                    if self.tree.layout(parent).is_group() {
                        self.tree.set_layout(parent, self.tree.last_ungrouped_layout(parent))
                    }
                }
                EventResponse::default()
            }
            LayoutCommand::Debug => {
                self.tree.print_tree(layout);
                EventResponse::default()
            }
            LayoutCommand::Serialize => {
                println!("{}", self.serialize_to_string());
                EventResponse::default()
            }
            LayoutCommand::SaveAndExit(path) => match self.save(path) {
                Ok(()) => std::process::exit(0),
                Err(e) => {
                    error!("Could not save layout: {e}");
                    std::process::exit(3);
                }
            },
        }
    }

    pub fn calculate_layout(&mut self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let layout = self.layout(space);
        //debug!("{}", self.tree.draw_tree(space));
        self.tree.calculate_layout(layout, screen)
    }

    fn layout(&mut self, space: SpaceId) -> LayoutId {
        self.spaces.entry(space).or_insert_with(|| self.tree.create_layout()).clone()
    }

    pub fn load(path: PathBuf) -> anyhow::Result<Self> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        Ok(ron::from_str(&buf)?)
    }

    fn save(&self, path: PathBuf) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        File::create(path)?.write_all(self.serialize_to_string().as_bytes())?;
        Ok(())
    }

    fn serialize_to_string(&self) -> String {
        ron::ser::to_string(&self).unwrap()
    }

    // This seems a bit messy, but it's simpler and more robust to write some
    // reactor tests as integration tests with this actor.
    #[cfg(test)]
    pub(super) fn selected_window(&mut self, space: SpaceId) -> Option<WindowId> {
        let layout = self.layout(space);
        self.tree.window_at(self.tree.selection(layout))
    }
}
