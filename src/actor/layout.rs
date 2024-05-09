use std::io::Write;

use icrate::Foundation::CGRect;
use tracing::{debug, error};

use crate::{
    actor::app::{pid_t, WindowId},
    model::{Direction, LayoutKind, LayoutTree, Orientation},
    sys::screen::SpaceId,
};

pub struct LayoutManager {
    tree: LayoutTree,
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
    SaveAndExit,
}

#[derive(Debug, Clone)]
pub enum LayoutEvent {
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
        LayoutManager { tree: LayoutTree::new() }
    }

    pub fn add_window(&mut self, space: SpaceId, wid: WindowId) {
        let space = self.tree.space(space);
        self.tree.add_window(space, wid);
    }

    pub fn add_windows(&mut self, space: SpaceId, wids: impl Iterator<Item = WindowId>) {
        let space = self.tree.space(space);
        self.tree.add_windows(space, wids);
    }

    pub fn retain_windows(&mut self, f: impl FnMut(&WindowId) -> bool) {
        self.tree.retain_windows(f)
    }

    pub fn set_windows_for_app(&mut self, space: SpaceId, app: pid_t, wids: Vec<WindowId>) {
        self.tree.set_windows_for_app(space, app, wids);
    }

    #[allow(dead_code)]
    pub fn windows(&self) -> impl Iterator<Item = WindowId> + '_ {
        self.tree.windows()
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        debug!(?event);
        match event {
            LayoutEvent::WindowRaised(space, wid) => {
                if let Some(wid) = wid {
                    if let Some(node) = self.tree.window_node(space, wid) {
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
                if let Some(node) = self.tree.window_node(space, wid) {
                    self.tree.set_frame_from_resize(node, old_frame, new_frame, screen);
                }
            }
        }
        EventResponse::default()
    }

    pub fn handle_command(&mut self, space: SpaceId, command: LayoutCommand) -> EventResponse {
        let root = self.tree.space(space);
        debug!("Tree:\n{}", self.tree.draw_tree(root).trim());
        debug!(selection = ?self.tree.selection(root));
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
                    .selection(root)
                    .and_then(|cur| self.tree.traverse(cur, direction))
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
            LayoutCommand::Ascend => {
                self.tree.ascend_selection(root);
                EventResponse::default()
            }
            LayoutCommand::Descend => {
                self.tree.descend_selection(root);
                EventResponse::default()
            }
            LayoutCommand::MoveNode(direction) => {
                if let Some(selection) = self.tree.selection(root) {
                    self.tree.move_node(selection, direction);
                }
                EventResponse::default()
            }
            LayoutCommand::Split(orientation) => {
                if let Some(selection) = self.tree.selection(root) {
                    self.tree.nest_in_container(selection, LayoutKind::from(orientation));
                }
                EventResponse::default()
            }
            LayoutCommand::Group(orientation) => {
                if let Some(parent) =
                    self.tree.selection(root).and_then(|s| s.parent(self.tree.map()))
                {
                    self.tree.set_layout(parent, LayoutKind::group(orientation));
                }
                EventResponse::default()
            }
            LayoutCommand::Ungroup => {
                if let Some(parent) =
                    self.tree.selection(root).and_then(|s| s.parent(self.tree.map()))
                {
                    if self.tree.layout(parent).is_group() {
                        self.tree.set_layout(parent, self.tree.last_ungrouped_layout(parent))
                    }
                }
                EventResponse::default()
            }
            LayoutCommand::Debug => {
                let root = self.tree.space(space);
                self.tree.print_tree(root);
                EventResponse::default()
            }
            LayoutCommand::Serialize => {
                println!("{}", ron::ser::to_string(&self.tree).unwrap());
                EventResponse::default()
            }
            LayoutCommand::SaveAndExit => match self.save() {
                Ok(()) => std::process::exit(0),
                Err(e) => {
                    error!("Could not save layout: {e}");
                    std::process::exit(3);
                }
            },
        }
    }

    pub fn calculate(&mut self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let space = self.tree.space(space);
        //debug!("{}", self.tree.draw_tree(space));
        self.tree.calculate_layout(space, screen)
    }

    fn save(&self) -> std::io::Result<()> {
        let config_dir = &dirs::home_dir().unwrap().join(".nimbus");
        std::fs::create_dir_all(config_dir)?;
        std::fs::File::create(config_dir.join("layout.ron"))?
            .write_all(ron::ser::to_string(&self.tree).unwrap().as_bytes())?;
        Ok(())
    }
}
