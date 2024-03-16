use icrate::Foundation::CGRect;
use tracing::debug;

use crate::{
    app::WindowId,
    model::{Direction, LayoutKind, LayoutTree, Orientation},
    screen::SpaceId,
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
    MoveNode(Direction),
    Split(Orientation),
    Debug,
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

    #[allow(dead_code)]
    pub fn windows(&self) -> impl Iterator<Item = WindowId> + '_ {
        self.tree.windows()
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        debug!(?event);
        match event {
            LayoutEvent::WindowRaised(space, wid) => {
                let space = self.tree.space(space);
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
                let space = self.tree.space(space);
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
            LayoutCommand::MoveNode(direction) => {
                if let Some(selection) = self.tree.selection(root) {
                    self.tree.move_node(selection, direction);
                }
                EventResponse::default()
            }
            LayoutCommand::Split(orientation) => {
                if let Some(selection) = self.tree.selection(root) {
                    let kind = match orientation {
                        Orientation::Horizontal => LayoutKind::Horizontal,
                        Orientation::Vertical => LayoutKind::Vertical,
                    };
                    self.tree.nest_in_container(selection, kind);
                }
                EventResponse::default()
            }
            LayoutCommand::Debug => {
                let root = self.tree.space(space);
                println!("{}", self.tree.draw_tree(root));
                EventResponse::default()
            }
        }
    }

    pub fn calculate(&mut self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let space = self.tree.space(space);
        //debug!("{}", self.tree.draw_tree(space));
        self.tree.calculate_layout(space, screen)
    }
}
