use icrate::Foundation::CGRect;
use tracing::debug;

use crate::{
    app::WindowId,
    model::{Direction, Tree},
    screen::SpaceId,
};

pub struct LayoutManager {
    tree: Tree,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone)]
pub enum LayoutCommand {
    Shuffle,
    NextWindow,
    PrevWindow,
}

#[derive(Debug, Clone)]
pub enum LayoutEvent {
    WindowRaised(SpaceId, Option<WindowId>),
    WindowResized {
        space: SpaceId,
        wid: WindowId,
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
        LayoutManager { tree: Tree::new() }
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
                self.tree.select(wid.and_then(|wid| self.tree.window_node(space, wid)));
            }
            LayoutEvent::WindowResized { space, wid, new_frame, screen } => {
                let space = self.tree.space(space);
                if let Some(node) = self.tree.window_node(space, wid) {
                    // This is inefficient, but works for now.
                    if let Some((_, old_frame)) = self
                        .tree
                        .calculate_layout(space, screen)
                        .into_iter()
                        .filter(|(w, _)| *w == wid)
                        .next()
                    {
                        self.tree.set_frame_from_resize(node, old_frame, new_frame, screen);
                    }
                }
            }
        }
        EventResponse::default()
    }

    pub fn handle_command(&mut self, space: SpaceId, command: LayoutCommand) -> EventResponse {
        let root = self.tree.space(space);
        debug!("Tree:\n{}", self.tree.draw_tree(root).trim());
        debug!(selection = ?self.tree.selection());
        match command {
            LayoutCommand::Shuffle => {
                // TODO
                // self.window_order.shuffle(&mut rand::thread_rng());
                EventResponse::default()
            }
            LayoutCommand::NextWindow => {
                let new = self
                    .tree
                    .selection()
                    // TODO
                    .and_then(|cur| self.tree.traverse(cur, Direction::Right))
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
            LayoutCommand::PrevWindow => {
                let new = self
                    .tree
                    .selection()
                    // TODO
                    .and_then(|cur| self.tree.traverse(cur, Direction::Left))
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
        }
    }

    pub fn calculate(&mut self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let space = self.tree.space(space);
        //debug!("{}", self.tree.draw_tree(space));
        self.tree.calculate_layout(space, screen)
    }
}
