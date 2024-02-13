#![allow(dead_code)]

use std::{collections::HashMap, mem};

use crate::{
    app::WindowId,
    model::node::{Forest, NodeId, OwnedNode},
    screen::SpaceId,
};

use super::selection::Selection;

/// The layout tree.
///
/// All interactions with the data model happen through the public APIs on this
/// type.
pub struct Tree {
    forest: Forest,
    windows: slotmap::SecondaryMap<NodeId, WindowId>,
    spaces: HashMap<SpaceId, OwnedNode>,
    c: Components,
}

#[derive(Default)]
struct Components {
    selection: Selection,
}

pub(super) enum TreeEvent {
    AddedWindow(NodeId, WindowId),
    RemovingNode(NodeId),
    RemovedNode(NodeId),
}

impl Tree {
    pub fn new() -> Tree {
        Tree {
            spaces: Default::default(),
            forest: Forest::default(),
            windows: Default::default(),
            c: Components::default(),
        }
    }

    pub fn add_window(&mut self, space: SpaceId, wid: WindowId) -> NodeId {
        let node = self.space(space).push_back(&mut self.forest);
        self.windows.insert(node, wid);
        self.dispatch_event(TreeEvent::AddedWindow(node, wid));
        node
    }

    pub fn add_windows(&mut self, space: SpaceId, wids: impl ExactSizeIterator<Item = WindowId>) {
        self.forest.reserve(wids.len());
        self.windows.set_capacity(self.forest.capacity());
        for wid in wids {
            self.add_window(space, wid);
        }
    }

    pub fn retain_windows(&mut self, mut predicate: impl FnMut(&WindowId) -> bool) {
        self.windows.retain(|node, wid| {
            if !predicate(wid) {
                self.c.dispatch_event(&self.forest, TreeEvent::RemovingNode(node));
                node.remove(&mut self.forest);
                self.c.dispatch_event(&self.forest, TreeEvent::RemovedNode(node));
                return false;
            }
            true
        })
    }

    pub fn windows(&self) -> impl Iterator<Item = WindowId> + '_ {
        self.windows.iter().map(|(_, &wid)| wid)
    }

    pub fn select(&mut self, selection: impl Into<Option<NodeId>>) {
        self.c.selection.select(&self.forest, selection.into())
    }

    pub fn selection(&self) -> Option<NodeId> {
        self.c.selection.current_selection()
    }

    fn space(&mut self, space: SpaceId) -> NodeId {
        self.spaces
            .entry(space)
            .or_insert_with(|| OwnedNode::new_root_in(&mut self.forest, "space_root"))
            .id()
    }

    fn dispatch_event(&mut self, event: TreeEvent) {
        self.c.dispatch_event(&self.forest, event);
    }
}

impl Drop for Tree {
    fn drop(&mut self) {
        // It's okay to skip removing these, since we're dropping the Forest too.
        mem::forget(self.spaces.drain());
    }
}

impl Components {
    fn dispatch_event(&mut self, forest: &Forest, event: TreeEvent) {
        self.selection.handle_event(forest, event);
    }
}
