#![allow(dead_code)]

use std::{collections::HashMap, mem};

use icrate::Foundation::CGRect;

use crate::{
    app::WindowId,
    model::node::{Forest, NodeId, OwnedNode},
    screen::SpaceId,
};

use super::{
    layout::{Layout, LayoutKind},
    node,
    selection::Selection,
};

/// The layout tree.
///
/// All interactions with the data model happen through the public APIs on this
/// type.
pub struct Tree {
    forest: Forest,
    windows: Windows,
    spaces: HashMap<SpaceId, OwnedNode>,
    c: Components,
}

pub type Windows = slotmap::SecondaryMap<NodeId, WindowId>;

#[derive(Default)]
struct Components {
    selection: Selection,
    layout: Layout,
}

#[derive(Copy, Clone)]
pub(super) enum TreeEvent {
    /// A node was added to its parent. Note that the node may have existed in
    /// the tree previously under a different parent.
    AddedToParent(NodeId),
    /// A node will be removed from its parent.
    RemovingFromParent(NodeId),
    /// A node was removed from the tree.
    RemovedFromTree(NodeId),
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

    pub fn add_window(&mut self, parent: NodeId, wid: WindowId) -> NodeId {
        let node = parent.push_back(&mut self.forest, &mut self.c);
        self.windows.insert(node, wid);
        node
    }

    pub fn add_windows(&mut self, parent: NodeId, wids: impl ExactSizeIterator<Item = WindowId>) {
        self.forest.reserve(wids.len());
        self.windows.set_capacity(self.forest.capacity());
        for wid in wids {
            self.add_window(parent, wid);
        }
    }

    pub fn retain_windows(&mut self, mut predicate: impl FnMut(&WindowId) -> bool) {
        self.windows.retain(|node, wid| {
            if !predicate(wid) {
                node.remove(&mut self.forest, &mut self.c);
                return false;
            }
            true
        })
    }

    pub fn add_container(&mut self, parent: NodeId, kind: LayoutKind) -> NodeId {
        let node = parent.push_back(&mut self.forest, &mut self.c);
        self.c.layout.set_layout(&self.forest, node, kind);
        node
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

    pub fn space(&mut self, space: SpaceId) -> NodeId {
        self.spaces
            .entry(space)
            .or_insert_with(|| OwnedNode::new_root_in(&mut self.forest, "space_root"))
            .id()
    }

    pub fn calculate_layout(&self, space: SpaceId, frame: CGRect) -> Vec<(WindowId, CGRect)> {
        self.c
            .layout
            .get_sizes(&self.forest, &self.windows, self.spaces[&space].id(), frame)
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
        self.layout.handle_event(forest, event);
    }
}

impl node::Observer for Components {
    fn added_to_parent(&mut self, forest: &Forest, node: NodeId) {
        self.dispatch_event(forest, TreeEvent::AddedToParent(node))
    }

    fn removing_from_parent(&mut self, forest: &Forest, node: NodeId) {
        self.dispatch_event(forest, TreeEvent::RemovingFromParent(node))
    }

    fn removed_from_tree(&mut self, forest: &Forest, node: NodeId) {
        self.dispatch_event(forest, TreeEvent::RemovedFromTree(node))
    }
}
