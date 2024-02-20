#![allow(dead_code)]

use std::{collections::HashMap, mem};

use icrate::Foundation::CGRect;

use crate::{
    app::WindowId,
    model::node::{Forest, NodeId, OwnedNode},
    screen::SpaceId,
};

use super::{
    layout::{Direction, Layout, LayoutKind},
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
        self.c.layout.set_kind(node, kind);
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

    pub fn traverse(&self, from: NodeId, direction: Direction) -> Option<NodeId> {
        let forest = &self.forest;
        let selection = &self.c.selection;
        let Some(mut node) =
            // Keep going up...
            from.ancestors(forest)
            // ...until we can move in the desired direction, then move.
            .flat_map(|n| self.move_over(n, direction)).next()
        else {
            return None;
        };
        // Descend as far down as we can go, keeping close to the direction we're
        // moving from.
        loop {
            let child = if self.c.layout.kind(node).orientation() == direction.orientation() {
                match direction {
                    Direction::Up | Direction::Left => node.first_child(forest),
                    Direction::Down | Direction::Right => node.last_child(forest),
                }
            } else {
                selection.local_selection(forest, node).or(node.first_child(forest))
            };
            let Some(child) = child else { break };
            node = child;
        }
        Some(node)
    }

    fn move_over(&self, from: NodeId, direction: Direction) -> Option<NodeId> {
        let Some(parent) = from.parent(&self.forest) else {
            return None;
        };
        if self.c.layout.kind(parent).orientation() == direction.orientation() {
            match direction {
                Direction::Up | Direction::Left => from.prev_sibling(&self.forest),
                Direction::Down | Direction::Right => from.next_sibling(&self.forest),
            }
        } else {
            None
        }
    }

    pub fn resize(&mut self, node: NodeId, screen_ratio: f64, direction: Direction) -> bool {
        // Pick an ancestor to resize that has a sibling in the given direction.
        let can_resize = |&node: &NodeId| -> bool {
            let Some(parent) = node.parent(&self.forest) else {
                return false;
            };
            !self.c.layout.kind(parent).is_group() && self.move_over(node, direction).is_some()
        };
        let Some(resizing_node) = node.ancestors(&self.forest).filter(can_resize).next() else {
            return false;
        };
        let sibling = self.move_over(resizing_node, direction).unwrap();

        // Compute the share of resizing_node's parent that needs to be taken
        // from the sibling.
        let exchange_rate = resizing_node.ancestors(&self.forest).skip(1).fold(1.0, |r, node| {
            match node.parent(&self.forest) {
                Some(parent)
                    if self.c.layout.kind(parent).orientation() == direction.orientation()
                        && !self.c.layout.kind(parent).is_group() =>
                {
                    r * self.c.layout.proportion(&self.forest, node).unwrap()
                }
                _ => r,
            }
        });
        let local_ratio = f64::from(screen_ratio)
            * self.c.layout.total(resizing_node.parent(&self.forest).unwrap())
            / exchange_rate;
        self.c
            .layout
            .take_share(&self.forest, resizing_node, sibling, local_ratio as f32);

        true
    }

    pub fn draw_tree(&self, root: NodeId) -> String {
        let tree = self.get_ascii_tree(root);
        let mut out = String::new();
        ascii_tree::write_tree(&mut out, &tree).unwrap();
        out
    }

    fn get_ascii_tree(&self, node: NodeId) -> ascii_tree::Tree {
        let desc = format!("{node:?} {layout:?}", layout = self.c.layout.debug(node));
        let children: Vec<_> =
            node.children(&self.forest).map(|c| self.get_ascii_tree(c)).collect();
        if children.is_empty() {
            let lines = [
                Some(desc),
                self.windows.get(node).map(|wid| format!("{wid:?}")),
            ];
            ascii_tree::Tree::Leaf(lines.into_iter().flatten().collect())
        } else {
            ascii_tree::Tree::Node(desc, children)
        }
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use icrate::Foundation::{CGPoint, CGSize};
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::{model::Tree, screen::SpaceId};

    struct TestTree {
        tree: Tree,
        root: NodeId,
    }

    #[test]
    fn traverse() {
        let mut tree = Tree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Vertical);
        let b1 = tree.add_window(a2, WindowId::new(2, 1));
        let b2 = tree.add_window(a2, WindowId::new(2, 2));
        let b3 = tree.add_window(a2, WindowId::new(2, 3));
        let a3 = tree.add_window(root, WindowId::new(1, 3));
        tree.select(b2);

        use Direction::*;
        assert_eq!(tree.traverse(a1, Left), None);
        assert_eq!(tree.traverse(a1, Up), None);
        assert_eq!(tree.traverse(a1, Down), None);
        assert_eq!(tree.traverse(a1, Right), Some(b2));
        assert_eq!(tree.traverse(a2, Left), Some(a1));
        assert_eq!(tree.traverse(a2, Up), None);
        assert_eq!(tree.traverse(a2, Down), None);
        assert_eq!(tree.traverse(a2, Right), Some(a3));
        assert_eq!(tree.traverse(b1, Left), Some(a1));
        assert_eq!(tree.traverse(b1, Up), None);
        assert_eq!(tree.traverse(b1, Down), Some(b2));
        assert_eq!(tree.traverse(b1, Right), Some(a3));
        assert_eq!(tree.traverse(b2, Left), Some(a1));
        assert_eq!(tree.traverse(b2, Up), Some(b1));
        assert_eq!(tree.traverse(b2, Down), Some(b3));
        assert_eq!(tree.traverse(b2, Right), Some(a3));
        assert_eq!(tree.traverse(b3, Left), Some(a1));
        assert_eq!(tree.traverse(b3, Up), Some(b2));
        assert_eq!(tree.traverse(b3, Down), None);
        assert_eq!(tree.traverse(b3, Right), Some(a3));
        assert_eq!(tree.traverse(a3, Left), Some(b2));
        assert_eq!(tree.traverse(a3, Up), None);
        assert_eq!(tree.traverse(a3, Down), None);
        assert_eq!(tree.traverse(a3, Right), None);
    }

    fn rect(x: i32, y: i32, w: i32, h: i32) -> CGRect {
        CGRect::new(
            CGPoint::new(f64::from(x), f64::from(y)),
            CGSize::new(f64::from(w), f64::from(h)),
        )
    }

    #[track_caller]
    fn assert_frames_are(
        left: impl IntoIterator<Item = (WindowId, CGRect)>,
        right: impl IntoIterator<Item = (WindowId, CGRect)>,
    ) {
        // Use BTreeMap for dedup and sorting.
        let left: BTreeMap<_, _> = left.into_iter().collect();
        let right: BTreeMap<_, _> = right.into_iter().collect();
        assert_eq!(left, right);
    }

    #[test]
    fn resize() {
        // ┌─────┬─────┬─────┐
        // │     │ b1  │     │
        // │     +─────+     │
        // │ a1  │c1│c2│  a3 │
        // │     +─────+     │
        // │     │ b3  │     │
        // └─────┴─────┴─────┘
        let mut tree = Tree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Vertical);
        let _b1 = tree.add_window(a2, WindowId::new(2, 1));
        let b2 = tree.add_container(a2, LayoutKind::Horizontal);
        let _c1 = tree.add_window(b2, WindowId::new(3, 1));
        let c2 = tree.add_window(b2, WindowId::new(3, 2));
        let _b3 = tree.add_window(a2, WindowId::new(2, 3));
        let _a3 = tree.add_window(root, WindowId::new(1, 3));
        let screen = rect(0, 0, 3000, 3000);
        println!("{}", tree.draw_tree(root));

        let orig = vec![
            (WindowId::new(1, 1), rect(0, 0, 1000, 3000)),
            (WindowId::new(2, 1), rect(1000, 0, 1000, 1000)),
            (WindowId::new(3, 1), rect(1000, 1000, 500, 1000)),
            (WindowId::new(3, 2), rect(1500, 1000, 500, 1000)),
            (WindowId::new(2, 3), rect(1000, 2000, 1000, 1000)),
            (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
        ];
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());

        tree.resize(c2, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(space, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 1000, 3000)),
                (WindowId::new(2, 1), rect(1000, 0, 1030, 1000)),
                (WindowId::new(3, 1), rect(1000, 1000, 515, 1000)),
                (WindowId::new(3, 2), rect(1515, 1000, 515, 1000)),
                (WindowId::new(2, 3), rect(1000, 2000, 1030, 1000)),
                (WindowId::new(1, 3), rect(2030, 0, 970, 3000)),
            ],
        );

        tree.resize(c2, -0.01, Direction::Right);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());

        tree.resize(c2, 0.01, Direction::Left);
        assert_frames_are(
            tree.calculate_layout(space, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 1000, 3000)),
                (WindowId::new(2, 1), rect(1000, 0, 1000, 1000)),
                (WindowId::new(3, 1), rect(1000, 1000, 470, 1000)),
                (WindowId::new(3, 2), rect(1470, 1000, 530, 1000)),
                (WindowId::new(2, 3), rect(1000, 2000, 1000, 1000)),
                (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
            ],
        );

        tree.resize(c2, -0.01, Direction::Left);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());

        tree.resize(b2, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(space, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 1000, 3000)),
                (WindowId::new(2, 1), rect(1000, 0, 1030, 1000)),
                (WindowId::new(3, 1), rect(1000, 1000, 515, 1000)),
                (WindowId::new(3, 2), rect(1515, 1000, 515, 1000)),
                (WindowId::new(2, 3), rect(1000, 2000, 1030, 1000)),
                (WindowId::new(1, 3), rect(2030, 0, 970, 3000)),
            ],
        );

        tree.resize(b2, -0.01, Direction::Right);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());

        tree.resize(a1, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(space, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 1030, 3000)),
                (WindowId::new(2, 1), rect(1030, 0, 970, 1000)),
                (WindowId::new(3, 1), rect(1030, 1000, 485, 1000)),
                (WindowId::new(3, 2), rect(1515, 1000, 485, 1000)),
                (WindowId::new(2, 3), rect(1030, 2000, 970, 1000)),
                (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
            ],
        );

        tree.resize(a1, -0.01, Direction::Right);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());

        tree.resize(a1, 0.01, Direction::Left);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());
        tree.resize(a1, -0.01, Direction::Left);
        assert_frames_are(tree.calculate_layout(space, screen), orig.clone());
    }
}
