use std::{collections::HashMap, iter, mem};

use icrate::Foundation::CGRect;

use super::{
    layout::{Direction, Layout, LayoutKind},
    selection::Selection,
    tree::{self, Tree},
};
use crate::{
    app::WindowId,
    model::tree::{NodeId, NodeMap, OwnedNode},
    screen::SpaceId,
};

/// The layout tree.
///
/// All interactions with the data model happen through the public APIs on this
/// type.
pub struct LayoutTree {
    tree: Tree<Components>,
    windows: slotmap::SecondaryMap<NodeId, WindowId>,
    window_nodes: HashMap<WindowId, Vec<WindowNodeInfo>>,
    space_roots: HashMap<SpaceId, OwnedNode>,
    root_spaces: HashMap<NodeId, SpaceId>,
}

pub(super) type Windows = slotmap::SecondaryMap<NodeId, WindowId>;

struct WindowNodeInfo {
    space: SpaceId,
    node: NodeId,
}

#[derive(Default)]
struct Components {
    selection: Selection,
    layout: Layout,
}

#[derive(Copy, Clone)]
pub(super) enum TreeEvent {
    /// A node was added to the forest.
    AddedToForest(NodeId),
    /// A node was added to its parent. Note that the node may have existed in
    /// the tree previously under a different parent.
    AddedToParent(NodeId),
    /// A node will be removed from its parent.
    RemovingFromParent(NodeId),
    /// A node was removed from the forest.
    RemovedFromForest(NodeId),
}

impl LayoutTree {
    pub fn new() -> LayoutTree {
        LayoutTree {
            tree: Tree::with_observer(Components::default()),
            windows: Default::default(),
            window_nodes: Default::default(),
            space_roots: Default::default(),
            root_spaces: Default::default(),
        }
    }

    pub fn add_window(&mut self, parent: NodeId, wid: WindowId) -> NodeId {
        let root = parent.ancestors(&self.tree.map).last().unwrap();
        let node = self.tree.mk_node().push_back(parent);
        self.windows.insert(node, wid);
        let space = self.root_spaces[&root];
        self.window_nodes.entry(wid).or_default().push(WindowNodeInfo { space, node });
        node
    }

    pub fn add_windows(&mut self, parent: NodeId, wids: impl Iterator<Item = WindowId>) {
        self.tree.map.reserve(wids.size_hint().1.unwrap_or(0));
        self.windows.set_capacity(self.tree.map.capacity());
        for wid in wids {
            self.add_window(parent, wid);
        }
    }

    pub fn retain_windows(&mut self, mut predicate: impl FnMut(&WindowId) -> bool) {
        self.window_nodes.retain(|wid, nodes| {
            if !predicate(wid) {
                for info in nodes {
                    info.node.detach(&mut self.tree).remove();
                    self.windows.remove(info.node);
                }
                return false;
            }
            true
        })
    }

    pub fn windows(&self) -> impl Iterator<Item = WindowId> + '_ {
        self.window_nodes.keys().copied()
    }

    pub fn window_node(&self, space: SpaceId, wid: WindowId) -> Option<NodeId> {
        self.window_nodes
            .get(&wid)
            .into_iter()
            .flat_map(|nodes| nodes.iter().filter(|info| info.space == space))
            .next()
            .map(|info| info.node)
    }

    pub fn window_at(&self, node: NodeId) -> Option<WindowId> {
        self.windows.get(node).copied()
    }

    #[allow(dead_code)]
    pub fn add_container(&mut self, parent: NodeId, kind: LayoutKind) -> NodeId {
        let node = self.tree.mk_node().push_back(parent);
        self.tree.data.layout.set_kind(node, kind);
        node
    }

    pub fn select(&mut self, selection: NodeId) {
        self.tree.data.selection.select(&self.tree.map, selection)
    }

    // TODO: Remove Option
    pub fn selection(&self, root: NodeId) -> Option<NodeId> {
        Some(self.tree.data.selection.current_selection(root))
    }

    pub fn ascend_selection(&mut self, root: NodeId) -> bool {
        if let Some(parent) = self.selection(root).and_then(|n| n.parent(self.map())) {
            self.select(parent);
            return true;
        }
        false
    }

    pub fn descend_selection(&mut self, root: NodeId) -> bool {
        if let Some(child) = self
            .selection(root)
            .and_then(|n| self.tree.data.selection.last_selection(self.map(), n))
        {
            self.select(child);
            return true;
        }
        false
    }

    pub fn space(&mut self, space: SpaceId) -> NodeId {
        self.space_roots
            .entry(space)
            .or_insert_with(|| {
                let node = OwnedNode::new_root_in(&mut self.tree, "space_root");
                self.root_spaces.insert(node.id(), space);
                node
            })
            .id()
    }

    pub fn calculate_layout(&self, root: NodeId, frame: CGRect) -> Vec<(WindowId, CGRect)> {
        self.tree.data.layout.get_sizes(&self.tree.map, &self.windows, root, frame)
    }

    pub fn traverse(&self, from: NodeId, direction: Direction) -> Option<NodeId> {
        let map = &self.tree.map;
        let node =
            // Keep going up...
            from.ancestors(map)
            // ...until we can move in the desired direction, then move.
            .flat_map(|n| self.move_over(n, direction)).next();
        // Descend as far down as we can go, keeping close to the direction we're
        // moving from.
        iter::successors(node, |&node| {
            if self.tree.data.layout.kind(node).orientation() == direction.orientation() {
                match direction {
                    Direction::Up | Direction::Left => node.last_child(map),
                    Direction::Down | Direction::Right => node.first_child(map),
                }
            } else {
                self.tree.data.selection.local_selection(map, node).or(node.first_child(map))
            }
        })
        .last()
    }

    fn move_over(&self, from: NodeId, direction: Direction) -> Option<NodeId> {
        let Some(parent) = from.parent(&self.tree.map) else {
            return None;
        };
        if self.tree.data.layout.kind(parent).orientation() == direction.orientation() {
            match direction {
                Direction::Up | Direction::Left => from.prev_sibling(&self.tree.map),
                Direction::Down | Direction::Right => from.next_sibling(&self.tree.map),
            }
        } else {
            None
        }
    }

    pub fn move_node(&mut self, moving_node: NodeId, direction: Direction) -> bool {
        let map = &self.tree.map;
        let Some(old_parent) = moving_node.parent(map) else {
            return false;
        };
        let is_selection =
            self.tree.data.selection.local_selection(map, old_parent) == Some(moving_node);
        self.move_node_inner(moving_node, direction);
        if is_selection {
            for node in moving_node.ancestors(&self.tree.map).take_while(|&a| a != old_parent) {
                self.tree.data.selection.select_locally(&self.tree.map, node);
            }
        }
        true
    }

    fn move_node_inner(&mut self, moving_node: NodeId, direction: Direction) {
        /// Where to insert the node, along the direction we're moving.
        enum Destination {
            Ahead(NodeId),
            Behind(NodeId),
        }
        let map = &self.tree.map;
        let destination;
        if let Some(sibling) = self.move_over(moving_node, direction) {
            // Traverse down the sibling until we hit the next node with
            // the same orientation we're moving in.
            let mut node = sibling;
            let target = loop {
                let Some(next) =
                    self.tree.data.selection.local_selection(map, node).or(node.first_child(map))
                else {
                    break node;
                };
                if self.tree.data.layout.kind(node).orientation() == direction.orientation() {
                    break next;
                }
                node = next;
            };
            if target == sibling {
                // Our sibling is a leaf; we're switching places.
                destination = Destination::Ahead(sibling);
            } else {
                // The target is our new sibling. We have already moved laterally,
                // so don't do that here.
                destination = Destination::Behind(target);
            }
        } else {
            // Traverse up the tree until we can move in the desired direction.
            let target = moving_node
                .ancestors_with_parent(&self.tree.map)
                .skip(1) // We already tried moving at the current level.
                .skip_while(|(_node, parent)| {
                    parent
                        .map(|p| self.layout(p).orientation() != direction.orientation())
                        // If we get all the way to the root, give up and skip it too.
                        .unwrap_or(true)
                })
                .next();
            if let Some((target, _parent)) = target {
                // The target is our new sibling. We haven't moved laterally yet, so do that here.
                destination = Destination::Ahead(target);
            } else {
                // We went all the way to the root and couldn't move in the
                // desired direction, so we'll make a new container level above it.
                let old_root = moving_node.ancestors(map).last().unwrap();
                self.nest_in_container(old_root, LayoutKind::from(direction.orientation()));
                destination = Destination::Ahead(old_root);
            }
        }
        match (destination, direction) {
            (Destination::Ahead(target), Direction::Right | Direction::Down) => {
                moving_node.detach(&mut self.tree).insert_after(target);
            }
            (Destination::Behind(target), Direction::Right | Direction::Down) => {
                moving_node.detach(&mut self.tree).insert_before(target);
            }
            (Destination::Ahead(target), Direction::Left | Direction::Up) => {
                moving_node.detach(&mut self.tree).insert_before(target);
            }
            (Destination::Behind(target), Direction::Left | Direction::Up) => {
                moving_node.detach(&mut self.tree).insert_after(target);
            }
        }
    }

    pub fn map(&self) -> &NodeMap {
        &self.tree.map
    }

    pub fn layout(&self, node: NodeId) -> LayoutKind {
        self.tree.data.layout.kind(node)
    }

    pub fn last_ungrouped_layout(&self, node: NodeId) -> LayoutKind {
        self.tree.data.layout.last_ungrouped_kind(node)
    }

    pub fn set_layout(&mut self, node: NodeId, kind: LayoutKind) {
        self.tree.data.layout.set_kind(node, kind);
    }

    pub fn nest_in_container(&mut self, node: NodeId, kind: LayoutKind) -> NodeId {
        let old_parent = node.parent(&self.tree.map);
        let parent = if node.prev_sibling(&self.tree.map).is_none()
            && node.next_sibling(&self.tree.map).is_none()
            && old_parent.is_some()
        {
            old_parent.unwrap()
        } else {
            let new_parent = if let Some(old_parent) = old_parent {
                let is_selection =
                    self.tree.data.selection.local_selection(self.map(), old_parent) == Some(node);
                let new_parent = self.tree.mk_node().insert_before(node);
                self.tree.data.layout.assume_size_of(new_parent, node, &self.tree.map);
                node.detach(&mut self.tree).push_back(new_parent);
                if is_selection {
                    self.tree.data.selection.select_locally(&self.tree.map, new_parent);
                }
                new_parent
            } else {
                // New root.
                let space = self.root_spaces[&node];
                let space_root = self.space_roots.get_mut(&space).unwrap();
                self.root_spaces.remove(&space_root.id());
                space_root.replace(self.tree.mk_node()).push_back(space_root.id());
                self.root_spaces.insert(space_root.id(), space);
                space_root.id()
            };
            self.tree.data.selection.select_locally(&self.tree.map, node);
            new_parent
        };
        self.tree.data.layout.set_kind(parent, kind);
        parent
    }

    pub fn resize(&mut self, node: NodeId, screen_ratio: f64, direction: Direction) -> bool {
        // Pick an ancestor to resize that has a sibling in the given direction.
        let can_resize = |&node: &NodeId| -> bool {
            let Some(parent) = node.parent(&self.tree.map) else {
                return false;
            };
            !self.tree.data.layout.kind(parent).is_group()
                && self.move_over(node, direction).is_some()
        };
        let Some(resizing_node) = node.ancestors(&self.tree.map).filter(can_resize).next() else {
            return false;
        };
        let sibling = self.move_over(resizing_node, direction).unwrap();

        // Compute the share of resizing_node's parent that needs to be taken
        // from the sibling.
        let exchange_rate = resizing_node.ancestors(&self.tree.map).skip(1).fold(1.0, |r, node| {
            match node.parent(&self.tree.map) {
                Some(parent)
                    if self.tree.data.layout.kind(parent).orientation()
                        == direction.orientation()
                        && !self.tree.data.layout.kind(parent).is_group() =>
                {
                    r * self.tree.data.layout.proportion(&self.tree.map, node).unwrap()
                }
                _ => r,
            }
        });
        let local_ratio = f64::from(screen_ratio)
            * self.tree.data.layout.total(resizing_node.parent(&self.tree.map).unwrap())
            / exchange_rate;
        self.tree.data.layout.take_share(
            &self.tree.map,
            resizing_node,
            sibling,
            local_ratio as f32,
        );

        true
    }

    /// Call this during a user resize to have the model respond appropriately.
    ///
    /// Only two edges are allowed to change at a time; otherwise, this function
    /// will panic.
    pub fn set_frame_from_resize(
        &mut self,
        node: NodeId,
        old_frame: CGRect,
        new_frame: CGRect,
        screen: CGRect,
    ) {
        let mut count = 0;
        let mut check_and_resize = |direction, delta, whole| {
            if delta != 0.0 {
                count += 1;
                self.resize(node, f64::from(delta) / f64::from(whole), direction);
            }
        };
        check_and_resize(
            Direction::Left,
            old_frame.min().x - new_frame.min().x,
            screen.size.width,
        );
        check_and_resize(
            Direction::Right,
            new_frame.max().x - old_frame.max().x,
            screen.size.width,
        );
        check_and_resize(
            Direction::Up,
            old_frame.min().y - new_frame.min().y,
            screen.size.height,
        );
        check_and_resize(
            Direction::Down,
            new_frame.max().y - old_frame.max().y,
            screen.size.height,
        );
        if count > 2 {
            panic!(
                "Only resizing in 2 directions is supported, but was asked \
                to resize from {old_frame:?} to {new_frame:?}"
            );
        }
    }

    pub fn print_tree(&self, root: NodeId) {
        print!("{}", self.draw_tree(root))
    }

    pub fn draw_tree(&self, root: NodeId) -> String {
        let tree = self.get_ascii_tree(root);
        let mut out = String::new();
        ascii_tree::write_tree(&mut out, &tree).unwrap();
        out
    }

    fn get_ascii_tree(&self, node: NodeId) -> ascii_tree::Tree {
        let status = match node.parent(&self.tree.map) {
            None => "", // Root
            Some(parent)
                if self.tree.data.selection.local_selection(&self.tree.map, parent)
                    == Some(node) =>
            {
                "☒ "
            }
            _ => "☐ ",
        };
        let desc = format!("{status}{node:?}",);
        let desc = match self.windows.get(node) {
            Some(wid) => format!(
                "{desc} {wid:?} {}",
                self.tree.data.layout.debug(node, false)
            ),
            None => format!("{desc} {}", self.tree.data.layout.debug(node, true)),
        };
        let children: Vec<_> =
            node.children(&self.tree.map).map(|c| self.get_ascii_tree(c)).collect();
        if children.is_empty() {
            ascii_tree::Tree::Leaf(vec![desc])
        } else {
            ascii_tree::Tree::Node(desc, children)
        }
    }
}

impl Drop for LayoutTree {
    fn drop(&mut self) {
        // It's okay to skip removing these, since we're dropping the map too.
        mem::forget(self.space_roots.drain());
    }
}

impl Components {
    fn dispatch_event(&mut self, map: &NodeMap, event: TreeEvent) {
        self.selection.handle_event(map, event);
        self.layout.handle_event(map, event);
    }
}

impl tree::Observer for Components {
    fn added_to_forest(&mut self, map: &NodeMap, node: NodeId) {
        self.dispatch_event(map, TreeEvent::AddedToForest(node))
    }

    fn added_to_parent(&mut self, map: &NodeMap, node: NodeId) {
        self.dispatch_event(map, TreeEvent::AddedToParent(node))
    }

    fn removing_from_parent(&mut self, map: &NodeMap, node: NodeId) {
        self.dispatch_event(map, TreeEvent::RemovingFromParent(node))
    }

    fn removed_child(tree: &mut Tree<Self>, parent: NodeId) {
        // parent must be a container, or it wouldn't have had a child in the first place.
        // Cull it if it's empty.
        // Don't cull the root node, which would require extra bookkeeping.
        if parent.is_empty(&tree.map) && parent.parent(&tree.map).is_some() {
            parent.detach(tree).remove()
        }
    }

    fn removed_from_forest(&mut self, map: &NodeMap, node: NodeId) {
        self.dispatch_event(map, TreeEvent::RemovedFromForest(node))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use icrate::Foundation::{CGPoint, CGSize};
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::{model::LayoutTree, screen::SpaceId};

    #[test]
    fn traverse() {
        let mut tree = LayoutTree::new();
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

    #[test]
    fn traverse_nested_same_orientation() {
        let mut tree = LayoutTree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let b1 = tree.add_window(a2, WindowId::new(2, 1));
        let b2 = tree.add_window(a2, WindowId::new(2, 2));
        let b3 = tree.add_window(a2, WindowId::new(2, 3));
        let a3 = tree.add_window(root, WindowId::new(1, 3));
        tree.select(b2);

        use Direction::*;
        assert_eq!(tree.traverse(a1, Left), None);
        assert_eq!(tree.traverse(a2, Left), Some(a1));
        assert_eq!(tree.traverse(b1, Left), Some(a1));
        assert_eq!(tree.traverse(b2, Left), Some(b1));
        assert_eq!(tree.traverse(b2, Left), Some(b1));
        assert_eq!(tree.traverse(b3, Left), Some(b2));
        assert_eq!(tree.traverse(a3, Left), Some(b3));
        assert_eq!(tree.traverse(a1, Right), Some(b1));
        assert_eq!(tree.traverse(a2, Right), Some(a3));
        assert_eq!(tree.traverse(b1, Right), Some(b2));
        assert_eq!(tree.traverse(b2, Right), Some(b3));
        assert_eq!(tree.traverse(b3, Right), Some(a3));
        assert_eq!(tree.traverse(a3, Right), None);
    }

    impl LayoutTree {
        #[track_caller]
        fn assert_children_are<const N: usize>(&self, children: [NodeId; N], parent: NodeId) {
            let actual: Vec<_> = parent.children(&self.tree.map).collect();
            assert_eq!(&children, actual.as_slice());
        }
    }

    #[test]
    fn move_node() {
        let mut tree = LayoutTree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Vertical);
        let b1 = tree.add_window(a2, WindowId::new(2, 1));
        let b2 = tree.add_window(a2, WindowId::new(2, 2));
        let b3 = tree.add_window(a2, WindowId::new(2, 3));
        let a3 = tree.add_window(root, WindowId::new(1, 3));
        tree.select(b2);
        tree.assert_children_are([a1, a2, a3], root);
        assert_eq!(Some(b2), tree.selection(root));

        tree.move_node(b2, Direction::Left);
        tree.assert_children_are([a1, b2, a2, a3], root);
        assert_eq!(Some(b2), tree.selection(root));

        tree.move_node(b2, Direction::Left);
        tree.assert_children_are([b2, a1, a2, a3], root);
        assert_eq!(Some(b2), tree.selection(root));

        tree.move_node(a2, Direction::Left);
        tree.assert_children_are([b2, a2, a1, a3], root);
        assert_eq!(Some(b2), tree.selection(root));

        tree.select(a3);
        tree.move_node(a3, Direction::Left);
        tree.assert_children_are([b2, a2, a3, a1], root);
        assert_eq!(Some(a3), tree.selection(root));

        tree.move_node(a3, Direction::Left);
        tree.assert_children_are([b2, a2, a1], root);
        tree.assert_children_are([b1, b3, a3], a2);
        assert_eq!(Some(a3), tree.selection(root));

        tree.move_node(a3, Direction::Right);
        tree.assert_children_are([b2, a2, a3, a1], root);
        tree.assert_children_are([b1, b3], a2);
        assert_eq!(Some(a3), tree.selection(root));

        tree.move_node(b1, Direction::Down);
        tree.assert_children_are([b3, b1], a2);
        assert_eq!(Some(a3), tree.selection(root));

        tree.move_node(b1, Direction::Up);
        tree.assert_children_are([b1, b3], a2);
        assert_eq!(Some(a3), tree.selection(root));

        tree.move_node(b1, Direction::Up);
        let (old_root, root) = (root, tree.space(space));
        tree.assert_children_are([b1, old_root], root);
        tree.assert_children_are([b2, a2, a3, a1], old_root);
        assert_eq!(LayoutKind::Vertical, tree.layout(root));
        assert_eq!(Some(a3), tree.selection(root));
        assert_eq!(Some(b1), tree.window_node(space, WindowId::new(2, 1)));

        // a2 is culled when its last child moves out of it.
        tree.move_node(b3, Direction::Right);
        tree.assert_children_are([b2, b3, a3, a1], old_root);

        assert!(!tree.move_node(root, Direction::Right));
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
    fn nest_in_container() {
        let mut tree = LayoutTree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));

        // Calling on only child updates the (root) parent.
        assert_eq!(root, tree.nest_in_container(a1, LayoutKind::Vertical));
        assert_eq!(LayoutKind::Vertical, tree.tree.data.layout.kind(root));

        let a2 = tree.add_window(root, WindowId::new(1, 2));
        tree.resize(a2, 0.10, Direction::Up);
        let orig_frames = tree.calculate_layout(root, rect(0, 0, 1000, 1000));

        // Calling on child with siblings creates a new parent.
        // To keep the naming scheme consistent, rename the node a1 to b1
        // once it's nested a level deeper.
        tree.select(a1);
        let (b1, a1) = (a1, tree.nest_in_container(a1, LayoutKind::Horizontal));
        tree.assert_children_are([a1, a2], root);
        tree.assert_children_are([b1], a1);
        assert_eq!(Some(b1), tree.selection(root));

        tree.select(a2);
        let (b2, a2) = (a2, tree.nest_in_container(a2, LayoutKind::Horizontal));
        assert_eq!(Some(b2), tree.selection(root));
        tree.assert_children_are([a1, a2], root);
        tree.assert_children_are([b2], a2);
        assert_frames_are(
            orig_frames,
            tree.calculate_layout(root, rect(0, 0, 1000, 1000)),
        );
        assert_eq!(Some(b2), tree.selection(root));

        // Calling on only child updates the (non-root) parent.
        assert_eq!(a2, tree.nest_in_container(b2, LayoutKind::Horizontal));
        tree.assert_children_are([a1, a2], root);
        tree.assert_children_are([b2], a2);
        assert_eq!(Some(b2), tree.selection(root));

        // Calling on root works too.
        let (old_root, root) = (root, tree.nest_in_container(root, LayoutKind::Vertical));
        tree.assert_children_are([old_root], root);
        tree.assert_children_are([a1, a2], old_root);
        assert_eq!(Some(b2), tree.selection(root));

        let a3 = tree.add_window(old_root, WindowId::new(1, 3));
        tree.assert_children_are([a1, a2, a3], old_root);
        assert_eq!(Some(b2), tree.selection(root));
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
        let mut tree = LayoutTree::new();
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

        let orig = vec![
            (WindowId::new(1, 1), rect(0, 0, 1000, 3000)),
            (WindowId::new(2, 1), rect(1000, 0, 1000, 1000)),
            (WindowId::new(3, 1), rect(1000, 1000, 500, 1000)),
            (WindowId::new(3, 2), rect(1500, 1000, 500, 1000)),
            (WindowId::new(2, 3), rect(1000, 2000, 1000, 1000)),
            (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
        ];
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        // We may want to have a mode that adjusts sizes so that only the
        // requested edge is resized. Notice that the width is redistributed
        // between c1 and c2 here.
        tree.resize(c2, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(root, screen),
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
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.resize(c2, 0.01, Direction::Left);
        assert_frames_are(
            tree.calculate_layout(root, screen),
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
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.resize(b2, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(root, screen),
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
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.resize(a1, 0.01, Direction::Right);
        assert_frames_are(
            tree.calculate_layout(root, screen),
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
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.resize(a1, 0.01, Direction::Left);
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());
        tree.resize(a1, -0.01, Direction::Left);
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());
    }

    #[test]
    fn set_frame_from_resize() {
        // ┌─────┬─────┬─────┐
        // │     │ b1  │     │
        // │     +─────+     │
        // │ a1  │c1│c2│  a3 │
        // │     +─────+     │
        // │     │ b3  │     │
        // └─────┴─────┴─────┘
        let mut tree = LayoutTree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Vertical);
        let _b1 = tree.add_window(a2, WindowId::new(2, 1));
        let b2 = tree.add_container(a2, LayoutKind::Horizontal);
        let c1 = tree.add_window(b2, WindowId::new(3, 1));
        let _c2 = tree.add_window(b2, WindowId::new(3, 2));
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
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.set_frame_from_resize(a1, rect(0, 0, 1000, 3000), rect(0, 0, 1010, 3000), screen);
        assert_frames_are(
            tree.calculate_layout(root, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 1010, 3000)),
                (WindowId::new(2, 1), rect(1010, 0, 990, 1000)),
                (WindowId::new(3, 1), rect(1010, 1000, 495, 1000)),
                (WindowId::new(3, 2), rect(1505, 1000, 495, 1000)),
                (WindowId::new(2, 3), rect(1010, 2000, 990, 1000)),
                (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
            ],
        );

        tree.set_frame_from_resize(a1, rect(0, 0, 1010, 3000), rect(0, 0, 1000, 3000), screen);
        assert_frames_are(tree.calculate_layout(root, screen), orig.clone());

        tree.set_frame_from_resize(
            c1,
            rect(1000, 1000, 500, 1000),
            rect(900, 900, 600, 1100),
            screen,
        );
        assert_frames_are(
            tree.calculate_layout(root, screen),
            [
                (WindowId::new(1, 1), rect(0, 0, 900, 3000)),
                (WindowId::new(2, 1), rect(900, 0, 1100, 900)),
                // This may not be what we actually want; notice the width
                // increase is redistributed across c1 and c2. In any case it's
                // confusing to have something called set_frame that results in
                // a different frame than requested..
                (WindowId::new(3, 1), rect(900, 900, 550, 1100)),
                (WindowId::new(3, 2), rect(1450, 900, 550, 1100)),
                (WindowId::new(2, 3), rect(900, 2000, 1100, 1000)),
                (WindowId::new(1, 3), rect(2000, 0, 1000, 3000)),
            ],
        );
    }
}
