#![allow(dead_code)]
use std::ops::{Deref, DerefMut};

use slotmap::SlotMap;

/// Core data structure that holds tree structures.
///
/// Multiple trees can be contained within a forest. This also makes it easier
/// to move branches between trees.
///
/// This type should not be used directly; instead, use the methods on
/// [`OwnedNode`] and [`NodeId`].
pub type Forest = SlotMap<NodeId, Node>;

/// Represents ownership of a particular node in a tree.
///
/// Nodes must be removed manually, because removal requires a reference to the
/// [`Forest`].  If a value of this type is dropped without
/// [`OwnedNode::remove`] being called, it will panic.
///
/// Every `OwnedNode` has a name which will be used in the panic message.
#[must_use]
pub struct OwnedNode(Option<NodeId>, &'static str);

impl OwnedNode {
    /// Creates a new root node.
    pub fn new_root_in(map: &mut Forest, name: &'static str) -> Self {
        Self::own(map.insert(Node::default()), name)
    }

    /// Marks a non-root node as owned.
    pub fn own(node: NodeId, name: &'static str) -> Self {
        OwnedNode(Some(node), name)
    }

    pub fn id(&self) -> NodeId {
        self.0.unwrap()
    }

    pub fn is_removed(&self) -> bool {
        self.0.is_none()
    }

    #[track_caller]
    pub fn remove(&mut self, map: &mut Forest, obs: &mut impl Observer) {
        self.0.take().unwrap().remove(map, obs)
    }
}

impl Deref for OwnedNode {
    type Target = NodeId;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl DerefMut for OwnedNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

impl Drop for OwnedNode {
    fn drop(&mut self) {
        if let Some(node) = self.0 {
            panic!(
                "OwnedNode {name:?} dropped without being removed: {node:?}",
                name = self.1,
            );
        }
    }
}

slotmap::new_key_type! {
    /// Represents a node somewhere in the tree.
    pub struct NodeId;
}

impl NodeId {
    #[track_caller]
    pub fn parent(self, map: &Forest) -> Option<NodeId> {
        map[self].parent
    }

    #[track_caller]
    pub fn children(self, map: &Forest) -> impl Iterator<Item = NodeId> + '_ {
        NodeIterator {
            cur: map[self].first_child,
            map,
        }
    }

    #[track_caller]
    pub fn children_rev(self, map: &Forest) -> impl Iterator<Item = NodeId> + '_ {
        NodeRevIterator { cur: map[self].last_child, map }
    }

    #[track_caller]
    pub fn next_sibling(self, map: &Forest) -> Option<NodeId> {
        map[self].next_sibling
    }

    #[track_caller]
    pub fn prev_sibling(self, map: &Forest) -> Option<NodeId> {
        map[self].prev_sibling
    }

    #[track_caller]
    pub fn first_child(self, map: &Forest) -> Option<NodeId> {
        map[self].first_child
    }

    #[track_caller]
    pub fn last_child(self, map: &Forest) -> Option<NodeId> {
        map[self].last_child
    }
}

pub trait Observer {
    fn added_to_parent(&mut self, forest: &Forest, node: NodeId);
    fn removing_from_parent(&mut self, forest: &Forest, node: NodeId);
    fn removed_from_tree(&mut self, forest: &Forest, node: NodeId);
}

#[derive(Clone, Copy)]
pub struct NoopObserver;
impl Observer for NoopObserver {
    fn added_to_parent(&mut self, _forest: &Forest, _node: NodeId) {}
    fn removing_from_parent(&mut self, _forest: &Forest, _node: NodeId) {}
    fn removed_from_tree(&mut self, _forest: &Forest, _node: NodeId) {}
}
pub const NOOP: NoopObserver = NoopObserver;

impl NodeId {
    #[track_caller]
    pub(super) fn push_back(self, map: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = map.insert(Node::default());
        new.link_under_back(self, map);
        obs.added_to_parent(&map, new);
        new
    }

    #[track_caller]
    pub(super) fn push_front(self, map: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = map.insert(Node::default());
        new.link_under_front(self, map);
        obs.added_to_parent(&map, new);
        new
    }

    #[track_caller]
    pub(super) fn insert_before(self, map: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = map.insert(Node::default());
        new.link_before(self, map);
        obs.added_to_parent(&map, new);
        new
    }

    #[track_caller]
    pub(super) fn insert_after(self, map: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = map.insert(Node::default());
        new.link_after(self, map);
        obs.added_to_parent(&map, new);
        new
    }

    #[track_caller]
    pub(super) fn remove(self, map: &mut Forest, obs: &mut impl Observer) {
        obs.removing_from_parent(&map, self);
        map.remove(self).unwrap().unlink(self, map).delete_recursive(map, obs);
    }
}

#[derive(Clone, Default, PartialEq, Debug)]
pub struct Node {
    parent: Option<NodeId>,
    prev_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    first_child: Option<NodeId>,
    last_child: Option<NodeId>,
}

impl NodeId {
    fn link_under_back(self, parent: NodeId, map: &mut Forest) {
        debug_assert_eq!(map[self], Node::default());
        map[self].parent = Some(parent);
        map[parent].first_child.get_or_insert(self);
        if let Some(prev) = map[parent].last_child.replace(self) {
            self.hlink_after(prev, map);
        }
    }

    fn link_under_front(self, parent: NodeId, map: &mut Forest) {
        debug_assert_eq!(map[self], Node::default());
        map[self].parent = Some(parent);
        map[parent].last_child.get_or_insert(self);
        if let Some(next) = map[parent].first_child.replace(self) {
            self.hlink_before(next, map);
        }
    }

    #[track_caller]
    fn link_before(self, next: NodeId, map: &mut Forest) {
        let parent = map[next].parent.expect("cannot make a sibling of the root node");
        map[self].parent.replace(parent);
        debug_assert!(map[parent].first_child.is_some());
        if map[parent].first_child == Some(next) {
            map[parent].first_child.replace(self);
        }
        self.hlink_before(next, map);
    }

    #[track_caller]
    fn link_after(self, prev: NodeId, map: &mut Forest) {
        debug_assert_eq!(map[self].parent, None);
        let parent = map[prev].parent.expect("cannot make a sibling of the root node");
        map[self].parent.replace(parent);
        debug_assert!(map[parent].last_child.is_some());
        if map[parent].last_child == Some(prev) {
            map[parent].last_child.replace(self);
        }
        self.hlink_after(prev, map);
    }

    fn hlink_after(self, prev: NodeId, map: &mut Forest) {
        debug_assert_ne!(self, prev);
        debug_assert_eq!(map[self].prev_sibling, None);
        map[self].prev_sibling.replace(prev);
        let next = map[prev].next_sibling.replace(self);
        if let Some(next) = next {
            map[next].prev_sibling.replace(self);
            map[self].next_sibling.replace(next);
        }
    }

    fn hlink_before(self, next: NodeId, map: &mut Forest) {
        debug_assert_ne!(self, next);
        debug_assert_eq!(map[self].next_sibling, None);
        map[self].next_sibling.replace(next);
        let prev = map[next].prev_sibling.replace(self);
        if let Some(prev) = prev {
            map[prev].next_sibling.replace(self);
            map[self].prev_sibling.replace(prev);
        }
    }
}

impl Node {
    #[must_use]
    #[track_caller]
    fn unlink(self, id: NodeId, map: &mut Forest) -> Self {
        if let Some(prev) = self.prev_sibling {
            map[prev].next_sibling = self.next_sibling;
        }
        if let Some(next) = self.next_sibling {
            map[next].prev_sibling = self.prev_sibling;
        }
        if let Some(parent) = self.parent {
            if Some(id) == map[parent].first_child {
                map[parent].first_child = self.next_sibling;
            }
            if Some(id) == map[parent].last_child {
                map[parent].last_child = self.prev_sibling;
            }
        }
        self
    }

    #[track_caller]
    fn delete_recursive(&self, map: &mut Forest, obs: &mut impl Observer) {
        let mut iter = self.first_child;
        while let Some(child) = iter {
            let node = map.remove(child).unwrap();
            obs.removed_from_tree(&map, child);
            node.delete_recursive(map, obs);
            iter = node.next_sibling;
        }
    }
}

struct NodeIterator<'a> {
    cur: Option<NodeId>,
    map: &'a Forest,
}

impl<'a> Iterator for NodeIterator<'a> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        let Some(id) = self.cur else { return None };
        self.cur = self.map[id].next_sibling;
        Some(id)
    }
}

struct NodeRevIterator<'a> {
    cur: Option<NodeId>,
    map: &'a Forest,
}

impl<'a> Iterator for NodeRevIterator<'a> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        let Some(id) = self.cur else { return None };
        self.cur = self.map[id].prev_sibling;
        Some(id)
    }
}

#[allow(const_item_mutation)]
#[cfg(test)]
mod tests {
    use super::*;

    /// A tree with the following structure:
    /// ```text
    ///         [tree]              [other_tree]
    ///        __root__              other_root
    ///       /    |   \
    /// child1  child2  child3
    ///            |
    ///           gc1
    /// ```
    struct TestTree {
        map: Forest,
        tree: OwnedNode,
        root: NodeId,
        child1: NodeId,
        child2: NodeId,
        child3: NodeId,
        gc1: NodeId,
        other_tree: OwnedNode,
        other_root: NodeId,
    }

    impl Drop for TestTree {
        fn drop(&mut self) {
            if !self.tree.is_removed() {
                self.tree.remove(&mut self.map, &mut NOOP);
            }
            if !self.other_tree.is_removed() {
                self.other_tree.remove(&mut self.map, &mut NOOP);
            }
        }
    }

    impl TestTree {
        #[rustfmt::skip]
        fn new() -> Self {
            let mut map = Forest::default();
            let m = &mut map;

            let tree = OwnedNode::new_root_in(m, "tree");
            let root = tree.id();
            let child1 = root.push_back(m, &mut NOOP);
            let child2 = root.push_back(m, &mut NOOP);
            let child3 = root.push_back(m, &mut NOOP);

            let gc1 = child2.push_back(m, &mut NOOP);
            let other_tree = OwnedNode::new_root_in(m, "other_tree");
            let other_root = other_tree.id();

            TestTree {
                map, tree, root,
                child1, child2, child3, gc1,
                other_tree, other_root,
            }
        }

        fn get_children(&self, node: NodeId) -> Vec<NodeId> {
            node.children(&self.map).collect()
        }

        fn get_children_rev(&self, node: NodeId) -> Vec<NodeId> {
            node.children_rev(&self.map).collect()
        }

        #[track_caller]
        fn assert_children_are<const N: usize>(&self, children: [NodeId; N], parent: NodeId) {
            self.assert_children_are_inner(&children, parent);
        }

        #[track_caller]
        fn assert_children_are_inner(&self, children: &[NodeId], parent: NodeId) {
            assert_eq!(
                children,
                self.get_children(parent),
                "children did not match"
            );
            assert_eq!(
                children.iter().copied().rev().collect::<Vec<_>>(),
                self.get_children_rev(parent),
                "reverse children did not match"
            );
            for child in self.get_children(parent) {
                assert_eq!(
                    self.map[child].parent,
                    Some(parent),
                    "child has incorrect parent"
                );
            }
        }
    }

    #[test]
    fn iterator() {
        let t = TestTree::new();
        assert_eq!([t.child1, t.child2, t.child3], *t.get_children(t.root));
        assert!(t.get_children(t.child1).is_empty());
        assert_eq!([t.gc1], *t.get_children(t.child2));
        assert!(t.get_children(t.gc1).is_empty());
        assert!(t.get_children(t.child3).is_empty());
        assert!(t.get_children(t.other_root).is_empty());
    }

    #[test]
    fn rev_iterator() {
        let t = TestTree::new();
        assert_eq!([t.child3, t.child2, t.child1], *t.get_children_rev(t.root));
        assert!(t.get_children_rev(t.child1).is_empty());
        assert_eq!([t.gc1], *t.get_children_rev(t.child2));
        assert!(t.get_children_rev(t.gc1).is_empty());
        assert!(t.get_children_rev(t.child3).is_empty());
        assert!(t.get_children_rev(t.other_root).is_empty());
    }

    #[test]
    fn push_front() {
        let mut t = TestTree::new();
        let child0 = t.root.push_front(&mut t.map, &mut NOOP);
        let gc0 = t.child2.push_front(&mut t.map, &mut NOOP);
        let gc2 = t.child3.push_front(&mut t.map, &mut NOOP);
        t.assert_children_are([child0, t.child1, t.child2, t.child3], t.root);
        t.assert_children_are([], t.child1);
        t.assert_children_are([gc0, t.gc1], t.child2);
        t.assert_children_are([], gc2);
        t.assert_children_are([gc2], t.child3);
        t.assert_children_are([], t.other_root);
    }

    #[test]
    fn push_back() {
        let mut t = TestTree::new();
        let child4 = t.root.push_back(&mut t.map, &mut NOOP);
        let gc0 = t.child1.push_back(&mut t.map, &mut NOOP);
        let gc2 = t.child2.push_back(&mut t.map, &mut NOOP);
        t.assert_children_are([t.child1, t.child2, t.child3, child4], t.root);
        t.assert_children_are([gc0], t.child1);
        t.assert_children_are([t.gc1, gc2], t.child2);
        t.assert_children_are([], gc2);
        t.assert_children_are([], t.child3);
        t.assert_children_are([], t.other_root);
    }

    #[test]
    fn insert_before() {
        let mut t = TestTree::new();
        let child0 = t.child1.insert_before(&mut t.map, &mut NOOP);
        let child1_5 = t.child2.insert_before(&mut t.map, &mut NOOP);
        let child2_5 = t.child3.insert_before(&mut t.map, &mut NOOP);
        let gc0 = t.gc1.insert_before(&mut t.map, &mut NOOP);
        t.assert_children_are(
            [child0, t.child1, child1_5, t.child2, child2_5, t.child3],
            t.root,
        );
        t.assert_children_are([], child0);
        t.assert_children_are([], t.child1);
        t.assert_children_are([], child1_5);
        t.assert_children_are([gc0, t.gc1], t.child2);
        t.assert_children_are([], child2_5);
        t.assert_children_are([], t.child3);
        t.assert_children_are([], t.other_root);
    }

    #[test]
    fn insert_after() {
        let mut t = TestTree::new();
        let child1_5 = t.child1.insert_after(&mut t.map, &mut NOOP);
        let child2_5 = t.child2.insert_after(&mut t.map, &mut NOOP);
        let child4 = t.child3.insert_after(&mut t.map, &mut NOOP);
        let gc2 = t.gc1.insert_after(&mut t.map, &mut NOOP);
        t.assert_children_are(
            [t.child1, child1_5, t.child2, child2_5, t.child3, child4],
            t.root,
        );
        t.assert_children_are([], t.child1);
        t.assert_children_are([], child1_5);
        t.assert_children_are([t.gc1, gc2], t.child2);
        t.assert_children_are([], child2_5);
        t.assert_children_are([], t.child3);
        t.assert_children_are([], child4);
        t.assert_children_are([], t.other_root);
    }

    #[test]
    fn remove() {
        let mut t = TestTree::new();

        t.child2.remove(&mut t.map, &mut NOOP);
        t.assert_children_are([t.child1, t.child3], t.root);
        assert!(!t.map.contains_key(t.child2));
        assert!(!t.map.contains_key(t.gc1));

        t.child3.remove(&mut t.map, &mut NOOP);
        t.assert_children_are([t.child1], t.root);
        assert!(!t.map.contains_key(t.child3));

        t.child1.remove(&mut t.map, &mut NOOP);
        t.assert_children_are([], t.root);
        assert!(!t.map.contains_key(t.child1));

        assert!(t.map.contains_key(t.root));
        assert!(t.map.contains_key(t.other_root));
        t.tree.remove(&mut t.map, &mut NOOP);
        assert!(!t.map.contains_key(t.root));
        t.other_tree.remove(&mut t.map, &mut NOOP);
        assert!(!t.map.contains_key(t.other_root));
    }
}
