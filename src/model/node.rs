#![allow(dead_code)]
use std::ops::{Deref, DerefMut, Index, IndexMut};

use slotmap::SlotMap;

/// Core data structure that holds tree structures.
///
/// Multiple trees can be contained within a forest. This also makes it easier
/// to move branches between trees.
///
/// This type should not be used directly; instead, use the methods on
/// [`OwnedNode`] and [`NodeId`].
pub struct Forest {
    map: SlotMap<NodeId, Node>,
}

impl Forest {
    pub fn new() -> Forest {
        Forest { map: SlotMap::default() }
    }

    #[must_use]
    fn mk_node(&mut self) -> NodeId {
        self.map.insert(Node::default())
    }

    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional)
    }
}

impl Index<NodeId> for Forest {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.map[index]
    }
}

impl IndexMut<NodeId> for Forest {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.map[index]
    }
}

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
    pub fn new_root_in(forest: &mut Forest, name: &'static str, obs: &mut impl Observer) -> Self {
        let node = forest.mk_node();
        obs.added_to_forest(forest, node);
        Self::own(node, name)
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
    pub fn remove(&mut self, forest: &mut Forest, obs: &mut impl Observer) {
        self.0.take().unwrap().remove(forest, obs)
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
    pub fn parent(self, forest: &Forest) -> Option<NodeId> {
        forest[self].parent
    }

    #[track_caller]
    pub fn children(self, forest: &Forest) -> impl Iterator<Item = NodeId> + '_ {
        NodeIterator {
            cur: forest[self].first_child,
            forest,
        }
    }

    #[track_caller]
    pub fn children_rev(self, forest: &Forest) -> impl Iterator<Item = NodeId> + '_ {
        NodeRevIterator {
            cur: forest[self].last_child,
            forest,
        }
    }

    /// Returns an iterator over all ancestors of the current node, including itself.
    #[track_caller]
    pub fn ancestors(self, forest: &Forest) -> impl Iterator<Item = NodeId> + '_ {
        let mut next = Some(self);
        std::iter::from_fn(move || {
            let node = next;
            next = next.and_then(|n| forest[n].parent);
            node
        })
    }

    /// Returns an iterator over all ancestors of the current node, including itself.
    #[track_caller]
    pub fn ancestors_with_parent(
        self,
        forest: &Forest,
    ) -> impl Iterator<Item = (NodeId, Option<NodeId>)> + '_ {
        let mut next = Some(self);
        std::iter::from_fn(move || {
            let node = next;
            next = next.and_then(|n| forest[n].parent);
            node.map(|n| (n, next))
        })
    }

    #[track_caller]
    pub fn next_sibling(self, forest: &Forest) -> Option<NodeId> {
        forest[self].next_sibling
    }

    #[track_caller]
    pub fn prev_sibling(self, forest: &Forest) -> Option<NodeId> {
        forest[self].prev_sibling
    }

    #[track_caller]
    pub fn first_child(self, forest: &Forest) -> Option<NodeId> {
        forest[self].first_child
    }

    #[track_caller]
    pub fn last_child(self, forest: &Forest) -> Option<NodeId> {
        forest[self].last_child
    }
}

pub trait Observer {
    fn added_to_forest(&mut self, forest: &Forest, node: NodeId);
    fn added_to_parent(&mut self, forest: &Forest, node: NodeId);
    fn removing_from_parent(&mut self, forest: &Forest, node: NodeId);
    fn removed_from_forest(&mut self, forest: &Forest, node: NodeId);
}

#[derive(Clone, Copy)]
pub struct NoopObserver;
impl Observer for NoopObserver {
    fn added_to_forest(&mut self, _forest: &Forest, _node: NodeId) {}
    fn added_to_parent(&mut self, _forest: &Forest, _node: NodeId) {}
    fn removing_from_parent(&mut self, _forest: &Forest, _node: NodeId) {}
    fn removed_from_forest(&mut self, _forest: &Forest, _node: NodeId) {}
}
pub const NOOP: NoopObserver = NoopObserver;

impl NodeId {
    #[track_caller]
    pub(super) fn push_back(self, forest: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = forest.mk_node();
        obs.added_to_forest(&forest, new);
        new.link_under_back(self, forest);
        obs.added_to_parent(&forest, new);
        new
    }

    #[track_caller]
    pub(super) fn push_front(self, forest: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = forest.mk_node();
        obs.added_to_forest(&forest, new);
        new.link_under_front(self, forest);
        obs.added_to_parent(&forest, new);
        new
    }

    #[track_caller]
    pub(super) fn insert_before(self, forest: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = forest.mk_node();
        obs.added_to_forest(&forest, new);
        new.link_before(self, forest);
        obs.added_to_parent(&forest, new);
        new
    }

    #[track_caller]
    pub(super) fn insert_after(self, forest: &mut Forest, obs: &mut impl Observer) -> NodeId {
        let new = forest.mk_node();
        obs.added_to_forest(&forest, new);
        new.link_after(self, forest);
        obs.added_to_parent(&forest, new);
        new
    }

    #[track_caller]
    pub(super) fn remove(self, forest: &mut Forest, obs: &mut impl Observer) {
        obs.removing_from_parent(&forest, self);
        forest
            .map
            .remove(self)
            .unwrap()
            .unlink(self, forest)
            .delete_recursive(forest, obs);
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
    fn link_under_back(self, parent: NodeId, forest: &mut Forest) {
        debug_assert_eq!(forest[self], Node::default());
        forest[self].parent = Some(parent);
        forest[parent].first_child.get_or_insert(self);
        if let Some(prev) = forest[parent].last_child.replace(self) {
            self.hlink_after(prev, forest);
        }
    }

    fn link_under_front(self, parent: NodeId, forest: &mut Forest) {
        debug_assert_eq!(forest[self], Node::default());
        forest[self].parent = Some(parent);
        forest[parent].last_child.get_or_insert(self);
        if let Some(next) = forest[parent].first_child.replace(self) {
            self.hlink_before(next, forest);
        }
    }

    #[track_caller]
    fn link_before(self, next: NodeId, forest: &mut Forest) {
        let parent = forest[next].parent.expect("cannot make a sibling of the root node");
        forest[self].parent.replace(parent);
        debug_assert!(forest[parent].first_child.is_some());
        if forest[parent].first_child == Some(next) {
            forest[parent].first_child.replace(self);
        }
        self.hlink_before(next, forest);
    }

    #[track_caller]
    fn link_after(self, prev: NodeId, forest: &mut Forest) {
        debug_assert_eq!(forest[self].parent, None);
        let parent = forest[prev].parent.expect("cannot make a sibling of the root node");
        forest[self].parent.replace(parent);
        debug_assert!(forest[parent].last_child.is_some());
        if forest[parent].last_child == Some(prev) {
            forest[parent].last_child.replace(self);
        }
        self.hlink_after(prev, forest);
    }

    fn hlink_after(self, prev: NodeId, forest: &mut Forest) {
        debug_assert_ne!(self, prev);
        debug_assert_eq!(forest[self].prev_sibling, None);
        forest[self].prev_sibling.replace(prev);
        let next = forest[prev].next_sibling.replace(self);
        if let Some(next) = next {
            forest[next].prev_sibling.replace(self);
            forest[self].next_sibling.replace(next);
        }
    }

    fn hlink_before(self, next: NodeId, forest: &mut Forest) {
        debug_assert_ne!(self, next);
        debug_assert_eq!(forest[self].next_sibling, None);
        forest[self].next_sibling.replace(next);
        let prev = forest[next].prev_sibling.replace(self);
        if let Some(prev) = prev {
            forest[prev].next_sibling.replace(self);
            forest[self].prev_sibling.replace(prev);
        }
    }
}

impl Node {
    #[must_use]
    #[track_caller]
    fn unlink(self, id: NodeId, forest: &mut Forest) -> Self {
        if let Some(prev) = self.prev_sibling {
            forest[prev].next_sibling = self.next_sibling;
        }
        if let Some(next) = self.next_sibling {
            forest[next].prev_sibling = self.prev_sibling;
        }
        if let Some(parent) = self.parent {
            if Some(id) == forest[parent].first_child {
                forest[parent].first_child = self.next_sibling;
            }
            if Some(id) == forest[parent].last_child {
                forest[parent].last_child = self.prev_sibling;
            }
        }
        self
    }

    #[track_caller]
    fn delete_recursive(&self, forest: &mut Forest, obs: &mut impl Observer) {
        let mut iter = self.first_child;
        while let Some(child) = iter {
            let node = forest.map.remove(child).unwrap();
            obs.removed_from_forest(&forest, child);
            node.delete_recursive(forest, obs);
            iter = node.next_sibling;
        }
    }
}

struct NodeIterator<'a> {
    cur: Option<NodeId>,
    forest: &'a Forest,
}

impl<'a> Iterator for NodeIterator<'a> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        let Some(id) = self.cur else { return None };
        self.cur = self.forest[id].next_sibling;
        Some(id)
    }
}

struct NodeRevIterator<'a> {
    cur: Option<NodeId>,
    forest: &'a Forest,
}

impl<'a> Iterator for NodeRevIterator<'a> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        let Some(id) = self.cur else { return None };
        self.cur = self.forest[id].prev_sibling;
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
        forest: Forest,
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
                self.tree.remove(&mut self.forest, &mut NOOP);
            }
            if !self.other_tree.is_removed() {
                self.other_tree.remove(&mut self.forest, &mut NOOP);
            }
        }
    }

    impl TestTree {
        #[rustfmt::skip]
        fn new() -> Self {
            let mut forest = Forest::new();
            let f = &mut forest;

            let tree = OwnedNode::new_root_in(f, "tree", &mut NOOP);
            let root = tree.id();
            let child1 = root.push_back(f, &mut NOOP);
            let child2 = root.push_back(f, &mut NOOP);
            let child3 = root.push_back(f, &mut NOOP);

            let gc1 = child2.push_back(f, &mut NOOP);
            let other_tree = OwnedNode::new_root_in(f, "other_tree", &mut NOOP);
            let other_root = other_tree.id();

            TestTree {
                forest, tree, root,
                child1, child2, child3, gc1,
                other_tree, other_root,
            }
        }

        fn get_children(&self, node: NodeId) -> Vec<NodeId> {
            node.children(&self.forest).collect()
        }

        fn get_children_rev(&self, node: NodeId) -> Vec<NodeId> {
            node.children_rev(&self.forest).collect()
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
                    self.forest[child].parent,
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
    fn ancestors() {
        let t = TestTree::new();
        let ancestors = |node: NodeId| node.ancestors(&t.forest).collect::<Vec<_>>();
        assert_eq!([t.child1, t.root], *ancestors(t.child1));
        assert_eq!([t.gc1, t.child2, t.root], *ancestors(t.gc1));
        assert_eq!([t.child2, t.root], *ancestors(t.child2));
        assert_eq!([t.root], *ancestors(t.root));
        assert_eq!([t.other_root], *ancestors(t.other_root));
    }

    #[test]
    fn push_front() {
        let mut t = TestTree::new();
        let child0 = t.root.push_front(&mut t.forest, &mut NOOP);
        let gc0 = t.child2.push_front(&mut t.forest, &mut NOOP);
        let gc2 = t.child3.push_front(&mut t.forest, &mut NOOP);
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
        let child4 = t.root.push_back(&mut t.forest, &mut NOOP);
        let gc0 = t.child1.push_back(&mut t.forest, &mut NOOP);
        let gc2 = t.child2.push_back(&mut t.forest, &mut NOOP);
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
        let child0 = t.child1.insert_before(&mut t.forest, &mut NOOP);
        let child1_5 = t.child2.insert_before(&mut t.forest, &mut NOOP);
        let child2_5 = t.child3.insert_before(&mut t.forest, &mut NOOP);
        let gc0 = t.gc1.insert_before(&mut t.forest, &mut NOOP);
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
        let child1_5 = t.child1.insert_after(&mut t.forest, &mut NOOP);
        let child2_5 = t.child2.insert_after(&mut t.forest, &mut NOOP);
        let child4 = t.child3.insert_after(&mut t.forest, &mut NOOP);
        let gc2 = t.gc1.insert_after(&mut t.forest, &mut NOOP);
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

        t.child2.remove(&mut t.forest, &mut NOOP);
        t.assert_children_are([t.child1, t.child3], t.root);
        assert!(!t.forest.map.contains_key(t.child2));
        assert!(!t.forest.map.contains_key(t.gc1));

        t.child3.remove(&mut t.forest, &mut NOOP);
        t.assert_children_are([t.child1], t.root);
        assert!(!t.forest.map.contains_key(t.child3));

        t.child1.remove(&mut t.forest, &mut NOOP);
        t.assert_children_are([], t.root);
        assert!(!t.forest.map.contains_key(t.child1));

        assert!(t.forest.map.contains_key(t.root));
        assert!(t.forest.map.contains_key(t.other_root));
        t.tree.remove(&mut t.forest, &mut NOOP);
        assert!(!t.forest.map.contains_key(t.root));
        t.other_tree.remove(&mut t.forest, &mut NOOP);
        assert!(!t.forest.map.contains_key(t.other_root));
    }
}
