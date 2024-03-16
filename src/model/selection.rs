use super::{
    layout_tree::TreeEvent,
    tree::{NodeId, NodeMap},
};

#[derive(Default)]
pub struct Selection {
    nodes: slotmap::SecondaryMap<NodeId, SelectionInfo>,
}

struct SelectionInfo {
    selected_child: NodeId,
    stop_here: bool,
}

impl Selection {
    pub(super) fn current_selection(&self, root: NodeId) -> NodeId {
        let mut node = root;
        while let Some(info) = self.nodes.get(node) {
            if info.stop_here {
                break;
            }
            node = info.selected_child;
        }
        node
    }

    pub(super) fn local_selection(&self, map: &NodeMap, node: NodeId) -> Option<NodeId> {
        let result = self.nodes.get(node);
        if let Some(result) = result {
            debug_assert_eq!(result.selected_child.parent(map), Some(node));
        }
        result.filter(|info| !info.stop_here).map(|info| info.selected_child)
    }

    pub(super) fn select_locally(&mut self, map: &NodeMap, node: NodeId) {
        if let Some(parent) = node.parent(map) {
            self.nodes.insert(
                parent,
                SelectionInfo {
                    selected_child: node,
                    stop_here: false,
                },
            );
        }
    }

    pub(super) fn select(&mut self, map: &NodeMap, selection: NodeId) {
        if let Some(info) = self.nodes.get_mut(selection) {
            info.stop_here = true;
        }
        let mut node = selection;
        while let Some(parent) = node.parent(map) {
            self.nodes.insert(
                parent,
                SelectionInfo {
                    selected_child: node,
                    stop_here: false,
                },
            );
            node = parent;
        }
    }

    pub(super) fn handle_event(&mut self, map: &NodeMap, event: TreeEvent) {
        use TreeEvent::*;
        match event {
            AddedToForest(_node) => {}
            AddedToParent(_node) => {}
            RemovingFromParent(node) => {
                let parent = node.parent(map).unwrap();
                if self.nodes.get(parent).map(|n| n.selected_child) == Some(node) {
                    if let Some(new_selection) = node.next_sibling(map).or(node.prev_sibling(map)) {
                        self.nodes[parent].selected_child = new_selection;
                    } else {
                        self.nodes.remove(parent);
                    }
                }
            }
            RemovedFromForest(node) => {
                self.nodes.remove(node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        app::WindowId,
        model::{layout::LayoutKind, layout_tree::LayoutTree, Direction},
        screen::SpaceId,
    };

    #[test]
    fn it_moves_as_nodes_are_added_and_removed() {
        let mut tree = LayoutTree::new();
        let root = tree.space(SpaceId::new(1));
        let n1 = tree.add_window(root, WindowId::new(1, 1));
        let n2 = tree.add_window(root, WindowId::new(1, 2));
        let n3 = tree.add_window(root, WindowId::new(1, 3));
        assert_eq!(tree.selection(root), Some(root));
        tree.select(n2);
        assert_eq!(tree.selection(root), Some(n2));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 2));
        assert_eq!(tree.selection(root), Some(n3));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 3));
        assert_eq!(tree.selection(root), Some(n1));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 1));
        assert_eq!(tree.selection(root), Some(root));
    }

    #[test]
    fn remembers_nested_paths() {
        let mut tree = LayoutTree::new();
        let root = tree.space(SpaceId::new(1));
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let _b1 = tree.add_window(a2, WindowId::new(1, 2));
        let b2 = tree.add_window(a2, WindowId::new(1, 3));
        let _b3 = tree.add_window(a2, WindowId::new(1, 4));
        let a3 = tree.add_window(root, WindowId::new(1, 5));

        tree.select(b2);
        assert_eq!(tree.selection(root), Some(b2));
        tree.select(a1);
        assert_eq!(tree.selection(root), Some(a1));
        tree.select(a3);
        assert_eq!(tree.selection(root), Some(a3));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 5));
        assert_eq!(tree.selection(root), Some(b2));
    }

    #[test]
    fn selects_parent_when_there_are_no_children() {
        let mut tree = LayoutTree::new();
        let root = tree.space(SpaceId::new(1));
        let _a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let _b1 = tree.add_window(a2, WindowId::new(2, 2));
        let b2 = tree.add_window(a2, WindowId::new(2, 3));
        let _b3 = tree.add_window(a2, WindowId::new(2, 4));
        let _a3 = tree.add_window(root, WindowId::new(1, 5));

        tree.select(b2);
        assert_eq!(tree.selection(root), Some(b2));
        tree.retain_windows(|&wid| wid.pid != 2);
        assert_eq!(tree.selection(root), Some(a2));
    }

    #[test]
    fn preserves_selection_after_move_within_parent() {
        let mut tree = LayoutTree::new();
        let root = tree.space(SpaceId::new(1));
        let _n1 = tree.add_window(root, WindowId::new(1, 1));
        let n2 = tree.add_window(root, WindowId::new(1, 2));
        let _n3 = tree.add_window(root, WindowId::new(1, 3));
        tree.select(n2);
        assert_eq!(tree.selection(root), Some(n2));
        tree.move_node(n2, Direction::Left);
        assert_eq!(tree.selection(root), Some(n2));
    }

    #[test]
    fn allows_parent_selection() {
        let mut tree = LayoutTree::new();
        let root = tree.space(SpaceId::new(1));
        let _a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let b1 = tree.add_window(a2, WindowId::new(1, 2));
        tree.select(b1);
        assert_eq!(tree.selection(root), Some(b1));
        tree.select(a2);
        assert_eq!(tree.selection(root), Some(a2));
    }
}
