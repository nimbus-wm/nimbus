use super::{
    node::{Forest, NodeId},
    tree::TreeEvent,
};

#[derive(Default)]
pub struct Selection {
    selected_child: slotmap::SecondaryMap<NodeId, Option<NodeId>>,
    current_selection: Option<NodeId>,
}

impl Selection {
    pub(super) fn current_selection(&self) -> Option<NodeId> {
        self.current_selection
    }

    pub(super) fn select(&mut self, forest: &Forest, mut selection: Option<NodeId>) {
        self.current_selection = selection;
        while let Some(node) = selection {
            let parent = node.parent(forest);
            if let Some(parent) = parent {
                self.selected_child.insert(parent, Some(node));
            }
            selection = parent;
        }
    }

    pub(super) fn handle_event(&mut self, forest: &Forest, event: TreeEvent) {
        use TreeEvent::*;
        match event {
            AddedWindow(node, _wid) => {
                self.select(forest, Some(node));
            }
            RemovingNode(node) => {
                let parent = node.parent(forest).unwrap();
                let alternative = node.next_sibling(forest).or(node.prev_sibling(forest));
                if self.selected_child[parent] == Some(node) {
                    self.selected_child[parent] = alternative;
                }
                if self.current_selection == Some(node) {
                    // TODO: This should "descend" the tree.
                    self.current_selection = alternative;
                }
            }
            RemovedNode(node) => {
                self.selected_child.remove(node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{app::WindowId, model::tree::Tree, screen::SpaceId};

    #[test]
    fn it_moves_as_nodes_are_added_and_removed() {
        let mut tree = Tree::new();
        let space = SpaceId::new(1);
        let n1 = tree.add_window(space, WindowId::new(1, 1));
        assert_eq!(tree.selection(), Some(n1));
        let n2 = tree.add_window(space, WindowId::new(1, 2));
        assert_eq!(tree.selection(), Some(n2));
        let n3 = tree.add_window(space, WindowId::new(1, 3));
        assert_eq!(tree.selection(), Some(n3));
        tree.select(n2);
        assert_eq!(tree.selection(), Some(n2));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 2));
        assert_eq!(tree.selection(), Some(n3));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 3));
        assert_eq!(tree.selection(), Some(n1));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 1));
        assert_eq!(tree.selection(), None);
    }
}
