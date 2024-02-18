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
            AddedToParent(node) => {
                self.select(forest, Some(node));
            }
            RemovingFromParent(node) => {
                let parent = node.parent(forest).unwrap();
                let alternative = node.next_sibling(forest).or(node.prev_sibling(forest));
                if self.selected_child[parent] == Some(node) {
                    self.selected_child[parent] = alternative;
                }
                if self.current_selection == Some(node) {
                    let mut new_selection = parent;
                    while let Some(selection) =
                        self.selected_child.get(new_selection).copied().flatten()
                    {
                        new_selection = selection;
                    }
                    self.current_selection = Some(new_selection);
                }
            }
            RemovedFromTree(node) => {
                self.selected_child.remove(node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        app::WindowId,
        model::{layout::LayoutKind, tree::Tree},
        screen::SpaceId,
    };

    #[test]
    fn it_moves_as_nodes_are_added_and_removed() {
        let mut tree = Tree::new();
        let root = tree.space(SpaceId::new(1));
        let n1 = tree.add_window(root, WindowId::new(1, 1));
        assert_eq!(tree.selection(), Some(n1));
        let n2 = tree.add_window(root, WindowId::new(1, 2));
        assert_eq!(tree.selection(), Some(n2));
        let n3 = tree.add_window(root, WindowId::new(1, 3));
        assert_eq!(tree.selection(), Some(n3));
        tree.select(n2);
        assert_eq!(tree.selection(), Some(n2));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 2));
        assert_eq!(tree.selection(), Some(n3));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 3));
        assert_eq!(tree.selection(), Some(n1));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 1));
        assert_eq!(tree.selection(), Some(root));
    }

    #[test]
    fn remembers_nested_paths() {
        let mut tree = Tree::new();
        let root = tree.space(SpaceId::new(1));
        let a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let _b1 = tree.add_window(a2, WindowId::new(1, 2));
        let b2 = tree.add_window(a2, WindowId::new(1, 3));
        let _b3 = tree.add_window(a2, WindowId::new(1, 4));
        let a3 = tree.add_window(root, WindowId::new(1, 5));

        tree.select(b2);
        assert_eq!(tree.selection(), Some(b2));
        tree.select(a1);
        assert_eq!(tree.selection(), Some(a1));
        tree.select(a3);
        assert_eq!(tree.selection(), Some(a3));
        tree.retain_windows(|&wid| wid != WindowId::new(1, 5));
        assert_eq!(tree.selection(), Some(b2));
    }

    #[test]
    fn selects_parent_when_there_are_no_children() {
        let mut tree = Tree::new();
        let root = tree.space(SpaceId::new(1));
        let _a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Horizontal);
        let _b1 = tree.add_window(a2, WindowId::new(2, 2));
        let b2 = tree.add_window(a2, WindowId::new(2, 3));
        let _b3 = tree.add_window(a2, WindowId::new(2, 4));
        let _a3 = tree.add_window(root, WindowId::new(1, 5));

        tree.select(b2);
        assert_eq!(tree.selection(), Some(b2));
        tree.retain_windows(|&wid| wid.pid != 2);
        assert_eq!(tree.selection(), Some(a2));
    }
}
