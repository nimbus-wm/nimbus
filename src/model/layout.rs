use core::fmt::Debug;

use icrate::Foundation::{CGPoint, CGRect, CGSize};

use super::{
    node::{Forest, NodeId},
    tree::{TreeEvent, Windows},
};
use crate::{app::WindowId, util::Round};

#[derive(Default)]
pub struct Layout {
    info: slotmap::SecondaryMap<NodeId, LayoutInfo>,
}

#[allow(unused)]
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub enum LayoutKind {
    #[default]
    Horizontal,
    Vertical,
    Tabbed,
    Stacked,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Orientation {
    Horizontal,
    Vertical,
}

impl LayoutKind {
    pub(super) fn orientation(self) -> Orientation {
        use LayoutKind::*;
        match self {
            Horizontal | Tabbed => Orientation::Horizontal,
            Vertical | Stacked => Orientation::Vertical,
        }
    }

    pub(super) fn is_group(self) -> bool {
        use LayoutKind::*;
        match self {
            Stacked | Tabbed => true,
            _ => false,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

impl Direction {
    pub(super) fn orientation(self) -> Orientation {
        use Direction::*;
        match self {
            Left | Right => Orientation::Horizontal,
            Up | Down => Orientation::Vertical,
        }
    }
}

// TODO:
//
// It'd be much easier to only move specific edges if we keep the min edge
// of each child (relative to the parent, from 0 to 1). Then we just need
// to adjust this edge, and preserve the invariant that no edge is greater
// than the following edge.
//
// Calculating the size of a single node is easy and just needs to look at the
// next sibling.
//
// Proportional changes would no longer happen by default, but should still be
// relatively easy. Just keep a count of children, and we can adjust each child's
// size in a single scan.
//
// This seems *way* simpler than trying to fix up a proportionate representation
// to create a single edge change.
//
// Actually, on second thought, this would still create proportional resizes of
// children. To prevent that we would need the edges to be absolute (relative
// to the root) and traverse *recursively* when one is modified, fixing up any
// edges that violate our invariant.
//
// This might still be overall simpler than the resize logic would need to be
// for the proportionate case, but it feels more like we are distributing the
// complexity rather than reducing it.

#[derive(Default, Debug)]
struct LayoutInfo {
    /// The share of the parent's size taken up by this node; 1.0 by default.
    size: f32,
    /// The total size of all children.
    total: f32,
    /// The orientation of this node. Not used for leaf nodes.
    kind: LayoutKind,
}

impl Layout {
    pub(super) fn handle_event(&mut self, forest: &Forest, event: TreeEvent) {
        match event {
            TreeEvent::AddedToForest(node) => {
                self.info.insert(node, LayoutInfo::default());
            }
            TreeEvent::AddedToParent(node) => {
                let parent = node.parent(forest).unwrap();
                self.info[node].size = 1.0;
                self.info[parent].total += 1.0;
            }
            TreeEvent::RemovingFromParent(node) => {
                self.info[node.parent(forest).unwrap()].total -= self.info[node].size;
            }
            TreeEvent::RemovedFromForest(node) => {
                self.info.remove(node);
            }
        }
    }

    pub(super) fn set_kind(&mut self, node: NodeId, kind: LayoutKind) {
        self.info[node].kind = kind;
    }

    pub(super) fn kind(&self, node: NodeId) -> LayoutKind {
        self.info[node].kind
    }

    pub(super) fn proportion(&self, forest: &Forest, node: NodeId) -> Option<f64> {
        let Some(parent) = node.parent(forest) else { return None };
        Some(f64::from(self.info[node].size) / f64::from(self.info[parent].total))
    }

    pub(super) fn total(&self, node: NodeId) -> f64 {
        f64::from(self.info[node].total)
    }

    pub(super) fn take_share(&mut self, forest: &Forest, node: NodeId, from: NodeId, share: f32) {
        assert_eq!(node.parent(forest), from.parent(forest));
        let share = share.min(self.info[from].size);
        let share = share.max(-self.info[node].size);
        self.info[from].size -= share;
        self.info[node].size += share;
    }

    pub(super) fn debug(&self, node: NodeId) -> impl Debug + '_ {
        &self.info[node]
    }

    pub(super) fn get_sizes(
        &self,
        forest: &Forest,
        windows: &Windows,
        root: NodeId,
        rect: CGRect,
    ) -> Vec<(WindowId, CGRect)> {
        let mut sizes = vec![];
        self.apply(forest, windows, root, rect, &mut sizes);
        sizes
    }

    fn apply(
        &self,
        forest: &Forest,
        windows: &Windows,
        node: NodeId,
        rect: CGRect,
        sizes: &mut Vec<(WindowId, CGRect)>,
    ) {
        if let Some(&wid) = windows.get(node) {
            debug_assert!(
                node.children(forest).next().is_none(),
                "non-leaf node with window id"
            );
            sizes.push((wid, rect));
            return;
        }

        use LayoutKind::*;
        match self.info[node].kind {
            Tabbed | Stacked => {
                for child in node.children(forest) {
                    self.apply(forest, windows, child, rect, sizes);
                }
            }
            Horizontal => {
                let mut x = rect.origin.x;
                let total = self.info[node].total;
                for child in node.children(forest) {
                    let ratio = f64::from(self.info[child].size) / f64::from(total);
                    let rect = CGRect {
                        origin: CGPoint { x, y: rect.origin.y },
                        size: CGSize {
                            width: rect.size.width * ratio,
                            height: rect.size.height,
                        },
                    }
                    .round();
                    self.apply(forest, windows, child, rect, sizes);
                    x = rect.max().x;
                }
            }
            Vertical => {
                let mut y = rect.origin.y;
                let total = self.info[node].total;
                for child in node.children(forest) {
                    let ratio = f64::from(self.info[child].size) / f64::from(total);
                    let rect = CGRect {
                        origin: CGPoint { x: rect.origin.x, y },
                        size: CGSize {
                            width: rect.size.width,
                            height: rect.size.height * ratio,
                        },
                    }
                    .round();
                    self.apply(forest, windows, child, rect, sizes);
                    y = rect.max().y;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{model::Tree, screen::SpaceId};
    use pretty_assertions::assert_eq;

    fn rect(x: i32, y: i32, w: i32, h: i32) -> CGRect {
        CGRect::new(
            CGPoint::new(f64::from(x), f64::from(y)),
            CGSize::new(f64::from(w), f64::from(h)),
        )
    }

    #[test]
    fn it_lays_out_windows_proportionally() {
        let mut tree = Tree::new();
        let space = SpaceId::new(1);
        let root = tree.space(space);
        let _a1 = tree.add_window(root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, LayoutKind::Vertical);
        let _b1 = tree.add_window(a2, WindowId::new(1, 2));
        let _b2 = tree.add_window(a2, WindowId::new(1, 3));
        let _a3 = tree.add_window(root, WindowId::new(1, 4));

        let screen = rect(0, 0, 3000, 1000);
        let mut frames = tree.calculate_layout(root, screen);
        frames.sort_by_key(|&(wid, _)| wid);
        assert_eq!(
            frames,
            vec![
                (WindowId::new(1, 1), rect(0, 0, 1000, 1000)),
                (WindowId::new(1, 2), rect(1000, 0, 1000, 500)),
                (WindowId::new(1, 3), rect(1000, 500, 1000, 500)),
                (WindowId::new(1, 4), rect(2000, 0, 1000, 1000)),
            ]
        );
    }
}