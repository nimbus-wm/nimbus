use core::fmt::Debug;
use std::mem;

use objc2_core_foundation::{CGPoint, CGRect, CGSize};
use serde::{Deserialize, Serialize};

use super::layout_tree::TreeEvent;
use super::tree::{NodeId, NodeMap};
use crate::actor::app::WindowId;
use crate::sys::geometry::Round;

#[derive(Default, Serialize, Deserialize)]
pub struct Size {
    info: slotmap::SecondaryMap<NodeId, LayoutInfo>,
}

#[allow(unused)]
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ContainerKind {
    #[default]
    Horizontal,
    Vertical,
    Tabbed,
    Stacked,
}

impl ContainerKind {
    pub fn from(orientation: Orientation) -> Self {
        match orientation {
            Orientation::Horizontal => ContainerKind::Horizontal,
            Orientation::Vertical => ContainerKind::Vertical,
        }
    }

    pub fn group(orientation: Orientation) -> Self {
        match orientation {
            Orientation::Horizontal => ContainerKind::Tabbed,
            Orientation::Vertical => ContainerKind::Stacked,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Orientation {
    Horizontal,
    Vertical,
}

impl ContainerKind {
    pub fn orientation(self) -> Orientation {
        use ContainerKind::*;
        match self {
            Horizontal | Tabbed => Orientation::Horizontal,
            Vertical | Stacked => Orientation::Vertical,
        }
    }

    pub fn is_group(self) -> bool {
        use ContainerKind::*;
        match self {
            Stacked | Tabbed => true,
            _ => false,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
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

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
struct LayoutInfo {
    /// The share of the parent's size taken up by this node; 1.0 by default.
    size: f32,
    /// The total size of all children.
    total: f32,
    /// The orientation of this node. Not used for leaf nodes.
    kind: ContainerKind,
    /// The last ungrouped layout of this node.
    last_ungrouped_kind: ContainerKind,
    /// Whether the node is fullscreen.
    #[serde(default)]
    is_fullscreen: bool,
}

impl Size {
    pub(super) fn handle_event(&mut self, map: &NodeMap, event: TreeEvent) {
        match event {
            TreeEvent::AddedToForest(node) => {
                self.info.insert(node, LayoutInfo::default());
            }
            TreeEvent::AddedToParent(node) => {
                let parent = node.parent(map).unwrap();
                self.info[node].size = 1.0;
                self.info[parent].total += 1.0;
            }
            TreeEvent::Copied { src, dest, .. } => {
                self.info.insert(dest, self.info[src].clone());
            }
            TreeEvent::RemovingFromParent(node) => {
                self.info[node.parent(map).unwrap()].total -= self.info[node].size;
            }
            TreeEvent::RemovedFromForest(node) => {
                self.info.remove(node);
            }
        }
    }

    pub(super) fn assume_size_of(&mut self, new: NodeId, old: NodeId, map: &NodeMap) {
        assert_eq!(new.parent(map), old.parent(map));
        let parent = new.parent(map).unwrap();
        self.info[parent].total -= self.info[new].size;
        self.info[new].size = mem::replace(&mut self.info[old].size, 0.0);
    }

    pub(super) fn set_kind(&mut self, node: NodeId, kind: ContainerKind) {
        self.info[node].kind = kind;
        if !kind.is_group() {
            self.info[node].last_ungrouped_kind = kind;
        }
    }

    pub(super) fn kind(&self, node: NodeId) -> ContainerKind {
        self.info[node].kind
    }

    pub(super) fn last_ungrouped_kind(&self, node: NodeId) -> ContainerKind {
        self.info[node].last_ungrouped_kind
    }

    pub(super) fn proportion(&self, map: &NodeMap, node: NodeId) -> Option<f64> {
        let Some(parent) = node.parent(map) else { return None };
        Some(f64::from(self.info[node].size) / f64::from(self.info[parent].total))
    }

    pub(super) fn total(&self, node: NodeId) -> f64 {
        f64::from(self.info[node].total)
    }

    pub(super) fn take_share(&mut self, map: &NodeMap, node: NodeId, from: NodeId, share: f32) {
        assert_eq!(node.parent(map), from.parent(map));
        let share = share.min(self.info[from].size);
        let share = share.max(-self.info[node].size);
        self.info[from].size -= share;
        self.info[node].size += share;
    }

    pub(super) fn set_fullscreen(&mut self, node: NodeId, is_fullscreen: bool) {
        self.info[node].is_fullscreen = is_fullscreen;
    }

    pub(super) fn toggle_fullscreen(&mut self, node: NodeId) -> bool {
        self.info[node].is_fullscreen = !self.info[node].is_fullscreen;
        self.info[node].is_fullscreen
    }

    pub(super) fn debug(&self, node: NodeId, is_container: bool) -> String {
        let info = &self.info[node];
        if is_container {
            format!("{:?} [size {} total={}]", info.kind, info.size, info.total)
        } else {
            format!("[size {}]", info.size)
        }
    }

    pub(super) fn get_sizes(
        &self,
        map: &NodeMap,
        window: &super::window::Window,
        root: NodeId,
        screen: CGRect,
    ) -> Vec<(WindowId, CGRect)> {
        let mut sizes = vec![];
        self.apply(map, window, root, screen, screen, &mut sizes);
        sizes
    }

    fn apply(
        &self,
        map: &NodeMap,
        window: &super::window::Window,
        node: NodeId,
        rect: CGRect,
        screen: CGRect,
        sizes: &mut Vec<(WindowId, CGRect)>,
    ) {
        let info = &self.info[node];
        let rect = if info.is_fullscreen { screen } else { rect };

        if let Some(wid) = window.at(node) {
            debug_assert!(
                node.children(map).next().is_none(),
                "non-leaf node with window id"
            );
            sizes.push((wid, rect));
            return;
        }

        use ContainerKind::*;
        match info.kind {
            Tabbed | Stacked => {
                for child in node.children(map) {
                    self.apply(map, window, child, rect, screen, sizes);
                }
            }
            Horizontal => {
                let mut x = rect.origin.x;
                let total = self.info[node].total;
                for child in node.children(map) {
                    let ratio = f64::from(self.info[child].size) / f64::from(total);
                    let rect = CGRect {
                        origin: CGPoint { x, y: rect.origin.y },
                        size: CGSize {
                            width: rect.size.width * ratio,
                            height: rect.size.height,
                        },
                    }
                    .round();
                    self.apply(map, window, child, rect, screen, sizes);
                    x = rect.max().x;
                }
            }
            Vertical => {
                let mut y = rect.origin.y;
                let total = self.info[node].total;
                for child in node.children(map) {
                    let ratio = f64::from(self.info[child].size) / f64::from(total);
                    let rect = CGRect {
                        origin: CGPoint { x: rect.origin.x, y },
                        size: CGSize {
                            width: rect.size.width,
                            height: rect.size.height * ratio,
                        },
                    }
                    .round();
                    self.apply(map, window, child, rect, screen, sizes);
                    y = rect.max().y;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::model::LayoutTree;

    fn rect(x: i32, y: i32, w: i32, h: i32) -> CGRect {
        CGRect::new(
            CGPoint::new(f64::from(x), f64::from(y)),
            CGSize::new(f64::from(w), f64::from(h)),
        )
    }

    #[test]
    fn it_lays_out_windows_proportionally() {
        let mut tree = LayoutTree::new();
        let layout = tree.create_layout();
        let root = tree.root(layout);
        let _a1 = tree.add_window_under(layout, root, WindowId::new(1, 1));
        let a2 = tree.add_container(root, ContainerKind::Vertical);
        let _b1 = tree.add_window_under(layout, a2, WindowId::new(1, 2));
        let _b2 = tree.add_window_under(layout, a2, WindowId::new(1, 3));
        let _a3 = tree.add_window_under(layout, root, WindowId::new(1, 4));

        let screen = rect(0, 0, 3000, 1000);
        let mut frames = tree.calculate_layout(layout, screen);
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
