use std::collections::BTreeMap;

use accessibility_sys::pid_t;
use serde::{Deserialize, Serialize};

use super::tree::{NodeId, NodeMap};
use super::LayoutId;
use crate::actor::app::WindowId;
use crate::collections::BTreeExt;
use crate::model::layout_tree::TreeEvent;

/// Maintains a two-way mapping between leaf nodes and window ids.
#[derive(Default, Serialize, Deserialize)]
pub struct Window {
    windows: slotmap::SecondaryMap<NodeId, WindowId>,
    window_nodes: BTreeMap<WindowId, Vec<WindowNodeInfo>>,
}

#[derive(Serialize, Deserialize)]
struct WindowNodeInfo {
    layout: LayoutId,
    node: NodeId,
}

impl Window {
    pub fn at(&self, node: NodeId) -> Option<WindowId> {
        self.windows.get(node).copied()
    }

    pub fn node_for(&self, layout: LayoutId, wid: WindowId) -> Option<NodeId> {
        self.window_nodes
            .get(&wid)
            .into_iter()
            .flat_map(|nodes| nodes.iter().filter(|info| info.layout == layout))
            .next()
            .map(|info| info.node)
    }

    pub fn set_window(&mut self, layout: LayoutId, node: NodeId, wid: WindowId) {
        let existing = self.windows.insert(node, wid);
        assert!(
            existing.is_none(),
            "Attempted to overwrite window for node {node:?} from {existing:?} to {wid:?}"
        );
        self.window_nodes.entry(wid).or_default().push(WindowNodeInfo { layout, node });
    }

    pub fn set_capacity(&mut self, capacity: usize) {
        self.windows.set_capacity(capacity);
        // There's not currently a stable way to do this for BTreeMap.
    }

    pub(super) fn take_nodes_for(
        &mut self,
        wid: WindowId,
    ) -> impl Iterator<Item = (LayoutId, NodeId)> + use<> {
        self.window_nodes
            .remove(&wid)
            .unwrap_or_default()
            .into_iter()
            .map(|info| (info.layout, info.node))
    }

    pub(super) fn take_nodes_for_app(
        &mut self,
        pid: pid_t,
    ) -> impl Iterator<Item = (WindowId, LayoutId, NodeId)> + use<> {
        let removed = self.window_nodes.remove_all_for_pid(pid);
        removed.into_iter().flat_map(|(wid, infos)| {
            infos.into_iter().map(move |info| (wid, info.layout, info.node))
        })
    }

    pub(super) fn handle_event(&mut self, map: &NodeMap, event: TreeEvent) {
        use TreeEvent::*;
        match event {
            AddedToForest(_) => (),
            AddedToParent(node) => debug_assert!(
                self.windows.get(node.parent(map).unwrap()).is_none(),
                "Window nodes are not allowed to have children: {:?}/{:?}",
                node.parent(map).unwrap(),
                node
            ),
            Copied { src, dest, dest_layout } => {
                if let Some(&wid) = self.windows.get(src) {
                    self.set_window(dest_layout, dest, wid);
                }
            }
            RemovingFromParent(_) => (),
            RemovedFromForest(node) => {
                if let Some(wid) = self.windows.remove(node) {
                    if let Some(window_nodes) = self.window_nodes.get_mut(&wid) {
                        window_nodes.retain(|info| info.node != node);
                        if window_nodes.is_empty() {
                            self.window_nodes.remove(&wid);
                        }
                    }
                }
            }
        }
    }
}
