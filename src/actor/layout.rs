use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::{self, File},
    io::{Read, Write},
    path::PathBuf,
};

use icrate::Foundation::{CGRect, CGSize};
use serde::{Deserialize, Serialize};
use tracing::debug;

use crate::{
    actor::app::{pid_t, WindowId},
    model::{Direction, LayoutId, LayoutKind, LayoutTree, Orientation},
    sys::screen::SpaceId,
    util::BTreeExt,
};

/// Actor that manages the layout tree.
///
/// This actor receives commands and (cleaned up) events from the Reactor,
/// converts them into layout operations, and calculates the desired position
/// and size of each window.
#[derive(Serialize, Deserialize)]
pub struct LayoutManager {
    tree: LayoutTree,
    active_layouts: HashMap<SpaceId, LayoutId>,
    space_configurations: HashMap<(SpaceId, Size), LayoutId>,
    floating_windows: BTreeSet<WindowId>,
    #[serde(skip)]
    active_floating_windows: HashMap<SpaceId, HashMap<pid_t, HashSet<WindowId>>>,
    #[serde(skip)]
    focused_window: Option<WindowId>,
    last_floating_focus: Option<WindowId>,
}

#[derive(Serialize, Deserialize, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Size {
    width: i32,
    height: i32,
}

impl From<CGSize> for Size {
    fn from(value: CGSize) -> Self {
        Self {
            width: value.width.round() as i32,
            height: value.height.round() as i32,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LayoutCommand {
    NextWindow,
    PrevWindow,
    MoveFocus(Direction),
    Ascend,
    Descend,
    MoveNode(Direction),
    Split(Orientation),
    Group(Orientation),
    Ungroup,
    ToggleFocusFloating,
    ToggleWindowFloating,
    ToggleFullscreen,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutEvent {
    WindowsOnScreenUpdated(SpaceId, pid_t, Vec<WindowId>),
    AppClosed(pid_t),
    WindowAdded(SpaceId, WindowId),
    WindowRemoved(WindowId),
    WindowFocused(Option<SpaceId>, Option<WindowId>),
    WindowResized {
        space: SpaceId,
        wid: WindowId,
        old_frame: CGRect,
        new_frame: CGRect,
        screen: CGRect,
    },
    SpaceExposed(SpaceId, CGSize),
}

#[must_use]
#[derive(Debug, Clone, Default)]
pub struct EventResponse {
    /// Windows to raise quietly. No WindowFocused events will be created for
    /// these.
    pub raise_windows: Vec<WindowId>,
    /// Window to focus. This window will be raised after the windows in
    /// raise_windows and a WindowFocused event will be generated.
    pub focus_window: Option<WindowId>,
}

impl LayoutManager {
    pub fn new() -> Self {
        LayoutManager {
            tree: LayoutTree::new(),
            active_layouts: Default::default(),
            space_configurations: Default::default(),
            floating_windows: Default::default(),
            active_floating_windows: Default::default(),
            focused_window: None,
            last_floating_focus: None,
        }
    }

    pub fn debug_tree(&self, space: SpaceId) {
        self.debug_tree_desc(space, "", false);
    }

    pub fn debug_tree_desc(&self, space: SpaceId, desc: &'static str, print: bool) {
        if let Some(&layout) = self.active_layouts.get(&space) {
            if print {
                println!("Tree {desc}\n{}", self.tree.draw_tree(layout).trim());
            } else {
                debug!("Tree {desc}\n{}", self.tree.draw_tree(layout).trim());
            }
        } else {
            debug!("No layout for space {space:?}");
        }
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        debug!(?event);
        match event {
            LayoutEvent::SpaceExposed(space, size) => {
                self.debug_tree(space);
                let layout =
                    self.space_configurations.entry((space, size.into())).or_insert_with(|| {
                        if let Some(&active) = self.active_layouts.get(&space) {
                            debug!("Cloning layout {active:?}");
                            self.tree.clone_layout(active)
                        } else {
                            debug!("Creating new layout");
                            self.tree.create_layout()
                        }
                    });
                debug!("Using layout {layout:?} on space {space:?}");
                self.active_layouts.insert(space, *layout);
            }
            LayoutEvent::WindowsOnScreenUpdated(space, pid, mut windows) => {
                self.debug_tree(space);
                // The windows may already be in the layout if we restored a saved state, so
                // make sure not to duplicate or erase them here.
                let floating_active =
                    self.active_floating_windows.entry(space).or_default().entry(pid).or_default();
                floating_active.clear();
                windows.retain(|wid| {
                    let floating = self.floating_windows.contains(wid);
                    if floating {
                        floating_active.insert(*wid);
                    }
                    !floating
                });
                self.tree.set_windows_for_app(self.layout(space), pid, windows);
            }
            LayoutEvent::AppClosed(pid) => {
                self.tree.remove_windows_for_app(pid);
                self.floating_windows.remove_all_for_pid(pid);
            }
            LayoutEvent::WindowAdded(space, wid) => {
                self.debug_tree(space);
                let layout = self.layout(space);
                self.tree.add_window(layout, self.tree.root(layout), wid);
            }
            LayoutEvent::WindowRemoved(wid) => {
                self.tree.remove_window(wid);
                self.floating_windows.remove(&wid);
            }
            LayoutEvent::WindowFocused(space, wid) => {
                self.focused_window = wid;
                if let Some(wid) = wid {
                    if self.floating_windows.contains(&wid) {
                        self.last_floating_focus = Some(wid);
                    } else if let Some(space) = space {
                        let layout = self.layout(space);
                        if let Some(node) = self.tree.window_node(layout, wid) {
                            self.tree.select(node);
                        }
                    }
                }
            }
            LayoutEvent::WindowResized {
                space,
                wid,
                old_frame,
                new_frame,
                screen,
            } => {
                let layout = self.layout(space);
                if let Some(node) = self.tree.window_node(layout, wid) {
                    self.tree.set_frame_from_resize(node, old_frame, new_frame, screen);
                }
            }
        }
        EventResponse::default()
    }

    pub fn handle_command(
        &mut self,
        space: Option<SpaceId>,
        command: LayoutCommand,
    ) -> EventResponse {
        if let Some(space) = space {
            let layout = self.layout(space);
            debug!("Tree:\n{}", self.tree.draw_tree(layout).trim());
            debug!(selection = ?self.tree.selection(layout));
        }
        let is_floating = if let Some(focus) = self.focused_window {
            self.floating_windows.contains(&focus)
        } else {
            false
        };
        debug!(?self.floating_windows);
        debug!(?self.focused_window, ?self.last_floating_focus, ?is_floating);

        // ToggleWindowFloating is the only command that works when the space is
        // disabled.
        if let LayoutCommand::ToggleWindowFloating = &command {
            let Some(wid) = self.focused_window else {
                return EventResponse::default();
            };
            if is_floating {
                if let Some(space) = space {
                    let layout = self.layout(space);
                    let selection = self.tree.selection(layout);
                    let node = self.tree.add_window(
                        layout,
                        selection.parent(self.tree.map()).unwrap_or(selection),
                        wid,
                    );
                    self.tree.select(node);
                    self.active_floating_windows
                        .entry(space)
                        .or_default()
                        .entry(wid.pid)
                        .or_default()
                        .remove(&wid);
                }
                self.floating_windows.remove(&wid);
                self.last_floating_focus = None;
            } else {
                if let Some(space) = space {
                    self.active_floating_windows
                        .entry(space)
                        .or_default()
                        .entry(wid.pid)
                        .or_default()
                        .insert(wid);
                }
                self.tree.remove_window(wid);
                self.floating_windows.insert(wid);
                self.last_floating_focus = Some(wid);
            }
            return EventResponse::default();
        }

        let Some(space) = space else {
            return EventResponse::default();
        };
        let layout = self.layout(space);

        if let LayoutCommand::ToggleFocusFloating = &command {
            if is_floating {
                let selection = self.tree.window_at(self.tree.selection(layout));
                let tree_windows = self
                    .tree
                    .root(layout)
                    .traverse_preorder(self.tree.map())
                    .flat_map(|node| self.tree.window_at(node));
                return EventResponse {
                    raise_windows: tree_windows.filter(|&wid| Some(wid) != selection).collect(),
                    focus_window: selection,
                };
            } else {
                let floating_windows = self
                    .active_floating_windows
                    .entry(space)
                    .or_default()
                    .values()
                    .flatten()
                    .copied();
                return EventResponse {
                    raise_windows: floating_windows
                        .filter(|&wid| Some(wid) != self.last_floating_focus)
                        .collect(),
                    focus_window: self.last_floating_focus,
                };
            }
        }

        // Remaining commands only work for tiling layout.
        if is_floating {
            return EventResponse::default();
        }

        match command {
            // Handled above.
            LayoutCommand::ToggleWindowFloating => unreachable!(),
            LayoutCommand::ToggleFocusFloating => unreachable!(),

            LayoutCommand::NextWindow => {
                // TODO
                self.handle_command(Some(space), LayoutCommand::MoveFocus(Direction::Left))
            }
            LayoutCommand::PrevWindow => {
                // TODO
                self.handle_command(Some(space), LayoutCommand::MoveFocus(Direction::Right))
            }
            LayoutCommand::MoveFocus(direction) => {
                let new = self
                    .tree
                    .traverse(self.tree.selection(layout), direction)
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse {
                    focus_window: Some(new),
                    ..Default::default()
                }
            }
            LayoutCommand::Ascend => {
                self.tree.ascend_selection(layout);
                EventResponse::default()
            }
            LayoutCommand::Descend => {
                self.tree.descend_selection(layout);
                EventResponse::default()
            }
            LayoutCommand::MoveNode(direction) => {
                let selection = self.tree.selection(layout);
                self.tree.move_node(layout, selection, direction);
                EventResponse::default()
            }
            LayoutCommand::Split(orientation) => {
                let selection = self.tree.selection(layout);
                self.tree.nest_in_container(layout, selection, LayoutKind::from(orientation));
                EventResponse::default()
            }
            LayoutCommand::Group(orientation) => {
                if let Some(parent) = self.tree.selection(layout).parent(self.tree.map()) {
                    self.tree.set_layout(parent, LayoutKind::group(orientation));
                }
                EventResponse::default()
            }
            LayoutCommand::Ungroup => {
                if let Some(parent) = self.tree.selection(layout).parent(self.tree.map()) {
                    if self.tree.layout(parent).is_group() {
                        self.tree.set_layout(parent, self.tree.last_ungrouped_layout(parent))
                    }
                }
                EventResponse::default()
            }
            LayoutCommand::ToggleFullscreen => {
                let node = self.tree.selection(layout);
                if self.tree.toggle_fullscreen(node) {
                    // If we have multiple windows in the newly fullscreen node,
                    // make sure they are on top.
                    let node_windows = node
                        .traverse_preorder(self.tree.map())
                        .flat_map(|n| self.tree.window_at(n))
                        .collect();
                    EventResponse {
                        raise_windows: node_windows,
                        focus_window: None,
                    }
                } else {
                    EventResponse::default()
                }
            }
        }
    }

    pub fn calculate_layout(&self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let layout = self.layout(space);
        //debug!("{}", self.tree.draw_tree(space));
        self.tree.calculate_layout(layout, screen)
    }

    fn layout(&self, space: SpaceId) -> LayoutId {
        self.active_layouts[&space]
    }

    pub fn load(path: PathBuf) -> anyhow::Result<Self> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        Ok(ron::from_str(&buf)?)
    }

    pub fn save(&self, path: PathBuf) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        File::create(path)?.write_all(self.serialize_to_string().as_bytes())?;
        Ok(())
    }

    pub fn serialize_to_string(&self) -> String {
        ron::ser::to_string(&self).unwrap()
    }

    // This seems a bit messy, but it's simpler and more robust to write some
    // reactor tests as integration tests with this actor.
    #[cfg(test)]
    pub(super) fn selected_window(&mut self, space: SpaceId) -> Option<WindowId> {
        let layout = self.layout(space);
        self.tree.window_at(self.tree.selection(layout))
    }
}

#[cfg(test)]
mod tests {
    use icrate::Foundation::CGPoint;

    use super::*;

    fn rect(x: i32, y: i32, w: i32, h: i32) -> CGRect {
        CGRect::new(
            CGPoint::new(x as f64, y as f64),
            CGSize::new(w as f64, h as f64),
        )
    }

    fn make_windows(pid: pid_t, num: u32) -> Vec<WindowId> {
        (1..=num).map(|idx| WindowId::new(pid, idx)).collect()
    }

    impl LayoutManager {
        fn layout_sorted(&self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
            let mut layout = self.calculate_layout(space, screen);
            layout.sort_by_key(|(wid, _)| *wid);
            layout
        }
    }

    #[test]
    fn it_maintains_separate_layouts_for_each_screen_size() {
        use LayoutEvent::*;
        let mut mgr = LayoutManager::new();
        let space = SpaceId::new(1);
        let pid = 1;

        // Set up the starting layout.
        let screen1 = rect(0, 0, 120, 120);
        _ = mgr.handle_event(SpaceExposed(space, screen1.size));
        _ = mgr.handle_event(WindowsOnScreenUpdated(space, pid, make_windows(pid, 3)));
        _ = mgr.handle_event(WindowFocused(Some(space), Some(WindowId::new(pid, 1))));
        _ = mgr.handle_command(Some(space), LayoutCommand::MoveNode(Direction::Up));
        assert_eq!(
            vec![
                (WindowId::new(pid, 1), rect(0, 0, 120, 60)),
                (WindowId::new(pid, 2), rect(0, 60, 60, 60)),
                (WindowId::new(pid, 3), rect(60, 60, 60, 60)),
            ],
            mgr.layout_sorted(space, screen1),
        );

        // Introduce new screen size.
        let screen2 = rect(0, 0, 1200, 1200);
        _ = mgr.handle_event(SpaceExposed(space, screen2.size));
        assert_eq!(
            vec![
                (WindowId::new(pid, 1), rect(0, 0, 1200, 600)),
                (WindowId::new(pid, 2), rect(0, 600, 600, 600)),
                (WindowId::new(pid, 3), rect(600, 600, 600, 600)),
            ],
            mgr.layout_sorted(space, screen2),
            "layout was not correctly scaled to new screen size"
        );

        // Change tha layout for the second screen size.
        _ = mgr.handle_command(Some(space), LayoutCommand::MoveNode(Direction::Down));
        assert_eq!(
            vec![
                (WindowId::new(pid, 1), rect(0, 0, 400, 1200)),
                (WindowId::new(pid, 2), rect(400, 0, 400, 1200)),
                (WindowId::new(pid, 3), rect(800, 0, 400, 1200)),
            ],
            mgr.layout_sorted(space, screen2),
        );

        // Switch back to the first size; the layout should be the same as before.
        _ = mgr.handle_event(SpaceExposed(space, screen1.size));
        assert_eq!(
            vec![
                (WindowId::new(pid, 1), rect(0, 0, 120, 60)),
                (WindowId::new(pid, 2), rect(0, 60, 60, 60)),
                (WindowId::new(pid, 3), rect(60, 60, 60, 60)),
            ],
            mgr.layout_sorted(space, screen1),
        );

        // Switch back to the second size.
        _ = mgr.handle_event(SpaceExposed(space, screen2.size));
        assert_eq!(
            vec![
                (WindowId::new(pid, 1), rect(0, 0, 400, 1200)),
                (WindowId::new(pid, 2), rect(400, 0, 400, 1200)),
                (WindowId::new(pid, 3), rect(800, 0, 400, 1200)),
            ],
            mgr.layout_sorted(space, screen2),
        );
    }

    #[test]
    fn floating_windows() {
        use LayoutEvent::*;
        let mut mgr = LayoutManager::new();
        let space = SpaceId::new(1);
        let pid = 1;

        let screen1 = rect(0, 0, 120, 120);
        _ = mgr.handle_event(SpaceExposed(space, screen1.size));
        _ = mgr.handle_event(WindowsOnScreenUpdated(space, pid, make_windows(pid, 3)));

        _ = mgr.handle_event(WindowFocused(Some(space), Some(WindowId::new(pid, 2))));
        _ = mgr.handle_event(WindowFocused(Some(space), Some(WindowId::new(pid, 1))));

        // Make the first window float.
        _ = mgr.handle_command(Some(space), LayoutCommand::ToggleWindowFloating);
        let sizes: HashMap<_, _> = mgr.calculate_layout(space, screen1).into_iter().collect();
        assert_eq!(sizes[&WindowId::new(pid, 2)], rect(0, 0, 60, 120));
        assert_eq!(sizes[&WindowId::new(pid, 3)], rect(60, 0, 60, 120));

        // Toggle back to the tiled windows.
        let response = mgr.handle_command(Some(space), LayoutCommand::ToggleFocusFloating);
        assert_eq!(vec![WindowId::new(pid, 3)], response.raise_windows);
        assert_eq!(Some(WindowId::new(pid, 2)), response.focus_window);
        _ = mgr.handle_event(WindowFocused(Some(space), response.focus_window));

        // Make the second window float.
        _ = mgr.handle_command(Some(space), LayoutCommand::ToggleWindowFloating);
        let sizes: HashMap<_, _> = mgr.calculate_layout(space, screen1).into_iter().collect();
        assert_eq!(sizes[&WindowId::new(pid, 3)], rect(0, 0, 120, 120));

        // Toggle back to tiled.
        let response = mgr.handle_command(Some(space), LayoutCommand::ToggleFocusFloating);
        assert!(response.raise_windows.is_empty());
        assert_eq!(Some(WindowId::new(pid, 3)), response.focus_window);
        _ = mgr.handle_event(WindowFocused(Some(space), response.focus_window));

        // Toggle back to floating.
        let response = mgr.handle_command(Some(space), LayoutCommand::ToggleFocusFloating);
        assert_eq!(vec![WindowId::new(pid, 1)], response.raise_windows);
        assert_eq!(Some(WindowId::new(pid, 2)), response.focus_window);
        _ = mgr.handle_event(WindowFocused(Some(space), response.focus_window));
    }

    #[test]
    fn floating_windows_space_disabled() {
        use LayoutEvent::*;
        let mut mgr = LayoutManager::new();
        let space = SpaceId::new(1);
        let pid = 1;

        _ = mgr.handle_event(WindowFocused(None, Some(WindowId::new(pid, 1))));

        // Make the first window float.
        _ = mgr.handle_command(None, LayoutCommand::ToggleWindowFloating);

        // Enable the space.
        let screen1 = rect(0, 0, 120, 120);
        _ = mgr.handle_event(SpaceExposed(space, screen1.size));
        _ = mgr.handle_event(WindowsOnScreenUpdated(space, pid, make_windows(pid, 3)));

        let sizes: HashMap<_, _> = mgr.calculate_layout(space, screen1).into_iter().collect();
        assert_eq!(sizes[&WindowId::new(pid, 2)], rect(0, 0, 60, 120));
        assert_eq!(sizes[&WindowId::new(pid, 3)], rect(60, 0, 60, 120));

        // Toggle back to the tiled windows.
        let response = mgr.handle_command(Some(space), LayoutCommand::ToggleFocusFloating);
        let mut raised_windows = response.raise_windows;
        raised_windows.extend(response.focus_window);
        raised_windows.sort();
        assert_eq!(
            vec![WindowId::new(pid, 2), WindowId::new(pid, 3)],
            raised_windows
        );
        _ = mgr.handle_event(WindowFocused(Some(space), response.focus_window));

        // Toggle back to floating.
        let response = mgr.handle_command(Some(space), LayoutCommand::ToggleFocusFloating);
        assert!(response.raise_windows.is_empty());
        assert_eq!(Some(WindowId::new(pid, 1)), response.focus_window);
        _ = mgr.handle_event(WindowFocused(Some(space), response.focus_window));
    }
}
