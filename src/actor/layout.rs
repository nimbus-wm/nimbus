use std::{
    collections::HashMap,
    fs::{self, File},
    io::{Read, Write},
    path::PathBuf,
};

use icrate::Foundation::{CGRect, CGSize};
use serde::{Deserialize, Serialize};
use tracing::{debug, error};

use crate::{
    actor::app::{pid_t, WindowId},
    model::{Direction, LayoutId, LayoutKind, LayoutTree, Orientation},
    sys::screen::SpaceId,
};

/// Actor that manages the layout tree.
///
/// This actor receives commands and (cleaned up) events from the Reactor,
/// converts them into layout operations, and calculates the desired position
/// and size of each window.
#[derive(Serialize, Deserialize)]
pub struct LayoutManager {
    tree: LayoutTree,
    active_layouts: HashMap<SpaceId, ActiveLayouts>,
    space_configurations: HashMap<(SpaceId, Size), Roots>,
}

#[derive(Serialize, Deserialize)]
struct ActiveLayouts {
    roots: Roots,
    selection: LayoutSelection,
}

#[derive(Serialize, Deserialize, Copy, Clone)]
struct Roots {
    tiling: LayoutId,
    floating: LayoutId,
}

#[derive(Serialize, Deserialize, Copy, Clone)]
enum LayoutSelection {
    Tiling,
    Floating,
}

impl Roots {
    fn iter(self) -> impl Iterator<Item = LayoutId> {
        [self.tiling, self.floating].into_iter()
    }

    fn select(&self, selection: LayoutSelection) -> LayoutId {
        match selection {
            LayoutSelection::Tiling => self.tiling,
            LayoutSelection::Floating => self.floating,
        }
    }
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
    Shuffle,
    NextWindow,
    PrevWindow,
    MoveFocus(Direction),
    Ascend,
    Descend,
    MoveNode(Direction),
    Split(Orientation),
    Group(Orientation),
    Ungroup,
    Debug,
    Serialize,
    SaveAndExit(PathBuf),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutEvent {
    WindowsOnScreenUpdated(SpaceId, pid_t, Vec<WindowId>),
    AppClosed(pid_t),
    WindowAdded(SpaceId, WindowId),
    WindowRemoved(WindowId),
    WindowRaised(SpaceId, Option<WindowId>),
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
    pub raise_window: Option<WindowId>,
}

impl LayoutManager {
    pub fn new() -> Self {
        LayoutManager {
            tree: LayoutTree::new(),
            active_layouts: Default::default(),
            space_configurations: Default::default(),
        }
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        debug!(?event);
        match event {
            LayoutEvent::SpaceExposed(space, size) => {
                let &mut roots =
                    self.space_configurations.entry((space, size.into())).or_insert_with(|| {
                        if let Some(active) = self.active_layouts.get(&space) {
                            Roots {
                                tiling: self.tree.clone_layout(active.roots.tiling),
                                floating: self.tree.clone_layout(active.roots.floating),
                            }
                        } else {
                            Roots {
                                tiling: self.tree.create_layout(),
                                floating: self.tree.create_layout(),
                            }
                        }
                    });
                self.active_layouts.insert(
                    space,
                    ActiveLayouts {
                        roots,
                        selection: LayoutSelection::Tiling,
                    },
                );
            }
            LayoutEvent::WindowsOnScreenUpdated(space, pid, mut windows) => {
                // The windows may already be in the layout if we restored a saved state, so
                // make sure not to duplicate or erase them here.
                let roots = self.roots(space);
                self.tree.intersect_windows_for_app(roots.floating, pid, &mut windows);
                self.tree.set_windows_for_app(roots.tiling, pid, windows);
            }
            LayoutEvent::AppClosed(pid) => {
                self.tree.remove_windows_for_app(pid);
            }
            LayoutEvent::WindowAdded(space, wid) => {
                let layout = self.active_layout(space);
                self.tree.add_window(layout, self.tree.root(layout), wid);
            }
            LayoutEvent::WindowRemoved(wid) => {
                self.tree.remove_window(wid);
            }
            LayoutEvent::WindowRaised(space, wid) => {
                if let Some(wid) = wid {
                    let roots = self.roots(space);
                    for layout in roots.iter() {
                        if let Some(node) = self.tree.window_node(layout, wid) {
                            self.tree.select(node);
                            break;
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
                let roots = self.roots(space);
                for layout in roots.iter() {
                    if let Some(node) = self.tree.window_node(layout, wid) {
                        self.tree.set_frame_from_resize(node, old_frame, new_frame, screen);
                        break;
                    }
                }
            }
        }
        EventResponse::default()
    }

    pub fn handle_command(&mut self, space: SpaceId, command: LayoutCommand) -> EventResponse {
        let active = self.active_layout(space);
        debug!("Tree:\n{}", self.tree.draw_tree(active).trim());
        debug!(selection = ?self.tree.selection(active));
        match command {
            LayoutCommand::Shuffle => {
                // TODO
                // self.window_order.shuffle(&mut rand::thread_rng());
                EventResponse::default()
            }
            LayoutCommand::NextWindow => {
                // TODO
                self.handle_command(space, LayoutCommand::MoveFocus(Direction::Left))
            }
            LayoutCommand::PrevWindow => {
                // TODO
                self.handle_command(space, LayoutCommand::MoveFocus(Direction::Right))
            }
            LayoutCommand::MoveFocus(direction) => {
                let new = self
                    .tree
                    .traverse(self.tree.selection(active), direction)
                    .and_then(|new| self.tree.window_at(new));
                let Some(new) = new else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
            LayoutCommand::Ascend => {
                self.tree.ascend_selection(active);
                EventResponse::default()
            }
            LayoutCommand::Descend => {
                self.tree.descend_selection(active);
                EventResponse::default()
            }
            LayoutCommand::MoveNode(direction) => {
                let selection = self.tree.selection(active);
                self.tree.move_node(active, selection, direction);
                EventResponse::default()
            }
            LayoutCommand::Split(orientation) => {
                let selection = self.tree.selection(active);
                self.tree.nest_in_container(active, selection, LayoutKind::from(orientation));
                EventResponse::default()
            }
            LayoutCommand::Group(orientation) => {
                if let Some(parent) = self.tree.selection(active).parent(self.tree.map()) {
                    self.tree.set_layout(parent, LayoutKind::group(orientation));
                }
                EventResponse::default()
            }
            LayoutCommand::Ungroup => {
                if let Some(parent) = self.tree.selection(active).parent(self.tree.map()) {
                    if self.tree.layout(parent).is_group() {
                        self.tree.set_layout(parent, self.tree.last_ungrouped_layout(parent))
                    }
                }
                EventResponse::default()
            }
            LayoutCommand::Debug => {
                let roots = self.roots(space);
                self.tree.print_tree(roots.tiling);
                self.tree.print_tree(roots.floating);
                EventResponse::default()
            }
            LayoutCommand::Serialize => {
                println!("{}", self.serialize_to_string());
                EventResponse::default()
            }
            LayoutCommand::SaveAndExit(path) => match self.save(path) {
                Ok(()) => std::process::exit(0),
                Err(e) => {
                    error!("Could not save layout: {e}");
                    std::process::exit(3);
                }
            },
        }
    }

    pub fn calculate_layout(&self, space: SpaceId, screen: CGRect) -> Vec<(WindowId, CGRect)> {
        let layouts = self.roots(space);
        //debug!("{}", self.tree.draw_tree(space));
        layouts
            .iter()
            .map(|layout| self.tree.calculate_layout(layout, screen))
            .flatten()
            .collect()
    }

    fn roots(&self, space: SpaceId) -> Roots {
        self.active_layouts[&space].roots
    }

    fn active_layout(&self, space: SpaceId) -> LayoutId {
        let layouts = &self.active_layouts[&space];
        layouts.roots.select(layouts.selection)
    }

    pub fn load(path: PathBuf) -> anyhow::Result<Self> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        Ok(ron::from_str(&buf)?)
    }

    fn save(&self, path: PathBuf) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        File::create(path)?.write_all(self.serialize_to_string().as_bytes())?;
        Ok(())
    }

    fn serialize_to_string(&self) -> String {
        ron::ser::to_string(&self).unwrap()
    }

    // This seems a bit messy, but it's simpler and more robust to write some
    // reactor tests as integration tests with this actor.
    #[cfg(test)]
    pub(super) fn selected_window(&mut self, space: SpaceId) -> Option<WindowId> {
        let layout = self.roots(space);
        self.tree.window_at(self.tree.selection(layout.tiling))
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
        _ = mgr.handle_event(WindowRaised(space, Some(WindowId::new(pid, 1))));
        _ = mgr.handle_command(space, LayoutCommand::MoveNode(Direction::Up));
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
        _ = mgr.handle_command(space, LayoutCommand::MoveNode(Direction::Down));
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
}
