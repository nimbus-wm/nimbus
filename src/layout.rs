use std::iter;

use icrate::Foundation::{CGPoint, CGRect, CGSize};
use rand::seq::SliceRandom;

use crate::app::WindowId;

pub struct LayoutManager {
    current_layout: Layout,
    window_order: Vec<WindowId>,
    main_window: Option<WindowId>,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone)]
pub enum Layout {
    Slice(Orientation),
    Bsp(Orientation),
}

#[derive(Debug, Copy, Clone)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone)]
pub enum LayoutCommand {
    Shuffle,
    NextWindow,
    PrevWindow,
}

#[derive(Debug, Clone)]
pub enum LayoutEvent {
    WindowRaised(Option<WindowId>),
}

#[must_use]
#[derive(Debug, Clone, Default)]
pub struct EventResponse {
    pub raise_window: Option<WindowId>,
}

impl LayoutManager {
    pub fn new() -> Self {
        LayoutManager {
            current_layout: Layout::Bsp(Orientation::Horizontal),
            window_order: Vec::new(),
            main_window: None,
        }
    }

    pub fn add_window(&mut self, wid: WindowId) {
        self.window_order.push(wid);
    }

    pub fn add_windows(&mut self, wids: impl Iterator<Item = WindowId>) {
        self.window_order.extend(wids);
    }

    pub fn retain_windows(&mut self, f: impl Fn(&WindowId) -> bool) {
        self.window_order.retain(f);
    }

    pub fn windows(&self) -> &[WindowId] {
        &self.window_order
    }

    pub fn handle_event(&mut self, event: LayoutEvent) -> EventResponse {
        match event {
            LayoutEvent::WindowRaised(wid) => self.main_window = wid,
        }
        EventResponse::default()
    }

    pub fn handle_command(&mut self, command: LayoutCommand) -> EventResponse {
        match command {
            LayoutCommand::Shuffle => {
                self.window_order.shuffle(&mut rand::thread_rng());
                EventResponse::default()
            }
            LayoutCommand::NextWindow => {
                let Some(cur) = self.main_window else {
                    return EventResponse::default();
                };
                let Some(idx) = self.window_order.iter().position(|&wid| wid == cur) else {
                    return EventResponse::default();
                };
                let Some(&new) = self.window_order.get(idx + 1) else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
            LayoutCommand::PrevWindow => {
                let Some(cur) = self.main_window else {
                    return EventResponse::default();
                };
                let Some(idx) = self.window_order.iter().position(|&wid| wid == cur) else {
                    return EventResponse::default();
                };
                if idx == 0 {
                    return EventResponse::default();
                }
                let Some(&new) = self.window_order.get(idx - 1) else {
                    return EventResponse::default();
                };
                EventResponse { raise_window: Some(new) }
            }
        }
    }

    pub fn calculate(&self, screen: CGRect) -> Vec<CGRect> {
        calculate_layout(screen, self.window_order.len() as u32, self.current_layout)
    }
}

fn calculate_layout(screen: CGRect, num_windows: u32, layout: Layout) -> Vec<CGRect> {
    use Layout::*;
    use Orientation::*;
    if num_windows == 0 {
        return vec![];
    }
    if num_windows == 1 {
        return vec![screen];
    }
    match layout {
        Slice(Horizontal) => {
            let width = screen.size.width / f64::from(num_windows);
            (0..num_windows)
                .map(|i| CGRect {
                    origin: CGPoint {
                        x: screen.origin.x + f64::from(i) * width,
                        y: screen.origin.y,
                    },
                    size: CGSize {
                        width,
                        height: screen.size.height,
                    },
                })
                .collect()
        }
        Slice(Vertical) => todo!(),
        Bsp(Horizontal) => {
            let size = CGSize {
                width: screen.size.width / 2.0,
                height: screen.size.height,
            };

            let window_frame = CGRect { origin: screen.origin, size };
            let remainder = CGRect {
                origin: CGPoint {
                    x: screen.origin.x + size.width,
                    y: screen.origin.y,
                },
                size,
            };

            iter::once(window_frame)
                .chain(calculate_layout(
                    remainder,
                    num_windows - 1,
                    Layout::Bsp(Orientation::Vertical),
                ))
                .collect()
        }
        Layout::Bsp(Orientation::Vertical) => {
            let size = CGSize {
                width: screen.size.width,
                height: screen.size.height / 2.0,
            };

            let window_frame = CGRect { origin: screen.origin, size };
            let remainder = CGRect {
                origin: CGPoint {
                    x: screen.origin.x,
                    y: screen.origin.y + size.height,
                },
                size,
            };

            iter::once(window_frame)
                .chain(calculate_layout(
                    remainder,
                    num_windows - 1,
                    Layout::Bsp(Orientation::Horizontal),
                ))
                .collect()
        }
    }
}
