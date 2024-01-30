use std::iter;

use icrate::Foundation::{CGPoint, CGRect, CGSize};

use crate::reactor::{AppInfo, WindowInfo};

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

pub fn calculate_layout(
    screen: CGRect,
    windows: &[(&AppInfo, &WindowInfo)],
    layout: Layout,
) -> Vec<CGRect> {
    use Layout::*;
    use Orientation::*;
    let num_windows: u32 = windows.len().try_into().unwrap();
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
                    &windows[1..],
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
                    &windows[1..],
                    Layout::Bsp(Orientation::Horizontal),
                ))
                .collect()
        }
    }
}
