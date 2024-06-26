//! This module defines the [`Tree`][tree::Tree] data structure, on which all
//! layout logic is defined.

mod layout;
mod layout_tree;
mod selection;
mod tree;
mod window;

#[allow(unused_imports)]
pub use layout::{Direction, LayoutKind, Orientation};
pub use layout_tree::{LayoutId, LayoutTree};
