//! Utilities for interfacing with OS-specific APIs.

#![cfg_attr(test, allow(dead_code))]

pub mod app;
pub mod event;
pub mod executor;
pub mod geometry;
pub mod observer;
pub mod run_loop;
pub mod screen;
pub mod window_server;
