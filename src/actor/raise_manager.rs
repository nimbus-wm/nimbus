//! Manages raise requests.
//!
//! This actor sits behind the Reactor and is responsible for managing raise requests.
//! It ensures that windows are raised in the correct order and handles timeouts.

use crate::actor::app::{AppThreadHandle, Quiet, RaiseToken, Request, WindowId};
use crate::actor::mouse;
use crate::actor::reactor::{Event, Sender};
use crate::sys::timer::Timer;
use objc2_core_foundation::CGPoint;
use rustc_hash::FxHashMap as HashMap;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;
use tracing::{Span, debug, info, trace, warn};

/// Messages that can be sent to the raise manager
#[derive(Debug)]
pub enum RaiseManagerMessage {
    ProcessLayoutResponse {
        raise_windows: Vec<WindowId>,
        focus_window: Option<(WindowId, Option<CGPoint>)>,
        app_handles: HashMap<i32, AppThreadHandle>,
        raise_token: RaiseToken,
    },
    RaiseCompleted {
        window_id: WindowId,
        sequence_id: u64,
    },
    RaiseTimeout {
        sequence_id: u64,
    },
}

/// A synchronous raise manager for testing
pub struct RaiseManager {
    /// All the raise requests we are still processing. Note that we keep these
    /// separate from each other so that the windows are raised in the correct order.
    pending_sequences: Vec<PendingSequence>,
    next_sequence_id: u64,
    mouse_tx: Option<mouse::Sender>,
}

/// Tracks a pending sequence of raises
#[derive(Debug)]
pub struct PendingSequence {
    sequence_id: u64,
    pending_raises: HashSet<WindowId>,
    focus_window: Option<(WindowId, Option<CGPoint>)>,
    app_handles: HashMap<i32, AppThreadHandle>,
    raise_token: RaiseToken,
    started_at: Instant,
}

const TIMEOUT_DURATION: Duration = Duration::from_millis(250);

impl RaiseManager {
    /// Run the raise manager task.
    pub async fn run(
        mut rx: mpsc::UnboundedReceiver<RaiseManagerMessage>,
        events_tx: Sender,
        mouse_tx: Option<mouse::Sender>,
    ) {
        let mut raise_manager = RaiseManager::new();
        raise_manager.mouse_tx = mouse_tx;
        let mut timeout_timer = Timer::manual();

        loop {
            // Calculate next timeout timer if we have pending sequences.
            let timeout = if !raise_manager.pending_sequences.is_empty() {
                let earliest_elapsed = raise_manager
                    .pending_sequences
                    .iter()
                    .map(|seq| seq.started_at.elapsed())
                    .max()
                    .unwrap();

                TIMEOUT_DURATION.saturating_sub(earliest_elapsed)
            } else {
                Duration::MAX
            };
            timeout_timer.set_next_fire(timeout);

            tokio::select! {
                // Handle messages from reactor
                Some(msg) = rx.recv() => {
                    raise_manager.handle_message(msg);
                }

                // Handle timeout - send timeout event to reactor
                _ = timeout_timer.next() => {
                    // Find the sequence that timed out and send timeout event
                    for sequence in &raise_manager.pending_sequences {
                        if sequence.started_at.elapsed() >= TIMEOUT_DURATION {
                            // Send timeout event to reactor; this will get
                            // relayed back to us. We send these events through
                            // the reactor so that we can record/replay them.
                            let _ = events_tx.send((
                                tracing::Span::current(),
                                Event::RaiseTimeout { sequence_id: sequence.sequence_id }
                            ));
                            break;
                        }
                    }
                }
            }
        }
    }

    fn new() -> Self {
        Self {
            pending_sequences: Vec::new(),
            next_sequence_id: 1,
            mouse_tx: None,
        }
    }

    fn handle_message(&mut self, msg: RaiseManagerMessage) {
        match msg {
            RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows,
                focus_window,
                app_handles,
                raise_token,
            } => {
                debug!(
                    "Processing layout response with {} raise_windows",
                    raise_windows.len()
                );

                let sequence_id = self.next_sequence_id;
                self.next_sequence_id += 1;

                // Send all raise requests with completion notification
                let mut pending_raises = HashSet::default();

                for wid in raise_windows {
                    raise_token.set_pid(wid.pid);

                    if let Some(app_handle) = app_handles.get(&wid.pid) {
                        if app_handle
                            .send(Request::Raise(wid, raise_token.clone(), sequence_id, Quiet::Yes))
                            .is_ok()
                        {
                            pending_raises.insert(wid);
                        }
                    } else {
                        // App not found
                        warn!("App not found for window {:?}", wid);
                    }
                }

                if !pending_raises.is_empty() || focus_window.is_some() {
                    self.pending_sequences.push(PendingSequence {
                        sequence_id,
                        pending_raises,
                        focus_window,
                        app_handles,
                        raise_token,
                        started_at: Instant::now(),
                    });
                }
            }
            RaiseManagerMessage::RaiseCompleted { window_id, sequence_id } => {
                trace!("Raise completed for {:?} in sequence {}", window_id, sequence_id);

                // Find the sequence and remove this window from pending
                if let Some(sequence) =
                    self.pending_sequences.iter_mut().find(|s| s.sequence_id == sequence_id)
                {
                    sequence.pending_raises.remove(&window_id);
                }
            }
            RaiseManagerMessage::RaiseTimeout { sequence_id } => {
                trace!("Raise sequence {} timed out", sequence_id);

                // Find the sequence and clear all pending raises
                if let Some(sequence) =
                    self.pending_sequences.iter_mut().find(|s| s.sequence_id == sequence_id)
                {
                    warn!(
                        "Sequence {} timed out, clearing {} pending raises",
                        sequence_id,
                        sequence.pending_raises.len()
                    );
                    sequence.pending_raises.clear();
                }
            }
        }

        // Process sequences after handling each message
        self.process_pending_sequences();
    }

    /// Process pending sequences, handling completed raises and focus windows
    pub fn process_pending_sequences(&mut self) {
        let mut i = 0;
        // TODO: Use a more idiomatic looping mechanism.
        while i < self.pending_sequences.len() {
            let sequence = &mut self.pending_sequences[i];

            // If all raises are complete, process focus window if any.
            if sequence.pending_raises.is_empty() {
                if let Some((wid, warp)) = sequence.focus_window {
                    info!(focus_window = ?wid);
                    let app_handle = sequence.app_handles.get(&wid.pid);
                    if let Some(handle) = app_handle {
                        sequence.raise_token.set_pid(wid.pid);

                        if handle
                            .send(Request::Raise(
                                wid,
                                sequence.raise_token.clone(),
                                0, // No sequence ID for focus window (FIXME)
                                Quiet::No,
                            ))
                            .is_ok()
                        {
                            // FIXME: For focus window, we don't wait for completion
                            trace!("Focus window request sent");
                        } else {
                            warn!("Failed to send focus window request");
                        }

                        if let Some(warp) = warp {
                            if let Some(mouse_tx) = &self.mouse_tx {
                                _ = mouse_tx.send((Span::current(), mouse::Request::Warp(warp)));
                            }
                        }
                    }
                }

                // Remove completed sequence
                self.pending_sequences.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actor::app::{AppThreadHandle, RaiseToken, WindowId};
    use crate::sys::executor::Executor;
    use std::collections::HashMap;
    use tokio::sync::mpsc;

    #[test]
    fn test_raise_manager_handles_layout_response() {
        Executor::run(async {
            let mut raise_manager = RaiseManager::new();

            // Create test app handles
            let mut app_handles = HashMap::default();
            let (app_tx, _app_rx) = mpsc::unbounded_channel();
            let app_handle = AppThreadHandle::new_for_test(app_tx);
            app_handles.insert(1, app_handle);

            // Send a layout response
            let msg = RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows: vec![WindowId::new(1, 1), WindowId::new(1, 2)],
                focus_window: Some((WindowId::new(1, 3), None)),
                app_handles,
                raise_token: RaiseToken::default(),
            };

            // Handle the message synchronously
            raise_manager.handle_message(msg);
            // Verify that a pending sequence was created
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            let sequence = &raise_manager.pending_sequences[0];
            assert_eq!(sequence.sequence_id, 1);
            assert_eq!(sequence.pending_raises.len(), 2);
            assert!(sequence.focus_window.is_some());
        });
    }

    #[test]
    fn test_raise_completion_removes_pending_window() {
        Executor::run(async {
            let mut raise_manager = RaiseManager::new();

            // Create test app handles
            let mut app_handles = HashMap::default();
            let (app_tx, _app_rx) = mpsc::unbounded_channel();
            let app_handle = AppThreadHandle::new_for_test(app_tx);
            app_handles.insert(1, app_handle);

            // Send a layout response
            let layout_msg = RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows: vec![WindowId::new(1, 1), WindowId::new(1, 2)],
                focus_window: Some((WindowId::new(1, 3), None)),
                app_handles,
                raise_token: RaiseToken::default(),
            };

            raise_manager.handle_message(layout_msg);

            // Verify initial state
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            assert_eq!(raise_manager.pending_sequences[0].pending_raises.len(), 2);

            // Send completion for one window
            let completion_msg = RaiseManagerMessage::RaiseCompleted {
                window_id: WindowId::new(1, 1),
                sequence_id: 1,
            };

            raise_manager.handle_message(completion_msg);

            // Verify that one window was removed from pending
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            assert_eq!(raise_manager.pending_sequences[0].pending_raises.len(), 1);
            assert!(
                !raise_manager.pending_sequences[0].pending_raises.contains(&WindowId::new(1, 1))
            );
            assert!(
                raise_manager.pending_sequences[0].pending_raises.contains(&WindowId::new(1, 2))
            );
        });
    }

    #[test]
    fn test_timeout_clears_pending_raises() {
        Executor::run(async {
            let mut raise_manager = RaiseManager::new();

            // Create test app handles
            let mut app_handles = HashMap::default();
            let (app_tx, _app_rx) = mpsc::unbounded_channel();
            let app_handle = AppThreadHandle::new_for_test(app_tx);
            app_handles.insert(1, app_handle);

            // Send a layout response
            let layout_msg = RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows: vec![WindowId::new(1, 1)],
                focus_window: None,
                app_handles,
                raise_token: RaiseToken::default(),
            };

            raise_manager.handle_message(layout_msg);

            // Verify initial state
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            assert_eq!(raise_manager.pending_sequences[0].pending_raises.len(), 1);

            // Send timeout directly
            raise_manager.handle_message(RaiseManagerMessage::RaiseTimeout { sequence_id: 1 });

            // Verify that pending raises were cleared
            assert_eq!(raise_manager.pending_sequences.len(), 0);
        });
    }

    #[test]
    fn test_all_raises_complete_triggers_focus() {
        Executor::run(async {
            let mut raise_manager = RaiseManager::new();
            let (mouse_tx, mut mouse_rx) = mpsc::unbounded_channel();
            raise_manager.mouse_tx = Some(mouse_tx);

            // Create test app handles
            let mut app_handles = HashMap::default();
            let (app_tx, mut app_rx) = mpsc::unbounded_channel();
            let app_handle = AppThreadHandle::new_for_test(app_tx);
            app_handles.insert(1, app_handle);

            // Send a layout response with multiple raises and a focus window
            let layout_msg = RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows: vec![WindowId::new(1, 1), WindowId::new(1, 2)],
                focus_window: Some((WindowId::new(1, 3), Some(CGPoint::new(100.0, 200.0)))),
                app_handles,
                raise_token: RaiseToken::default(),
            };

            raise_manager.handle_message(layout_msg);

            // Verify initial state
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            assert_eq!(raise_manager.pending_sequences[0].pending_raises.len(), 2);

            // Send completions for both raise windows
            raise_manager.handle_message(RaiseManagerMessage::RaiseCompleted {
                window_id: WindowId::new(1, 1),
                sequence_id: 1,
            });

            raise_manager.handle_message(RaiseManagerMessage::RaiseCompleted {
                window_id: WindowId::new(1, 2),
                sequence_id: 1,
            });

            // Verify that the sequence was completed and removed
            assert_eq!(raise_manager.pending_sequences.len(), 0);

            // Check that focus window request was sent
            let mut focus_request_found = false;
            while let Ok((_, request)) = app_rx.try_recv() {
                if let Request::Raise(wid, _, _, quiet) = request {
                    if wid == WindowId::new(1, 3) && quiet == Quiet::No {
                        focus_request_found = true;
                        break;
                    }
                }
            }
            assert!(focus_request_found, "Focus window request should have been sent");

            // Check that warp request was sent
            let warp_request = mouse_rx.try_recv().expect("Warp request should have been sent");
            let mouse::Request::Warp(warp) = warp_request.1 else {
                panic!("Unexpected mouse request sent: {:?}", warp_request.1)
            };
            assert_eq!(warp, CGPoint::new(100.0, 200.0));
        });
    }

    #[test]
    fn test_timeout_clears_pending_and_triggers_focus() {
        Executor::run(async {
            let mut raise_manager = RaiseManager::new();

            // Create test app handles
            let mut app_handles = HashMap::default();
            let (app_tx, mut app_rx) = mpsc::unbounded_channel();
            let app_handle = AppThreadHandle::new_for_test(app_tx);
            app_handles.insert(1, app_handle);

            // Send a layout response
            let layout_msg = RaiseManagerMessage::ProcessLayoutResponse {
                raise_windows: vec![WindowId::new(1, 1), WindowId::new(1, 2)],
                focus_window: Some((WindowId::new(1, 3), None)),
                app_handles,
                raise_token: RaiseToken::default(),
            };

            raise_manager.handle_message(layout_msg);

            // Verify initial state
            assert_eq!(raise_manager.pending_sequences.len(), 1);
            assert_eq!(raise_manager.pending_sequences[0].pending_raises.len(), 2);

            // Send timeout for the sequence
            raise_manager.handle_message(RaiseManagerMessage::RaiseTimeout { sequence_id: 1 });

            // Verify that the sequence was completed and removed
            assert_eq!(raise_manager.pending_sequences.len(), 0);

            // Check that focus window request was sent after timeout
            let mut focus_request_found = false;
            while let Ok((_, request)) = app_rx.try_recv() {
                if let Request::Raise(wid, _, _, quiet) = request {
                    if wid == WindowId::new(1, 3) && quiet == Quiet::No {
                        focus_request_found = true;
                        break;
                    }
                }
            }

            assert!(
                focus_request_found,
                "Focus window request should have been sent after timeout"
            );
        });
    }
}
