use std::{cell::RefCell, rc::Rc, sync::Arc};

use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
use core_graphics::event::{
    CGEvent, CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement, CGEventType,
};
use icrate::Foundation::CGPoint;
use tracing::{debug, error, trace, warn, Span};

use crate::{
    config::Config,
    sys::{event, window_server},
};

use super::reactor::{self, Event};

#[derive(Debug)]
pub enum Request {
    Warp(CGPoint),
    /// The system resets the hidden state of the mouse each time the focused
    /// application changes. WmController sends us this request when that
    /// happens, so we can re-hide the mouse if it is supposed to be hidden.
    EnforceHidden,
}

pub struct Mouse {
    config: Arc<Config>,
    events_tx: reactor::Sender,
    requests_rx: Receiver,
    state: Rc<RefCell<State>>,
}

#[derive(Default)]
struct State {
    hidden: bool,
}

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, Request)>;
pub type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, Request)>;

pub fn channel() -> (Sender, Receiver) {
    tokio::sync::mpsc::unbounded_channel()
}

impl Mouse {
    pub fn new(config: Arc<Config>, events_tx: reactor::Sender, requests_rx: Receiver) -> Self {
        Mouse {
            config,
            events_tx,
            requests_rx,
            state: Rc::new(RefCell::new(State::default())),
        }
    }

    pub async fn run(mut self) {
        let current = CFRunLoop::get_current();
        let tx = self.events_tx.clone();
        let state = Rc::clone(&self.state);
        let tap = CGEventTap::new(
            CGEventTapLocation::Session,
            CGEventTapPlacement::HeadInsertEventTap,
            CGEventTapOptions::ListenOnly,
            vec![
                // Any event we want the mouse to be shown for.
                // Note that this does not include scroll events.
                CGEventType::LeftMouseDown,
                CGEventType::LeftMouseUp,
                CGEventType::RightMouseDown,
                CGEventType::RightMouseUp,
                CGEventType::MouseMoved,
                CGEventType::LeftMouseDragged,
                CGEventType::RightMouseDragged,
            ],
            move |_, event_type, event| {
                Self::on_event(event_type, event, &tx, &mut *state.borrow_mut());
                None
            },
        )
        .expect("Could not create event tap");

        let loop_source = tap.mach_port().create_runloop_source(0).unwrap();
        current.add_source(&loop_source, unsafe { kCFRunLoopCommonModes });

        // Callbacks will be dispatched by the run loop, which we assume is
        // running by the time this function is awaited.
        tap.enable();

        if self.config.settings.mouse_hides_on_focus {
            if let Err(e) = window_server::allow_hide_mouse() {
                error!(
                    "Could not enable mouse hiding: {e:?}. \
                    mouse_hides_on_focus will have no effect."
                );
            }
        }

        while let Some((_span, request)) = self.requests_rx.recv().await {
            let mut state = self.state.borrow_mut();
            match request {
                Request::Warp(point) => {
                    if let Err(e) = event::warp_mouse(point) {
                        warn!("Failed to warp mouse: {e:?}");
                    }
                    if self.config.settings.mouse_hides_on_focus && !state.hidden {
                        debug!("Hiding mouse");
                        if let Err(e) = event::hide_mouse() {
                            warn!("Failed to hide mouse: {e:?}");
                        }
                        state.hidden = true;
                    }
                }
                Request::EnforceHidden => {
                    if state.hidden {
                        if let Err(e) = event::hide_mouse() {
                            warn!("Failed to hide mouse: {e:?}");
                        }
                    }
                }
            }
        }
    }

    fn on_event(event_type: CGEventType, event: &CGEvent, tx: &reactor::Sender, state: &mut State) {
        if state.hidden {
            debug!("Showing mouse");
            if let Err(e) = event::show_mouse() {
                warn!("Failed to show mouse: {e:?}");
            }
            state.hidden = false;
        }
        match event_type {
            CGEventType::LeftMouseUp => {
                _ = tx.send((Span::current().clone(), Event::MouseUp));
            }
            CGEventType::MouseMoved => {
                let loc = event.location();
                trace!("Mouse moved {loc:?}");
            }
            _ => (),
        }
    }
}
