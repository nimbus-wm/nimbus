use std::{cell::RefCell, mem::replace, rc::Rc, sync::Arc, time::Instant};

use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
use core_graphics::event::{
    CGEvent, CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement, CGEventType,
};
use icrate::Foundation::{CGPoint, MainThreadMarker, NSInteger};
use tracing::{debug, error, trace, warn, Span};

use super::reactor::{self, Event};
use crate::{
    config::Config,
    sys::{
        event,
        geometry::ToICrate,
        screen::CoordinateConverter,
        window_server::{self, get_window, WindowServerId},
    },
};

#[derive(Debug)]
pub enum Request {
    Warp(CGPoint),
    /// The system resets the hidden state of the mouse each time the focused
    /// application changes. WmController sends us this request when that
    /// happens, so we can re-hide the mouse if it is supposed to be hidden.
    EnforceHidden,
    ScreenParametersChanged(CoordinateConverter),
}

pub struct Mouse {
    config: Arc<Config>,
    events_tx: reactor::Sender,
    requests_rx: Option<Receiver>,
    state: RefCell<State>,
}

#[derive(Default)]
struct State {
    hidden: bool,
    above_window: Option<WindowServerId>,
    above_window_level: NSWindowLevel,
    converter: CoordinateConverter,
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
            requests_rx: Some(requests_rx),
            state: RefCell::new(State::default()),
        }
    }

    pub async fn run(mut self) {
        let mut requests_rx = self.requests_rx.take().unwrap();

        let this = Rc::new(self);
        let this_ = Rc::clone(&this);
        let current = CFRunLoop::get_current();
        let mtm = MainThreadMarker::new().unwrap();
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
                this_.on_event(event_type, event, mtm);
                None
            },
        )
        .expect("Could not create event tap");

        let loop_source = tap.mach_port().create_runloop_source(0).unwrap();
        current.add_source(&loop_source, unsafe { kCFRunLoopCommonModes });

        // Callbacks will be dispatched by the run loop, which we assume is
        // running by the time this function is awaited.
        tap.enable();

        if this.config.settings.mouse_hides_on_focus {
            if let Err(e) = window_server::allow_hide_mouse() {
                error!(
                    "Could not enable mouse hiding: {e:?}. \
                    mouse_hides_on_focus will have no effect."
                );
            }
        }

        while let Some((_span, request)) = requests_rx.recv().await {
            this.on_request(request);
        }
    }

    fn on_request(self: &Rc<Self>, request: Request) {
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
            Request::ScreenParametersChanged(converter) => state.converter = converter,
        }
    }

    fn on_event(self: &Rc<Self>, event_type: CGEventType, event: &CGEvent, mtm: MainThreadMarker) {
        let mut state = self.state.borrow_mut();
        if state.hidden {
            debug!("Showing mouse");
            if let Err(e) = event::show_mouse() {
                warn!("Failed to show mouse: {e:?}");
            }
            state.hidden = false;
        }
        match event_type {
            CGEventType::LeftMouseUp => {
                _ = self.events_tx.send((Span::current().clone(), Event::MouseUp));
            }
            CGEventType::MouseMoved if self.config.settings.focus_follows_mouse => {
                let loc = event.location();
                trace!("Mouse moved {loc:?}");
                if let Some(wsid) = state.track_mouse_move(loc.to_icrate(), mtm) {
                    _ = self.events_tx.send((Span::current(), Event::MouseMovedOverWindow(wsid)));
                }
            }
            _ => (),
        }
    }
}

impl State {
    fn track_mouse_move(&mut self, loc: CGPoint, mtm: MainThreadMarker) -> Option<WindowServerId> {
        // This takes on the order of 200µs, which can be a while for something
        // that may run many times a second on the main thread. For now this
        // isn't a problem, but when we start doing anything with UI we might
        // want to compute this internally.
        let new_window = trace_misc("NSWindow", || {
            window_server::get_window_at_point(loc, self.converter, mtm)
        });
        if self.above_window == new_window {
            return None;
        }

        debug!("Mouse is now above window {new_window:?}");
        let old_window = replace(&mut self.above_window, new_window);
        let new_window_level = new_window
            .and_then(|id| trace_misc("get_window", || get_window(id)))
            .map(|info| info.layer as NSWindowLevel)
            .unwrap_or(NSWindowLevel::MIN);
        let old_window_level = replace(&mut self.above_window_level, new_window_level);

        debug!(?old_window, ?old_window_level, ?new_window, ?new_window_level);

        // Don't dismiss popups when the mouse moves off them.
        //
        // The only reason this is NSMainMenuWindowLevel and not
        // NSPopUpMenuWindowLevel is that there seems to be a gap between the
        // menu bar and the actual menu pop-ups when a menu is opened. When the
        // mouse goes over this gap, the system reports it to be over whatever
        // window happens to be below the menu bar and behind the pop-up, and so
        // we would dismiss the pop-up. First observed on 13.5.2.
        if old_window_level >= NSMainMenuWindowLevel {
            return None;
        }

        // Don't focus windows outside the "normal" range.
        if !(0..NSPopUpMenuWindowLevel).contains(&new_window_level) {
            return None;
        }

        new_window
    }
}

fn trace_misc<T>(desc: &str, f: impl FnOnce() -> T) -> T {
    let start = Instant::now();
    let out = f();
    let end = Instant::now();
    trace!(time = ?(end - start), "{desc}");
    out
}

/// https://developer.apple.com/documentation/appkit/nswindowlevel?language=objc
pub type NSWindowLevel = NSInteger;
#[allow(non_upper_case_globals)]
pub const NSMainMenuWindowLevel: NSWindowLevel = 24;
#[allow(non_upper_case_globals)]
pub const NSPopUpMenuWindowLevel: NSWindowLevel = 101;
