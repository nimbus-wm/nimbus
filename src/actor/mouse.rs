use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
use core_graphics::event::{
    CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement, CGEventType,
};
use icrate::Foundation::CGPoint;
use tracing::{warn, Span};

use crate::sys::event;

use super::reactor::{self, Event};

#[derive(Debug)]
pub enum Request {
    Warp(CGPoint),
}

pub struct Mouse {
    events_tx: reactor::Sender,
    requests_rx: Receiver,
}

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, Request)>;
pub type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, Request)>;

pub fn channel() -> (Sender, Receiver) {
    tokio::sync::mpsc::unbounded_channel()
}

impl Mouse {
    pub fn new(events_tx: reactor::Sender, requests_rx: Receiver) -> Self {
        Mouse { events_tx, requests_rx }
    }

    pub async fn run(mut self) {
        let current = CFRunLoop::get_current();
        let tx = self.events_tx.clone();
        let tap = CGEventTap::new(
            CGEventTapLocation::Session,
            CGEventTapPlacement::HeadInsertEventTap,
            CGEventTapOptions::ListenOnly,
            vec![CGEventType::LeftMouseUp],
            move |_, _, _| {
                _ = tx.send((Span::current().clone(), Event::MouseUp));
                None
            },
        )
        .expect("Could not create event tap");

        let loop_source = tap.mach_port().create_runloop_source(0).unwrap();
        current.add_source(&loop_source, unsafe { kCFRunLoopCommonModes });

        // All the work is done in callbacks dispatched by the run loop, which
        // we assume is running once this function is awaited.
        tap.enable();

        while let Some((_span, request)) = self.requests_rx.recv().await {
            match request {
                Request::Warp(point) => {
                    if let Err(e) = event::warp_mouse(point) {
                        warn!("Failed to warp mouse: {e:?}");
                    }
                }
            }
        }
    }
}
