use std::future;

use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
use core_graphics::event::{
    CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement, CGEventType,
};
use tracing::Span;

use super::reactor::{Event, Sender};

pub struct Mouse {
    tx: Sender,
}

impl Mouse {
    pub fn new(tx: Sender) -> Self {
        Mouse { tx }
    }

    pub async fn run(self) {
        let current = CFRunLoop::get_current();
        let tx = self.tx.clone();
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
        tap.enable();

        // All the work is done in callbacks dispatched by the run loop, which
        // we assume is running once this function is awaited.
        future::pending().await
    }
}
