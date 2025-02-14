use accessibility_sys::pid_t;
use icrate::Foundation::{CGPoint, CGRect, CGSize};
use std::collections::BTreeMap;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tracing::Span;

use crate::{
    actor::app::{AppThreadHandle, Request, WindowId},
    sys::{
        app::{AppInfo, WindowInfo},
        geometry::SameAs,
        window_server::{WindowServerId, WindowServerInfo},
    },
};

use super::{Event, Reactor, Requested, TransactionId};

pub struct Apps(
    UnboundedSender<(Span, Request)>,
    UnboundedReceiver<(Span, Request)>,
);
impl Apps {
    pub fn new() -> Apps {
        let (tx, rx) = unbounded_channel();
        Apps(tx, rx)
    }

    pub fn make_app(&mut self, pid: pid_t, windows: Vec<WindowInfo>) -> Vec<Event> {
        self.make_app_with_opts(pid, windows, None, false, true)
    }

    pub fn make_app_with_opts(
        &mut self,
        pid: pid_t,
        windows: Vec<WindowInfo>,
        main_window: Option<WindowId>,
        is_frontmost: bool,
        with_ws_info: bool,
    ) -> Vec<Event> {
        let handle = AppThreadHandle::new_for_test(self.0.clone());
        vec![Event::ApplicationLaunched {
            pid,
            info: AppInfo {
                bundle_id: Some(format!("com.testapp{pid}")),
                localized_name: Some(format!("TestApp{pid}")),
            },
            handle,
            is_frontmost,
            main_window,
            window_server_info: if with_ws_info {
                windows
                    .iter()
                    .map(|info| WindowServerInfo {
                        pid,
                        id: info.sys_id.unwrap(),
                        layer: 0,
                        frame: info.frame,
                    })
                    .collect()
            } else {
                Default::default()
            },
            visible_windows: (1..).map(|idx| WindowId::new(pid, idx)).zip(windows).collect(),
        }]
    }

    pub fn requests(&mut self) -> Vec<Request> {
        let mut requests = Vec::new();
        while let Ok((_, req)) = self.1.try_recv() {
            requests.push(req);
        }
        requests
    }

    pub fn simulate_until_quiet(&mut self, reactor: &mut Reactor) {
        let mut requests = self.requests();
        while !requests.is_empty() {
            for event in simulate_events_for_requests(requests).0 {
                reactor.handle_event(event);
            }
            requests = self.requests();
        }
    }
}

pub fn make_window(idx: usize) -> WindowInfo {
    WindowInfo {
        is_standard: true,
        title: format!("Window{idx}"),
        frame: CGRect::new(
            CGPoint::new(100.0 * f64::from(idx as u32), 100.0),
            CGSize::new(50.0, 50.0),
        ),
        // TODO: This is wrong and conflicts with windows from other apps.
        sys_id: Some(WindowServerId::new(idx as u32)),
    }
}

pub fn make_windows(count: usize) -> Vec<WindowInfo> {
    (1..=count).map(make_window).collect()
}

#[derive(Default, PartialEq, Debug)]
pub struct WindowState {
    pub last_seen_txid: TransactionId,
    pub animating: bool,
    pub frame: CGRect,
}

pub fn simulate_events_for_requests(
    requests: Vec<Request>,
) -> (Vec<Event>, BTreeMap<WindowId, WindowState>) {
    let mut events = vec![];
    let mut windows: BTreeMap<WindowId, WindowState> = BTreeMap::new();

    for request in requests {
        match request {
            Request::Terminate => break,
            Request::GetVisibleWindows => {}
            Request::SetWindowFrame(wid, frame, txid) => {
                let window = windows.entry(wid).or_default();
                window.last_seen_txid = txid;
                let old_frame = window.frame;
                window.frame = frame;
                if !window.animating && !old_frame.same_as(frame) {
                    events.push(Event::WindowFrameChanged(
                        wid,
                        frame,
                        txid,
                        Requested(true),
                        None,
                    ));
                }
            }
            Request::SetWindowPos(wid, pos, txid) => {
                let window = windows.entry(wid).or_default();
                window.last_seen_txid = txid;
                let old_frame = window.frame;
                window.frame.origin = pos;
                if !window.animating && !old_frame.same_as(window.frame) {
                    events.push(Event::WindowFrameChanged(
                        wid,
                        window.frame,
                        txid,
                        Requested(true),
                        None,
                    ));
                }
            }
            Request::BeginWindowAnimation(wid) => {
                windows.entry(wid).or_default().animating = true;
            }
            Request::EndWindowAnimation(wid) => {
                let window = windows.entry(wid).or_default();
                window.animating = false;
                events.push(Event::WindowFrameChanged(
                    wid,
                    window.frame,
                    window.last_seen_txid,
                    Requested(true),
                    None,
                ));
            }
            Request::Raise(_, _, _, _) => todo!(),
        }
    }

    (events, windows)
}
