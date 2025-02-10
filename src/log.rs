use std::time::Duration;

use serde::{Deserialize, Serialize};
use tracing_subscriber::{
    layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer, Registry,
};
use tracing_timing::{group, Histogram};
use tracing_tree::time::UtcDateTime;

pub fn init_logging() {
    tracing_subscriber::registry()
        .with(tree_layer())
        .with(timing_layer())
        .with(EnvFilter::from_default_env())
        .init();
}

pub fn init_logging_for_test() {
    let _ = tracing_subscriber::registry()
        .with(tree_layer().with_filter(EnvFilter::from_default_env()))
        .with(span_tracker::SpanTracker::default())
        .try_init();
}

pub fn tree_layer() -> impl Layer<Registry> {
    tracing_tree::HierarchicalLayer::default()
        .with_indent_amount(2)
        .with_indent_lines(true)
        .with_deferred_spans(true)
        .with_span_retrace(true)
        .with_targets(true)
        .with_timer(UtcDateTime::default())
}

type TimingLayer = tracing_timing::TimingLayer<group::ByName, group::ByMessage>;

fn timing_layer() -> TimingLayer {
    tracing_timing::Builder::default()
        //.events(group::ByName)
        .layer(|| Histogram::new_with_max(100_000_000, 2).unwrap())
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum MetricsCommand {
    ShowTiming,
}

pub fn handle_command(command: MetricsCommand) {
    match command {
        MetricsCommand::ShowTiming => show_timing(),
    }
}

pub fn show_timing() {
    tracing::dispatcher::get_default(|d| {
        let timing_layer = d.downcast_ref::<TimingLayer>().unwrap();
        print_histograms(timing_layer);
    })
}

fn print_histograms(timing_layer: &TimingLayer) {
    timing_layer.force_synchronize();
    timing_layer.with_histograms(|hs| {
        println!("\nHistograms:\n");
        for (span, hs) in hs {
            for (event, h) in hs {
                let ns = |nanos| Duration::from_nanos(nanos);
                println!("{span} -> {event} ({} events)", h.len());
                println!("    mean: {:?}", ns(h.mean() as u64));
                println!("    min: {:?}", ns(h.min()));
                println!("    p50: {:?}", ns(h.value_at_quantile(0.50)));
                println!("    p90: {:?}", ns(h.value_at_quantile(0.90)));
                println!("    p99: {:?}", ns(h.value_at_quantile(0.99)));
                println!("    max: {:?}", ns(h.max()));
            }
        }
        println!();
    });
}

pub mod span_tracker {
    use crate::system::sync::{Condvar, Mutex};

    use rustc_hash::FxHashSet;
    use tracing::{span, Subscriber};
    use tracing_subscriber::Layer;

    #[derive(Default)]
    pub struct SpanTracker {
        spans: Mutex<FxHashSet<span::Id>>,
        empty: Condvar,
    }

    pub fn wait_for_spans() {
        tracing::dispatcher::get_default(|d| {
            let tracker = d.downcast_ref::<SpanTracker>().unwrap();
            tracker.wait_on(|| ())
        })
    }

    #[track_caller]
    pub fn wait_on<R>(f: impl FnOnce() -> R) -> R {
        let mut f = Some(f); // work around weird FnMut requirement on get_default
        tracing::dispatcher::get_default(|d| {
            let tracker = d.downcast_ref::<SpanTracker>().unwrap();
            if !tracker.spans.lock().unwrap().is_empty() {
                panic!("called wait_on with live spans");
            }
            tracker.wait_on(f.take().unwrap())
        })
    }

    impl<S: Subscriber> Layer<S> for SpanTracker {
        fn on_new_span(
            &self,
            _attrs: &span::Attributes<'_>,
            id: &span::Id,
            _ctx: tracing_subscriber::layer::Context<'_, S>,
        ) {
            self.spans.lock().unwrap().insert(id.clone());
        }

        fn on_id_change(
            &self,
            _old: &span::Id,
            _new: &span::Id,
            _ctx: tracing_subscriber::layer::Context<'_, S>,
        ) {
            // It's not clear what to do in this situation.
            panic!("unimplemented")
        }

        fn on_close(&self, id: span::Id, _ctx: tracing_subscriber::layer::Context<'_, S>) {
            let mut spans = self.spans.lock().unwrap();
            if !spans.remove(&id) {
                panic!("unknown span id closed: {id:?}");
            }
            if spans.is_empty() {
                self.empty.notify_all();
            }
        }
    }

    impl SpanTracker {
        fn wait_on<R>(&self, f: impl FnOnce() -> R) -> R {
            let result = f();

            let mut spans = self.spans.lock().unwrap();
            while !spans.is_empty() {
                spans = self.empty.wait(spans).unwrap();
            }

            result
        }
    }
}
