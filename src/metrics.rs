use std::time::Duration;

use tracing_timing::{group, Histogram};

pub type TimingLayer = tracing_timing::TimingLayer<group::ByName, group::ByMessage>;

#[derive(Debug, Clone)]
pub enum MetricsCommand {
    ShowTiming,
}

pub fn timing_layer() -> TimingLayer {
    tracing_timing::Builder::default()
        //.events(group::ByName)
        .layer(|| Histogram::new_with_max(100_000_000, 2).unwrap())
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
