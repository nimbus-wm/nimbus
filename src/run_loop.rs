//! Helpers for managing run loops.

use std::{
    ffi::c_void,
    mem,
    ops::Deref,
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
};

use core_foundation::{
    base::TCFType,
    mach_port::CFIndex,
    runloop::{
        CFRunLoop, CFRunLoopSource, CFRunLoopSourceContext, CFRunLoopSourceCreate,
        CFRunLoopSourceSignal, CFRunLoopWakeUp,
    },
};

/// A core foundation run loop source.
///
/// This type primarily exists for the purpose of managing manual sources, which
/// can be used for signaling code that blocks on a run loop.
///
/// More information is available in the Apple documentation at
/// https://developer.apple.com/documentation/corefoundation/cfrunloopsource-rhr.
#[derive(Clone, PartialEq)]
pub struct RunLoopSource(CFRunLoopSource);

struct Handler<F: Send + Sync> {
    ref_count: AtomicUsize,
    func: F,
}

// SAFETY:
// - CFRunLoopSource is an ObjC object, which are Send.
// - We conceptually own a Handler and may give references to it to other threads.
//   The Send + Sync bounds on its type parameter ensure this is safe:
//   - Sync so we can call the function from other threads without synchronization.
//   - Send so we can drop the function on other threads.
unsafe impl Send for RunLoopSource {}

impl RunLoopSource {
    /// Creates a manual source for a run loop.
    ///
    /// The supplied function `f` is called inside the run loop when this source
    /// has been signalled and the run loop is awake.
    ///
    /// Note that the handler is not
    pub fn new_with_handler<F: Fn() + Send + Sync + 'static>(order: CFIndex, handler: F) -> Self {
        let handler = Box::into_raw(Box::new(Handler {
            ref_count: AtomicUsize::new(0),
            func: handler,
        }));

        extern "C" fn perform<F: Fn() + Send + Sync + 'static>(info: *const c_void) {
            let handler = unsafe { &*(info as *mut Handler<F>) };
            (handler.func)();
        }
        extern "C" fn retain<F: Send + Sync>(info: *const c_void) -> *const c_void {
            let handler = unsafe { &*(info as *mut Handler<F>) };
            handler.ref_count.fetch_add(1, Ordering::Acquire);
            info
        }
        extern "C" fn release<F: Send + Sync>(info: *const c_void) {
            let handler = unsafe { &*(info as *mut Handler<F>) };
            if handler.ref_count.fetch_sub(1, Ordering::Release) == 1 {
                mem::drop(unsafe { Box::from_raw(info as *mut Handler<F>) });
            }
        }

        let mut context = CFRunLoopSourceContext {
            version: 0,
            info: handler as *mut c_void,
            retain: Some(retain::<F>),
            release: Some(release::<F>),
            copyDescription: None,
            equal: None,
            hash: None,
            schedule: None,
            cancel: None,
            perform: perform::<F>,
        };
        unsafe {
            let obj = CFRunLoopSourceCreate(ptr::null(), order, &mut context as *mut _);
            Self(CFRunLoopSource::wrap_under_create_rule(obj))
        }
    }

    /// Signal this source and wake the supplied run loop.
    ///
    /// Usually when a source is signaled manually, the corresponding run loop
    /// also needs to be awoken. This method performs both actions. Make sure
    /// that this source has actually been added to the supplied run loop, or
    /// the handler may not be called.
    ///
    /// Multiple signals may be collapsed into a single call of the handler.
    pub fn signal_and_wake(&self, runloop: &CFRunLoop) {
        self.signal_only();
        unsafe {
            CFRunLoopWakeUp(runloop.as_concrete_TypeRef());
        }
    }

    /// Signal this source without waking a corresponding run loop.
    ///
    /// When this method is called, it is up to the caller to wake a run loop
    /// that the source has been added to.
    ///
    /// Multiple signals may be collapsed into a single call of the handler.
    pub fn signal_only(&self) {
        unsafe {
            CFRunLoopSourceSignal(self.0.as_concrete_TypeRef());
        }
    }
}

impl Deref for RunLoopSource {
    type Target = CFRunLoopSource;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use std::{
        sync::{
            atomic::{AtomicI32, AtomicUsize, Ordering},
            mpsc::{channel, Receiver, Sender},
            Arc,
        },
        thread,
    };

    use core_foundation::{
        base::TCFType,
        runloop::{kCFRunLoopCommonModes, CFRunLoop, CFRunLoopWakeUp},
        string::CFStringRef,
    };

    use super::RunLoopSource;

    fn common_modes() -> CFStringRef {
        unsafe { kCFRunLoopCommonModes }
    }

    #[test]
    fn it_works_when_added_inside_the_thread() {
        let data = Arc::new(AtomicI32::new(0));
        let handler_data = data.clone();
        let (tx, rx) = channel();
        let thread = std::thread::spawn(move || {
            let source = RunLoopSource::new_with_handler(0, move || {
                println!("handler");
                handler_data.fetch_add(42, Ordering::SeqCst);
                CFRunLoop::get_current().stop();
                println!("done");
            });
            CFRunLoop::get_current().add_source(&source, common_modes());
            tx.send((source, CFRunLoop::get_current())).unwrap();
            CFRunLoop::run_current();
        });
        let (source, runloop) = rx.recv().unwrap();
        source.signal_and_wake(&runloop);
        thread.join().unwrap();
        assert_eq!(42, data.load(Ordering::SeqCst));
    }

    #[test]
    fn it_works_when_added_outside_the_thread() {
        let data = Arc::new(AtomicI32::new(0));
        let (tx, rx) = channel();
        let thread = std::thread::spawn(move || {
            // We have to add a dummy source here, because run_current() will
            // exit immediately if no sources have been added yet.
            let dummy = RunLoopSource::new_with_handler(0, || ());
            CFRunLoop::get_current().add_source(&dummy, common_modes());
            tx.send(CFRunLoop::get_current()).unwrap();
            CFRunLoop::run_current();
        });
        let runloop = rx.recv().unwrap();
        let handler_data = data.clone();
        let source = RunLoopSource::new_with_handler(0, move || {
            handler_data.fetch_add(42, Ordering::SeqCst);
            CFRunLoop::get_current().stop();
        });
        runloop.add_source(&source, common_modes());
        source.signal_and_wake(&runloop);
        thread.join().unwrap();
        assert_eq!(42, data.load(Ordering::SeqCst));
    }

    #[test]
    fn it_works_with_multiple_run_loops() {
        const NUM_THREADS: usize = 4;
        println!();
        let wakeups = Arc::new(AtomicUsize::new(0));
        let (tx, rx) = channel();
        let threads: Vec<_> = (0..NUM_THREADS)
            .map(|_| {
                let tx = tx.clone();
                std::thread::spawn(move || {
                    println!("thread {:?} starting", thread::current().id());
                    // We have to add a dummy source here, because run_current() will
                    // exit immediately if no sources have been added yet.
                    let dummy = RunLoopSource::new_with_handler(0, || ());
                    CFRunLoop::get_current().add_source(&dummy, common_modes());
                    tx.send(Some(CFRunLoop::get_current())).unwrap();
                    CFRunLoop::run_current();
                    // Let the main thread know we are exiting.
                    tx.send(None).unwrap();
                    println!("thread {:?} exiting", thread::current().id());
                })
            })
            .collect();
        let handler_wakeups = wakeups.clone();
        let (drop_tracker, drop_signaler) = DropTracker::new();
        let source = RunLoopSource::new_with_handler(0, move || {
            let _signaller = &drop_signaler;
            println!("handling source on thread {:?}", thread::current().id());
            handler_wakeups.fetch_add(1, Ordering::SeqCst);
            CFRunLoop::get_current().stop();
        });
        let runloops: Vec<_> = (0..NUM_THREADS)
            .map(|_| rx.recv().unwrap().unwrap())
            .collect();
        for runloop in &runloops {
            runloop.add_source(&source, common_modes());
        }
        for _ in 0..NUM_THREADS {
            println!("signaling");
            source.signal_only();
            // We must signal all runloops, because we don't track which have exited.
            runloops
                .iter()
                .for_each(|rl| unsafe { CFRunLoopWakeUp(rl.as_concrete_TypeRef()) });
            // Wait until the thread exits. We need this synchronization step,
            // otherwise a thread might "eat" multiple signals. A source is not
            // a sempahore!
            let None = rx.recv().unwrap() else {
                panic!("should have received None")
            };
        }
        for thread in threads {
            println!("joining thread {:?}", thread.thread().id());
            thread.join().unwrap();
        }
        assert_eq!(NUM_THREADS, wakeups.load(Ordering::SeqCst));
        drop(source);
        drop(runloops);
        drop_tracker.wait_for_drop();
    }

    struct DropTracker(Arc<AtomicUsize>, Receiver<()>);
    impl DropTracker {
        fn new() -> (DropTracker, DropSignaller) {
            let (tx, rx) = channel();
            let tracker = DropTracker(Default::default(), rx);
            let signaller = DropSignaller(tracker.0.clone(), tx);
            (tracker, signaller)
        }
        fn wait_for_drop(self) {
            self.1.recv().unwrap();
            assert_eq!(1, self.0.load(Ordering::SeqCst));
            assert!(
                Arc::into_inner(self.0).is_some(),
                "Another clone of our Arc exists somewhere!"
            );
            assert!(
                self.1.recv().is_err(),
                "Another clone of our sender exists somewhere!",
            );
        }
    }

    struct DropSignaller(Arc<AtomicUsize>, Sender<()>);
    impl Drop for DropSignaller {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
            self.1.send(()).unwrap();
        }
    }
}
