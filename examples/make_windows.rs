//! Example that creates numbered windows using AppKit and demonstrates the
//! effect of calling different system APIs on them.

use std::time::Duration;

use accessibility::{AXUIElement, AXUIElementActions, AXUIElementAttributes};
use nimbus_wm::sys::window_server::{self, WindowServerId};
use objc2::rc::Retained;
use objc2::runtime::ProtocolObject;
use objc2::{MainThreadMarker, MainThreadOnly, define_class, msg_send};
use objc2_app_kit::{
    NSApplication, NSApplicationActivationPolicy, NSApplicationDelegate, NSBackingStoreType,
    NSFont, NSTextAlignment, NSTextField, NSWindow, NSWindowStyleMask,
};
use objc2_foundation::{
    NSNotification, NSObject, NSObjectProtocol, NSPoint, NSRect, NSSize, NSString,
};

#[derive(Debug)]
struct Ivars;

define_class!(
    #[unsafe(super(NSObject))]
    #[thread_kind = MainThreadOnly]
    #[ivars = Ivars]
    struct AppDelegate;

    unsafe impl NSObjectProtocol for AppDelegate {}

    unsafe impl NSApplicationDelegate for AppDelegate {
        #[unsafe(method(applicationDidFinishLaunching:))]
        fn did_finish_launching(&self, _notification: &NSNotification) {
            println!("Creating four numbered windows...");

            let mtm = MainThreadMarker::from(self);

            // Create four windows
            let mut window_numbers = vec![];
            for i in 1..=4 {
                println!("Creating window {}", i);
                let number = create_numbered_window(i, mtm);
                window_numbers.push(number);
            }

            println!("All windows created successfully!");
            println!("The windows should now be visible on your screen.");
            println!("Press Ctrl+C to quit the application.");

            std::thread::spawn(move || demo(window_numbers));
        }

        #[unsafe(method(applicationWillTerminate:))]
        fn will_terminate(&self, _notification: &NSNotification) {
            println!("Application will terminate!");
        }
    }
);

impl AppDelegate {
    fn new(mtm: MainThreadMarker) -> Retained<Self> {
        let this = Self::alloc(mtm);
        let this = this.set_ivars(Ivars);
        unsafe { msg_send![super(this), init] }
    }
}

fn create_numbered_window(number: i32, mtm: MainThreadMarker) -> WindowServerId {
    // Calculate window position (staggered layout)
    let x = 100.0 + (number - 1) as f64 * 50.0;
    let y = 100.0 + (number - 1) as f64 * 50.0;
    let width = 300.0;
    let height = 200.0;

    let frame = NSRect {
        origin: objc2_foundation::NSPoint { x, y },
        size: NSSize { width, height },
    };

    // Create window with standard style
    let style_mask = NSWindowStyleMask::Titled
        | NSWindowStyleMask::Closable
        | NSWindowStyleMask::Miniaturizable
        | NSWindowStyleMask::Resizable;

    let window = unsafe {
        NSWindow::initWithContentRect_styleMask_backing_defer(
            NSWindow::alloc(mtm),
            frame,
            style_mask,
            NSBackingStoreType::Buffered,
            false,
        )
    };

    // Set window title
    let title_string = format!("Window {}", number);
    let title = NSString::from_str(&title_string);
    window.setTitle(&title);

    // Create a large text field with the number centered in the window
    let text_frame = NSRect {
        origin: NSPoint { x: 0.0, y: height / 2.0 - 50.0 },
        size: NSSize { width: width, height: 100.0 },
    };

    // Configure the text field
    let number_text = NSString::from_str(&number.to_string());
    let text_field = unsafe {
        let text_field = NSTextField::labelWithString(&number_text, mtm);
        text_field.setFrame(text_frame);
        text_field.setBezeled(false);
        text_field.setDrawsBackground(false);
        text_field.setAlignment(NSTextAlignment::Center);
        text_field.setFont(Some(&*NSFont::systemFontOfSize(72.0)));
        text_field
    };

    // Add text field to window's content view
    if let Some(content_view) = window.contentView() {
        unsafe {
            content_view.addSubview(&text_field);
        }
    }

    // Make window key and order front
    window.makeKeyAndOrderFront(None);

    // Don't release when closed (for memory management)
    unsafe {
        window.setReleasedWhenClosed(false);
    }
    WindowServerId::new(
        unsafe { window.windowNumber() }.try_into().expect("window number too large"),
    )
}

fn demo(window_numbers: Vec<WindowServerId>) {
    let sleep = |secs| {
        println!("Sleeping for {secs} seconds");
        std::thread::sleep(Duration::from_secs(secs))
    };

    sleep(2);

    let pid = std::process::id() as i32;
    let wsid = window_numbers[1];
    let ax_window = AXUIElement::application(pid)
        .windows()
        .expect("Could not get AX windows of current process")
        .iter()
        .find(|w| WindowServerId::try_from(&**w).ok() == Some(wsid))
        .expect("Could not find AX window for window #2")
        .clone();

    println!("Raising #2");
    ax_window.raise().expect("Raise failed");

    sleep(1);

    println!("Making #2 key window");
    window_server::make_key_window(pid, wsid).expect("make_key_window failed");
}

fn main() {
    let mtm = MainThreadMarker::new().expect("Must run on main thread");

    let app = NSApplication::sharedApplication(mtm);
    app.setActivationPolicy(NSApplicationActivationPolicy::Regular);

    // Configure the application delegate
    let delegate = AppDelegate::new(mtm);
    let object = ProtocolObject::from_ref(&*delegate);
    app.setDelegate(Some(object));

    // Run the app
    app.run();
}
