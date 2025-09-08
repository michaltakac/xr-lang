//! Quest 3 XR application host

// Android implementation
#[cfg(target_os = "android")]
mod android {
    use anyhow::Result;
    use winit::platform::android::activity::AndroidApp;

    #[no_mangle]
    pub fn android_main(app: AndroidApp) {
        if let Err(e) = quest_main(app) {
            log::error!("Quest app error: {}", e);
        }
    }

    fn quest_main(_app: AndroidApp) -> Result<()> {
        // Initialize logging
        android_logger::init_once(
            android_logger::Config::default().with_max_level(log::LevelFilter::Info),
        );

        log::info!("Starting XR-DSL Quest application");

        // TODO: Initialize OpenXR session
        // TODO: Initialize GPU context
        // TODO: Initialize JIT and VM
        // TODO: Main XR loop

        Ok(())
    }
}

// Non-Android stub to allow workspace builds/tests on desktop
#[cfg(not(target_os = "android"))]
mod stub {
    #[no_mangle]
    pub extern "C" fn android_main_stub() {}
}
