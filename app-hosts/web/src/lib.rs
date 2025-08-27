//! WebAssembly web host for XR-DSL 3D rendering

use wasm_bindgen::prelude::*;
use web_sys::console;

// Set up better panic messages in debug mode
#[cfg(feature = "console_error_panic_hook")]
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

// Use `wee_alloc` as the global allocator in release mode
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct WebRenderer {
    initialized: bool,
}

#[wasm_bindgen]
impl WebRenderer {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WebRenderer {
        WebRenderer { initialized: false }
    }

    pub async fn init(&mut self, _canvas: web_sys::HtmlCanvasElement) -> Result<(), JsValue> {
        console::log_1(&"Initializing WebGPU renderer...".into());
        
        // For now, just mark as initialized
        // Full WebGPU initialization would go here
        self.initialized = true;
        
        console::log_1(&"WebGPU renderer initialized!".into());
        Ok(())
    }

    pub fn render(&mut self) -> Result<(), JsValue> {
        if !self.initialized {
            return Err(JsValue::from_str("Renderer not initialized"));
        }
        
        // Render logic would go here
        Ok(())
    }

    pub fn resize(&mut self, _width: u32, _height: u32) -> Result<(), JsValue> {
        if !self.initialized {
            return Err(JsValue::from_str("Renderer not initialized"));
        }
        
        // Resize logic would go here
        Ok(())
    }

    pub fn load_dsl(&mut self, dsl_code: &str) -> Result<(), JsValue> {
        console::log_1(&format!("Loading DSL: {}", dsl_code).into());
        
        // DSL parsing and loading would go here
        match dsl::parse(dsl_code) {
            Ok(_ast) => {
                console::log_1(&"DSL parsed successfully!".into());
                Ok(())
            }
            Err(e) => {
                let error_msg = format!("DSL parse error: {}", e);
                console::error_1(&error_msg.clone().into());
                Err(JsValue::from_str(&error_msg))
            }
        }
    }
}

impl Default for WebRenderer {
    fn default() -> Self {
        Self::new()
    }
}