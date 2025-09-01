//! Simplified performance monitor that logs to console
//! This is a temporary implementation until text rendering is complete

use std::collections::VecDeque;
use std::time::{Duration, Instant};

const SAMPLE_COUNT: usize = 60;
const LOG_INTERVAL_MS: u64 = 1000;  // Log every second

pub struct SimplePerfMonitor {
    frame_times: VecDeque<Duration>,
    last_frame_time: Instant,
    last_log_time: Instant,
    
    // Stats
    current_fps: f32,
    avg_frame_time_ms: f32,
    min_frame_time_ms: f32,
    max_frame_time_ms: f32,
    
    // Counters
    draw_calls: u32,
    triangles_rendered: u32,
    
    enabled: bool,
}

impl SimplePerfMonitor {
    pub fn new() -> Self {
        Self {
            frame_times: VecDeque::with_capacity(SAMPLE_COUNT),
            last_frame_time: Instant::now(),
            last_log_time: Instant::now(),
            
            current_fps: 0.0,
            avg_frame_time_ms: 0.0,
            min_frame_time_ms: f32::MAX,
            max_frame_time_ms: 0.0,
            
            draw_calls: 0,
            triangles_rendered: 0,
            
            enabled: true,
        }
    }
    
    pub fn frame_start(&mut self) {
        if !self.enabled {
            return;
        }
        
        let now = Instant::now();
        let frame_time = now.duration_since(self.last_frame_time);
        self.last_frame_time = now;
        
        // Add to history
        self.frame_times.push_back(frame_time);
        if self.frame_times.len() > SAMPLE_COUNT {
            self.frame_times.pop_front();
        }
        
        // Log stats periodically
        if now.duration_since(self.last_log_time).as_millis() >= LOG_INTERVAL_MS as u128 {
            self.update_and_log_stats();
            self.last_log_time = now;
        }
    }
    
    fn update_and_log_stats(&mut self) {
        if self.frame_times.is_empty() {
            return;
        }
        
        let mut total_time = Duration::ZERO;
        let mut min_time = Duration::from_secs(1);
        let mut max_time = Duration::ZERO;
        
        for &frame_time in &self.frame_times {
            total_time += frame_time;
            min_time = min_time.min(frame_time);
            max_time = max_time.max(frame_time);
        }
        
        let avg_time = total_time / self.frame_times.len() as u32;
        
        self.avg_frame_time_ms = avg_time.as_secs_f32() * 1000.0;
        self.min_frame_time_ms = min_time.as_secs_f32() * 1000.0;
        self.max_frame_time_ms = max_time.as_secs_f32() * 1000.0;
        self.current_fps = if avg_time.as_secs_f32() > 0.0 {
            1.0 / avg_time.as_secs_f32()
        } else {
            0.0
        };
        
        // Create a visual FPS bar
        let fps_bar = self.create_fps_bar(self.current_fps);
        let triangles_k = self.triangles_rendered as f32 / 1000.0;
        
        // Log the stats with nice formatting
        println!("╔══════════════════════════════════════════════════════════╗");
        println!("║ 📊 Performance Monitor                                    ║");
        println!("╠══════════════════════════════════════════════════════════╣");
        println!("║ FPS: {:>6.1} {} ║", self.current_fps, fps_bar);
        println!("║ Frame: {:>5.2}ms (min: {:.2}, max: {:.2})              ║", 
                 self.avg_frame_time_ms, self.min_frame_time_ms, self.max_frame_time_ms);
        println!("║ Draw Calls: {:>4} | Triangles: {:>6.1}k                   ║", 
                 self.draw_calls, triangles_k);
        println!("╚══════════════════════════════════════════════════════════╝");
    }
    
    fn create_fps_bar(&self, fps: f32) -> String {
        let bar_width = 20;
        let max_fps = 120.0;
        let filled = ((fps / max_fps) * bar_width as f32).min(bar_width as f32) as usize;
        
        let mut bar = String::from("[");
        for i in 0..bar_width {
            if i < filled {
                bar.push('█');
            } else {
                bar.push('░');
            }
        }
        bar.push(']');
        
        // Color code based on FPS
        if fps >= 60.0 {
            format!("🟢{}", bar)  // Green for good
        } else if fps >= 30.0 {
            format!("🟡{}", bar)  // Yellow for okay
        } else {
            format!("🔴{}", bar)  // Red for poor
        }
    }
    
    pub fn record_draw_call(&mut self) {
        self.draw_calls += 1;
    }
    
    pub fn record_triangles(&mut self, count: u32) {
        self.triangles_rendered += count;
    }
    
    pub fn reset_frame_stats(&mut self) {
        self.draw_calls = 0;
        self.triangles_rendered = 0;
    }
    
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
        if self.enabled {
            println!("📊 Performance monitor enabled (logs every second)");
        } else {
            println!("📊 Performance monitor disabled");
        }
    }
    
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}