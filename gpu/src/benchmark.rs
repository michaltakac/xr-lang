//! Performance benchmarking and metrics tracking
//! Collects FPS, frame time, and rendering statistics

use std::collections::VecDeque;
use std::time::{Duration, Instant};
use std::fs::OpenOptions;
use std::io::Write;
use chrono::Local;

const SAMPLE_WINDOW: usize = 300;  // 5 seconds at 60 FPS
const WARMUP_FRAMES: usize = 60;   // Skip first second for warmup

#[derive(Debug, Clone)]
pub struct BenchmarkMetrics {
    pub timestamp: String,
    pub scene_name: String,
    pub total_objects: usize,
    pub rendered_objects: usize,
    pub culled_objects: usize,
    pub draw_calls: usize,
    pub fps_min: f32,
    pub fps_max: f32,
    pub fps_avg: f32,
    pub frame_time_min_ms: f32,
    pub frame_time_max_ms: f32,
    pub frame_time_avg_ms: f32,
    pub gpu_memory_mb: f32,
}

pub struct Benchmark {
    scene_name: String,
    frame_times: VecDeque<Duration>,
    frame_count: usize,
    last_frame_time: Instant,
    warmup_complete: bool,
    
    // Collected metrics
    fps_samples: Vec<f32>,
    frame_time_samples: Vec<f32>,
    
    // Current stats
    current_fps: f32,
    current_frame_time_ms: f32,
    
    // Tracking for benchmark completion
    benchmark_duration: Duration,
    benchmark_start: Option<Instant>,
}

impl Benchmark {
    pub fn new(scene_name: String) -> Self {
        Self {
            scene_name,
            frame_times: VecDeque::with_capacity(SAMPLE_WINDOW),
            frame_count: 0,
            last_frame_time: Instant::now(),
            warmup_complete: false,
            fps_samples: Vec::new(),
            frame_time_samples: Vec::new(),
            current_fps: 0.0,
            current_frame_time_ms: 0.0,
            benchmark_duration: Duration::from_secs(10),  // 10 second benchmark
            benchmark_start: None,
        }
    }
    
    /// Update frame timing
    pub fn frame_tick(&mut self) {
        let now = Instant::now();
        let frame_time = now.duration_since(self.last_frame_time);
        self.last_frame_time = now;
        
        self.frame_count += 1;
        
        // Skip warmup frames
        if self.frame_count < WARMUP_FRAMES {
            return;
        }
        
        if !self.warmup_complete {
            self.warmup_complete = true;
            self.benchmark_start = Some(now);
            println!("🔥 Benchmark warmup complete, starting measurements...");
        }
        
        // Add to rolling window
        self.frame_times.push_back(frame_time);
        if self.frame_times.len() > SAMPLE_WINDOW {
            self.frame_times.pop_front();
        }
        
        // Calculate current metrics
        if !self.frame_times.is_empty() {
            let total_time: Duration = self.frame_times.iter().sum();
            let avg_frame_time = total_time / self.frame_times.len() as u32;
            
            self.current_frame_time_ms = avg_frame_time.as_secs_f32() * 1000.0;
            self.current_fps = if avg_frame_time.as_secs_f32() > 0.0 {
                1.0 / avg_frame_time.as_secs_f32()
            } else {
                0.0
            };
            
            // Collect samples
            self.fps_samples.push(self.current_fps);
            self.frame_time_samples.push(self.current_frame_time_ms);
        }
    }
    
    /// Check if benchmark is complete
    pub fn is_complete(&self) -> bool {
        if let Some(start) = self.benchmark_start {
            Instant::now().duration_since(start) >= self.benchmark_duration
        } else {
            false
        }
    }
    
    /// Get current FPS
    pub fn get_current_fps(&self) -> f32 {
        self.current_fps
    }
    
    /// Get current frame time
    pub fn get_current_frame_time_ms(&self) -> f32 {
        self.current_frame_time_ms
    }
    
    /// Calculate final metrics
    pub fn calculate_metrics(
        &self,
        total_objects: usize,
        rendered_objects: usize,
        culled_objects: usize,
        draw_calls: usize,
    ) -> BenchmarkMetrics {
        let fps_min = self.fps_samples.iter().cloned().fold(f32::INFINITY, f32::min);
        let fps_max = self.fps_samples.iter().cloned().fold(f32::NEG_INFINITY, f32::max);
        let fps_avg = self.fps_samples.iter().sum::<f32>() / self.fps_samples.len() as f32;
        
        let frame_time_min_ms = self.frame_time_samples.iter().cloned().fold(f32::INFINITY, f32::min);
        let frame_time_max_ms = self.frame_time_samples.iter().cloned().fold(f32::NEG_INFINITY, f32::max);
        let frame_time_avg_ms = self.frame_time_samples.iter().sum::<f32>() / self.frame_time_samples.len() as f32;
        
        BenchmarkMetrics {
            timestamp: Local::now().format("%Y-%m-%d %H:%M:%S").to_string(),
            scene_name: self.scene_name.clone(),
            total_objects,
            rendered_objects,
            culled_objects,
            draw_calls,
            fps_min,
            fps_max,
            fps_avg,
            frame_time_min_ms,
            frame_time_max_ms,
            frame_time_avg_ms,
            gpu_memory_mb: 0.0,  // TODO: Implement GPU memory tracking
        }
    }
    
    /// Write metrics to CSV file
    pub fn write_to_file(metrics: &BenchmarkMetrics) -> std::io::Result<()> {
        let file_path = "benchmark_results.csv";
        let file_exists = std::path::Path::new(file_path).exists();
        
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(file_path)?;
        
        // Write header if file is new
        if !file_exists {
            writeln!(file, "timestamp,scene,total_objects,rendered,culled,draw_calls,fps_min,fps_max,fps_avg,frame_ms_min,frame_ms_max,frame_ms_avg,gpu_mb")?;
        }
        
        // Write data row
        writeln!(
            file,
            "{},{},{},{},{},{},{:.1},{:.1},{:.1},{:.2},{:.2},{:.2},{:.1}",
            metrics.timestamp,
            metrics.scene_name,
            metrics.total_objects,
            metrics.rendered_objects,
            metrics.culled_objects,
            metrics.draw_calls,
            metrics.fps_min,
            metrics.fps_max,
            metrics.fps_avg,
            metrics.frame_time_min_ms,
            metrics.frame_time_max_ms,
            metrics.frame_time_avg_ms,
            metrics.gpu_memory_mb,
        )?;
        
        println!("📊 Benchmark results written to {}", file_path);
        Ok(())
    }
    
    /// Print metrics to console
    pub fn print_metrics(metrics: &BenchmarkMetrics) {
        println!("\n═══════════════════════════════════════════════════════════════");
        println!("                    BENCHMARK RESULTS");
        println!("═══════════════════════════════════════════════════════════════");
        println!("Scene:          {}", metrics.scene_name);
        println!("Timestamp:      {}", metrics.timestamp);
        println!("───────────────────────────────────────────────────────────────");
        println!("OBJECTS:");
        println!("  Total:        {} objects", metrics.total_objects);
        println!("  Rendered:     {} objects", metrics.rendered_objects);
        println!("  Culled:       {} objects ({:.1}%)", 
            metrics.culled_objects,
            (metrics.culled_objects as f32 / metrics.total_objects as f32) * 100.0
        );
        println!("  Draw Calls:   {}", metrics.draw_calls);
        println!("───────────────────────────────────────────────────────────────");
        println!("PERFORMANCE:");
        println!("  FPS:");
        println!("    Min:        {:.1} fps", metrics.fps_min);
        println!("    Max:        {:.1} fps", metrics.fps_max);
        println!("    Average:    {:.1} fps", metrics.fps_avg);
        println!("  Frame Time:");
        println!("    Min:        {:.2} ms", metrics.frame_time_min_ms);
        println!("    Max:        {:.2} ms", metrics.frame_time_max_ms);
        println!("    Average:    {:.2} ms", metrics.frame_time_avg_ms);
        println!("═══════════════════════════════════════════════════════════════\n");
    }
}