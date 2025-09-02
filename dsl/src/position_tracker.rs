//! Position tracking for better error reporting

use crate::error::{Position, Span};

/// Tracks position in source code during lexing/parsing
#[derive(Debug, Clone)]
pub struct PositionTracker {
    source: String,
    byte_offset: usize,
    line: usize,
    column: usize,
    line_starts: Vec<usize>,
}

impl PositionTracker {
    pub fn new(source: String) -> Self {
        let line_starts = Self::find_line_starts(&source);
        Self {
            source,
            byte_offset: 0,
            line: 1,
            column: 1,
            line_starts,
        }
    }
    
    fn find_line_starts(source: &str) -> Vec<usize> {
        let mut starts = vec![0];
        for (i, ch) in source.char_indices() {
            if ch == '\n' {
                starts.push(i + 1);
            }
        }
        starts
    }
    
    pub fn current_position(&self) -> Position {
        Position::new(self.line, self.column, self.byte_offset)
    }
    
    pub fn advance(&mut self, bytes: usize) {
        for _ in 0..bytes {
            if self.byte_offset < self.source.len() {
                if self.source.as_bytes()[self.byte_offset] == b'\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                self.byte_offset += 1;
            }
        }
    }
    
    pub fn advance_char(&mut self, ch: char) {
        self.advance(ch.len_utf8());
    }
    
    pub fn peek_char(&self) -> Option<char> {
        self.source[self.byte_offset..].chars().next()
    }
    
    pub fn is_at_end(&self) -> bool {
        self.byte_offset >= self.source.len()
    }
    
    pub fn remaining(&self) -> &str {
        &self.source[self.byte_offset..]
    }
    
    pub fn create_span(&self, start: Position) -> Span {
        Span::new(start, self.current_position())
    }
    
    /// Get a slice of the source between two positions
    pub fn slice(&self, start: usize, end: usize) -> &str {
        &self.source[start..end]
    }
    
    /// Find the line and column for a byte offset
    pub fn position_at(&self, byte_offset: usize) -> Position {
        let line = self.line_starts
            .iter()
            .rposition(|&start| start <= byte_offset)
            .unwrap_or(0) + 1;
        
        let line_start = self.line_starts[line - 1];
        let column = byte_offset - line_start + 1;
        
        Position::new(line, column, byte_offset)
    }
}