//! Enhanced parser with full position tracking

use crate::ast::*;
use crate::error::{DslError, DslResult, ErrorKind, Position, Span};
use crate::position_tracker::PositionTracker;
use crate::parser_helpers::*;

#[derive(Debug, Clone)]
pub enum Token {
    LPar(Span),
    RPar(Span),
    Sym(String, Span),
    Str(String, Span),
    Num(f32, Span),
    Int(i32, Span),
    True(Span),
    False(Span),
    Eof,
}

impl Token {
    fn span(&self) -> Option<Span> {
        match self {
            Token::LPar(s) | Token::RPar(s) | Token::Sym(_, s) | Token::Str(_, s) 
            | Token::Num(_, s) | Token::Int(_, s) | Token::True(s) | Token::False(s) => Some(*s),
            Token::Eof => None,
        }
    }
}

pub struct EnhancedLexer {
    tracker: PositionTracker,
    peeked: Option<Token>,
}

impl EnhancedLexer {
    pub fn new(source: String) -> Self {
        Self {
            tracker: PositionTracker::new(source),
            peeked: None,
        }
    }
    
    pub fn peek(&mut self) -> DslResult<Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token()?);
        }
        Ok(self.peeked.clone().unwrap())
    }
    
    pub fn next(&mut self) -> DslResult<Token> {
        if let Some(token) = self.peeked.take() {
            Ok(token)
        } else {
            self.next_token()
        }
    }
    
    fn next_token(&mut self) -> DslResult<Token> {
        self.skip_whitespace_and_comments();
        
        if self.tracker.is_at_end() {
            return Ok(Token::Eof);
        }
        
        let start = self.tracker.current_position();
        
        match self.tracker.peek_char() {
            Some('(') => {
                self.tracker.advance_char('(');
                Ok(Token::LPar(self.tracker.create_span(start)))
            }
            Some(')') => {
                self.tracker.advance_char(')');
                Ok(Token::RPar(self.tracker.create_span(start)))
            }
            Some('"') => self.read_string(start),
            Some('-') => {
                // Check if this is a negative number or just the minus operator
                let remaining = self.tracker.remaining();
                if remaining.len() > 1 {
                    let next_ch = remaining.chars().nth(1);
                    if let Some(ch) = next_ch {
                        if ch.is_digit(10) {
                            // It's a negative number like -5
                            self.read_number(start)
                        } else if ch == '.' && remaining.len() > 2 {
                            // Check for -.5 pattern
                            if let Some(ch2) = remaining.chars().nth(2) {
                                if ch2.is_digit(10) {
                                    self.read_number(start)
                                } else {
                                    self.read_symbol(start)
                                }
                            } else {
                                self.read_symbol(start)
                            }
                        } else {
                            // It's the minus operator or part of a symbol
                            self.read_symbol(start)
                        }
                    } else {
                        self.read_symbol(start)
                    }
                } else {
                    // Just a minus at end of input
                    self.read_symbol(start)
                }
            }
            Some(c) if c.is_digit(10) => self.read_number(start),
            Some(c) if is_symbol_start(c) => self.read_symbol(start),
            Some(c) => {
                let span = self.tracker.create_span(start);
                Err(DslError::new(
                    ErrorKind::UnexpectedChar,
                    format!("unexpected character: '{}'", c)
                ).with_span(span))
            }
            None => Ok(Token::Eof),
        }
    }
    
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(ch) = self.tracker.peek_char() {
            if ch.is_whitespace() {
                self.tracker.advance_char(ch);
            } else if ch == ';' {
                // Skip comment line
                while let Some(ch) = self.tracker.peek_char() {
                    self.tracker.advance_char(ch);
                    if ch == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }
    
    fn read_string(&mut self, start: Position) -> DslResult<Token> {
        self.tracker.advance_char('"'); // Skip opening quote
        let mut value = String::new();
        
        while let Some(ch) = self.tracker.peek_char() {
            if ch == '"' {
                self.tracker.advance_char('"');
                let span = self.tracker.create_span(start);
                return Ok(Token::Str(value, span));
            } else if ch == '\\' {
                self.tracker.advance_char('\\');
                if let Some(escaped) = self.tracker.peek_char() {
                    self.tracker.advance_char(escaped);
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        c => c,
                    });
                }
            } else {
                self.tracker.advance_char(ch);
                value.push(ch);
            }
        }
        
        let span = self.tracker.create_span(start);
        Err(DslError::new(
            ErrorKind::UnterminatedString,
            "unterminated string literal".to_string()
        ).with_span(span))
    }
    
    fn read_number(&mut self, start: Position) -> DslResult<Token> {
        let mut value = String::new();
        let mut has_dot = false;
        
        // Handle negative sign
        if self.tracker.peek_char() == Some('-') {
            value.push('-');
            self.tracker.advance_char('-');
        }
        
        while let Some(ch) = self.tracker.peek_char() {
            if ch.is_digit(10) {
                value.push(ch);
                self.tracker.advance_char(ch);
            } else if ch == '.' && !has_dot {
                has_dot = true;
                value.push(ch);
                self.tracker.advance_char(ch);
            } else if is_delimiter(ch) {
                break;
            } else {
                let span = self.tracker.create_span(start);
                return Err(DslError::new(
                    ErrorKind::InvalidNumber,
                    format!("invalid number literal: {}", value)
                ).with_span(span));
            }
        }
        
        let span = self.tracker.create_span(start);
        
        if has_dot {
            match value.parse::<f32>() {
                Ok(n) => Ok(Token::Num(n, span)),
                Err(_) => Err(DslError::new(
                    ErrorKind::InvalidNumber,
                    format!("invalid float literal: {}", value)
                ).with_span(span)),
            }
        } else {
            match value.parse::<i32>() {
                Ok(n) => Ok(Token::Int(n, span)),
                Err(_) => Err(DslError::new(
                    ErrorKind::InvalidNumber,
                    format!("invalid integer literal: {}", value)
                ).with_span(span)),
            }
        }
    }
    
    fn read_symbol(&mut self, start: Position) -> DslResult<Token> {
        let mut value = String::new();
        
        while let Some(ch) = self.tracker.peek_char() {
            if is_symbol_char(ch) {
                value.push(ch);
                self.tracker.advance_char(ch);
            } else if is_delimiter(ch) {
                break;
            } else {
                break;
            }
        }
        
        let span = self.tracker.create_span(start);
        
        match value.as_str() {
            "true" => Ok(Token::True(span)),
            "false" => Ok(Token::False(span)),
            _ => Ok(Token::Sym(value, span)),
        }
    }
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '-' || c == '+' || c == '*' || c == '/' 
    || c == '=' || c == '<' || c == '>' || c == '!' || c == '?' || c == ':' || c == '.'
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_digit(10) || c == '.'
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';'
}

pub struct EnhancedParser {
    lexer: EnhancedLexer,
}

impl EnhancedParser {
    pub fn new(source: String) -> Self {
        Self {
            lexer: EnhancedLexer::new(source),
        }
    }
    
    pub fn parse(&mut self) -> DslResult<Vec<Top>> {
        let mut tops = Vec::new();
        
        loop {
            match self.lexer.peek()? {
                Token::Eof => break,
                Token::LPar(_) => {
                    tops.push(self.parse_top()?);
                }
                token => {
                    let span = token.span();
                    return Err(DslError::new(
                        ErrorKind::UnexpectedToken,
                        format!("expected '(' at top level")
                    ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
                }
            }
        }
        
        Ok(tops)
    }
    
    fn parse_top(&mut self) -> DslResult<Top> {
        let Token::LPar(start_span) = self.lexer.next()? else {
            unreachable!()
        };
        
        let form_name = match self.lexer.next()? {
            Token::Sym(s, _) => s,
            token => {
                let span = token.span().unwrap_or(start_span);
                return Err(DslError::new(
                    ErrorKind::InvalidSyntax,
                    "expected form name after '('".to_string()
                ).with_span(span));
            }
        };
        
        let result = match form_name.as_str() {
            "defbehavior" => self.parse_behavior(),
            "defscene3d" => self.parse_scene3d(),
            _ => {
                Err(DslError::new(
                    ErrorKind::UnknownForm,
                    format!("unknown top-level form: {}", form_name)
                ).with_span(start_span))
            }
        };
        
        // Consume closing paren
        match self.lexer.next()? {
            Token::RPar(_) => {}
            token => {
                let span = token.span().unwrap_or(start_span);
                return Err(DslError::new(
                    ErrorKind::UnexpectedToken,
                    "expected ')' to close form".to_string()
                ).with_span(span));
            }
        }
        
        result
    }
    
    fn parse_behavior(&mut self) -> DslResult<Top> {
        // Parse behavior name
        let name = match self.lexer.next()? {
            Token::Sym(s, _) => s,
            token => {
                let span = token.span();
                return Err(DslError::new(
                    ErrorKind::InvalidSyntax,
                    "expected behavior name".to_string()
                ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
            }
        };
        
        let mut state = Vec::new();
        let mut update = None;
        let mut on_select = None;
        
        // Parse behavior body
        while let Token::LPar(_) = self.lexer.peek()? {
            let Token::LPar(clause_start) = self.lexer.next()? else {
                unreachable!()
            };
            
            let clause_type = match self.lexer.next()? {
                Token::Sym(s, _) => s,
                token => {
                    let span = token.span().unwrap_or(clause_start);
                    return Err(DslError::new(
                        ErrorKind::InvalidSyntax,
                        "expected clause type".to_string()
                    ).with_span(span));
                }
            };
            
            match clause_type.as_str() {
                "state" => {
                    state = self.parse_state_vars()?;
                }
                "update" => {
                    update = Some(self.parse_fn_def()?);
                }
                "on-select" => {
                    on_select = Some(self.parse_fn_def()?);
                }
                _ => {
                    return Err(DslError::new(
                        ErrorKind::UnknownForm,
                        format!("unknown behavior clause: {}", clause_type)
                    ).with_span(clause_start));
                }
            }
            
            // Consume closing paren for clause
            match self.lexer.next()? {
                Token::RPar(_) => {}
                token => {
                    let span = token.span().unwrap_or(clause_start);
                    return Err(DslError::new(
                        ErrorKind::UnexpectedToken,
                        "expected ')' to close clause".to_string()
                    ).with_span(span));
                }
            }
        }
        
        let update = update.ok_or_else(|| {
            DslError::new(
                ErrorKind::InvalidSyntax,
                "behavior requires an update clause".to_string()
            )
        })?;
        
        Ok(Top::Behavior(Behavior {
            name,
            state,
            update,
            on_select,
        }))
    }
    
    fn parse_scene3d(&mut self) -> DslResult<Top> {
        // Parse scene name
        let name = match self.lexer.next()? {
            Token::Sym(s, _) => s,
            token => {
                let span = token.span();
                return Err(DslError::new(
                    ErrorKind::InvalidSyntax,
                    "expected scene name".to_string()
                ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
            }
        };
        
        let mut objects = Vec::new();
        let mut ui_elements = Vec::new();
        let mut camera = None;
        let mut lighting = None;
        let mut input = None;
        
        // First expand macros, then parse the scene forms
        let mut forms = Vec::new();
        while let Token::LPar(_) = self.lexer.peek()? {
            forms.push(self.parse_expr()?);
        }
        
        // Expand scene macros
        let expanded_forms = crate::scene_macros::expand_scene_forms(forms)
            .map_err(|e| DslError::new(ErrorKind::MacroExpansionFailed, e.to_string()))?;
        
        // Now parse the expanded forms
        for form in expanded_forms {
            let Expr::List(ref parts) = form else {
                continue;
            };
            if parts.is_empty() {
                continue;
            }
            let Expr::Sym(ref tag) = parts[0] else {
                continue;
            };
            
            match tag.as_str() {
                "object" => {
                    objects.push(parse_object3d_from_expr(&parts[1..])?);
                }
                "sphere" | "cube" | "cylinder" | "cone" | "torus" | "plane" | "mesh" => {
                    // Direct object types (from cond expansion)
                    let mut object_parts = vec![
                        Expr::Sym(format!("{}_{}", tag, objects.len())),
                        Expr::Sym(tag.to_string())
                    ];
                    object_parts.extend_from_slice(&parts[1..]);
                    objects.push(parse_object3d_from_expr(&object_parts)?);
                }
                "ui" | "ui-element" => {
                    ui_elements.push(parse_ui_element_from_expr(&parts[1..])?);
                }
                "camera" => {
                    camera = Some(parse_camera_from_expr(&parts[1..])?);
                }
                "lighting" => {
                    lighting = Some(parse_lighting_from_expr(&parts[1..])?);
                }
                "input" => {
                    input = Some(parse_input_from_expr(&parts[1..])?);
                }
                _ => {
                    // Unknown form, but don't error - just skip it
                }
            }
        }
        
        Ok(Top::Scene3D(Scene3D {
            name,
            objects,
            ui_elements,
            camera,
            lighting,
            input,
        }))
    }
    
    fn parse_state_vars(&mut self) -> DslResult<Vec<(String, f32)>> {
        let mut vars = Vec::new();
        
        while let Token::LPar(_) = self.lexer.peek()? {
            self.lexer.next()?; // consume (
            
            let name = match self.lexer.next()? {
                Token::Sym(s, _) => s,
                token => {
                    let span = token.span();
                    return Err(DslError::new(
                        ErrorKind::InvalidSyntax,
                        "expected variable name".to_string()
                    ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
                }
            };
            
            let value = match self.lexer.next()? {
                Token::Num(n, _) => n,
                Token::Int(n, _) => n as f32,
                token => {
                    let span = token.span();
                    return Err(DslError::new(
                        ErrorKind::TypeMismatch,
                        "expected number for state variable".to_string()
                    ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
                }
            };
            
            match self.lexer.next()? {
                Token::RPar(_) => {}
                token => {
                    let span = token.span();
                    return Err(DslError::new(
                        ErrorKind::UnexpectedToken,
                        "expected ')' to close state variable".to_string()
                    ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))));
                }
            }
            
            vars.push((name, value));
        }
        
        Ok(vars)
    }
    
    fn parse_fn_def(&mut self) -> DslResult<FnDef> {
        // Parse parameters
        let params = self.parse_params()?;
        
        // Parse body expressions - multiple expressions are wrapped in an implicit progn/do
        let mut body_exprs = Vec::new();
        
        // Keep parsing expressions until we hit the closing paren
        while !matches!(self.lexer.peek()?, Token::RPar(_)) {
            body_exprs.push(self.parse_expr()?);
        }
        
        // If there's only one expression, use it directly
        // Otherwise, wrap multiple expressions in a "do" form
        let body = if body_exprs.len() == 1 {
            body_exprs.into_iter().next().unwrap()
        } else if body_exprs.is_empty() {
            // Empty body defaults to nil/unit
            Expr::List(vec![Expr::Sym("nil".to_string())])
        } else {
            // Multiple expressions wrapped in implicit "do"
            let mut do_form = vec![Expr::Sym("do".to_string())];
            do_form.extend(body_exprs);
            Expr::List(do_form)
        };
        
        Ok(FnDef { params, body })
    }
    
    fn parse_params(&mut self) -> DslResult<Vec<String>> {
        match self.lexer.peek()? {
            Token::LPar(_) => {
                self.lexer.next()?; // consume (
                let mut params = Vec::new();
                
                while let Token::Sym(name, _) = self.lexer.peek()? {
                    self.lexer.next()?;
                    params.push(name);
                }
                
                match self.lexer.next()? {
                    Token::RPar(_) => Ok(params),
                    token => {
                        let span = token.span();
                        Err(DslError::new(
                            ErrorKind::UnexpectedToken,
                            "expected ')' to close parameter list".to_string()
                        ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))))
                    }
                }
            }
            _ => Ok(Vec::new())
        }
    }
    
    fn parse_expr(&mut self) -> DslResult<Expr> {
        match self.lexer.next()? {
            Token::Int(n, _) => Ok(Expr::I32(n)),
            Token::Num(n, _) => Ok(Expr::F32(n)),
            Token::True(_) => Ok(Expr::Bool(true)),
            Token::False(_) => Ok(Expr::Bool(false)),
            Token::Str(s, _) => Ok(Expr::Str(s)),
            Token::Sym(s, _) => Ok(Expr::Sym(s)),
            Token::LPar(_) => {
                let mut exprs = Vec::new();
                
                while !matches!(self.lexer.peek()?, Token::RPar(_)) {
                    exprs.push(self.parse_expr()?);
                }
                
                self.lexer.next()?; // consume )
                Ok(Expr::List(exprs))
            }
            token => {
                let span = token.span();
                Err(DslError::new(
                    ErrorKind::UnexpectedToken,
                    "unexpected token in expression".to_string()
                ).with_span(span.unwrap_or_else(|| Span::new(Position::new(1, 1, 0), Position::new(1, 1, 0)))))
            }
        }
    }
}

/// Parse DSL source code with full position tracking
pub fn parse(source: &str) -> DslResult<Vec<Top>> {
    let mut parser = EnhancedParser::new(source.to_string());
    parser.parse()
}