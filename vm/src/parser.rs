//! EDN-like S-Expression Parser for XR-Lang
//! 
//! Supports rich literal syntax while maintaining homoiconicity:
//! - S-expressions: (+ 1 2)
//! - Vectors: [1 2 3]
//! - Maps: {:key "value"}
//! - Sets: #{1 2 3}
//! - Keywords: :keyword
//! - Metadata: ^{:doc "..."} value

use crate::value::{Value, Symbol, Keyword};
use std::collections::HashMap;

/// Token types for the lexer
#[derive(Clone, Debug, PartialEq)]
enum Token {
    // Delimiters
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    HashBrace,  // #{
    
    // Literals
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Str(String),
    Symbol(String),
    Keyword(String),
    
    // Special
    Quote,      // '
    Metadata,   // ^
    Comment(String),
}

/// Lexer for tokenizing input
struct Lexer {
    input: Vec<char>,
    position: usize,
    current: Option<char>,
}

impl Lexer {
    fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current = chars.get(0).cloned();
        Lexer {
            input: chars,
            position: 0,
            current,
        }
    }
    
    fn advance(&mut self) {
        self.position += 1;
        self.current = self.input.get(self.position).cloned();
    }
    
    fn peek(&self) -> Option<char> {
        self.input.get(self.position + 1).cloned()
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    fn read_string(&mut self) -> Result<String, String> {
        let mut result = String::new();
        self.advance(); // skip opening "
        
        while let Some(ch) = self.current {
            if ch == '"' {
                self.advance(); // skip closing "
                return Ok(result);
            } else if ch == '\\' {
                self.advance();
                match self.current {
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some('r') => result.push('\r'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some(c) => return Err(format!("Invalid escape sequence: \\{}", c)),
                    None => return Err("Unexpected end of string".to_string()),
                }
                self.advance();
            } else {
                result.push(ch);
                self.advance();
            }
        }
        
        Err("Unterminated string".to_string())
    }
    
    fn read_number(&mut self) -> Result<Token, String> {
        let mut num_str = String::new();
        let mut is_float = false;
        
        if self.current == Some('-') {
            num_str.push('-');
            self.advance();
        }
        
        while let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else if ch == '.' && !is_float {
                is_float = true;
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        
        if is_float {
            num_str.parse::<f64>()
                .map(Token::Float)
                .map_err(|e| format!("Invalid float: {}", e))
        } else {
            num_str.parse::<i64>()
                .map(Token::Int)
                .map_err(|e| format!("Invalid integer: {}", e))
        }
    }
    
    fn read_symbol_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        let is_keyword = self.current == Some(':');
        
        if is_keyword {
            self.advance(); // skip :
        }
        
        while let Some(ch) = self.current {
            if ch.is_alphanumeric() || "-_+*/?!<>=".contains(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        
        if is_keyword {
            Token::Keyword(ident)
        } else {
            match ident.as_str() {
                "nil" => Token::Nil,
                "true" => Token::True,
                "false" => Token::False,
                _ => Token::Symbol(ident),
            }
        }
    }
    
    fn read_comment(&mut self) -> Token {
        let mut comment = String::new();
        self.advance(); // skip ;
        
        while let Some(ch) = self.current {
            if ch == '\n' {
                break;
            }
            comment.push(ch);
            self.advance();
        }
        
        Token::Comment(comment)
    }
    
    fn next_token(&mut self) -> Result<Option<Token>, String> {
        loop {
            self.skip_whitespace();
            
            match self.current {
                None => return Ok(None),
                Some('(') => {
                    self.advance();
                    return Ok(Some(Token::LParen));
                }
                Some(')') => {
                    self.advance();
                    return Ok(Some(Token::RParen));
                }
                Some('[') => {
                    self.advance();
                    return Ok(Some(Token::LBracket));
                }
                Some(']') => {
                    self.advance();
                    return Ok(Some(Token::RBracket));
                }
                Some('{') => {
                    self.advance();
                    return Ok(Some(Token::LBrace));
                }
                Some('}') => {
                    self.advance();
                    return Ok(Some(Token::RBrace));
                }
                Some('#') => {
                    // Check for #t, #f (boolean literals) or #{ (set)
                    match self.peek() {
                        Some('{') => {
                            self.advance();
                            self.advance();
                            return Ok(Some(Token::HashBrace));
                        }
                        Some('t') => {
                            self.advance(); // skip #
                            self.advance(); // skip t
                            return Ok(Some(Token::True));
                        }
                        Some('f') => {
                            self.advance(); // skip #
                            self.advance(); // skip f
                            return Ok(Some(Token::False));
                        }
                        _ => {
                            return Err(format!("Unexpected character after #: {:?}", self.peek()));
                        }
                    }
                }
                Some('\'') => {
                    self.advance();
                    return Ok(Some(Token::Quote));
                }
                Some('^') => {
                    self.advance();
                    return Ok(Some(Token::Metadata));
                }
                Some('"') => {
                    let s = self.read_string()?;
                    return Ok(Some(Token::Str(s)));
                }
                Some(';') => {
                    let _comment = self.read_comment();
                    // Skip comments, continue to next token
                    continue;
                }
                Some('-') if self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) => {
                    return self.read_number().map(Some);
                }
                Some(ch) if ch.is_ascii_digit() => {
                    return self.read_number().map(Some);
                }
                Some(':') => {
                    return Ok(Some(self.read_symbol_or_keyword()));
                }
                Some(ch) if ch.is_alphabetic() || "-_+*/?!<>=".contains(ch) => {
                    return Ok(Some(self.read_symbol_or_keyword()));
                }
                Some(ch) => {
                    return Err(format!("Unexpected character: {}", ch));
                }
            }
        }
    }
}

/// Parser for converting tokens to Values
pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token().ok().flatten();
        Parser {
            lexer,
            current_token,
        }
    }
    
    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }
    
    fn parse_list(&mut self) -> Result<Value, String> {
        let mut items = Vec::new();
        self.advance()?; // consume (
        
        while self.current_token != Some(Token::RParen) {
            if self.current_token.is_none() {
                return Err("Unexpected end of input in list".to_string());
            }
            items.push(self.parse_value()?);
        }
        
        self.advance()?; // consume )
        Ok(Value::List(items))
    }
    
    fn parse_vector(&mut self) -> Result<Value, String> {
        let mut items = Vec::new();
        self.advance()?; // consume [
        
        while self.current_token != Some(Token::RBracket) {
            if self.current_token.is_none() {
                return Err("Unexpected end of input in vector".to_string());
            }
            items.push(self.parse_value()?);
        }
        
        self.advance()?; // consume ]
        Ok(Value::Vector(items))
    }
    
    fn parse_map(&mut self) -> Result<Value, String> {
        let mut map = HashMap::new();
        self.advance()?; // consume {
        
        while self.current_token != Some(Token::RBrace) {
            if self.current_token.is_none() {
                return Err("Unexpected end of input in map".to_string());
            }
            
            let key = self.parse_value()?;
            if self.current_token.is_none() {
                return Err("Map missing value for key".to_string());
            }
            let value = self.parse_value()?;
            
            // Convert key to string for HashMap<String, Value>
            let key_str = match key {
                Value::Keyword(Keyword(k)) => k,
                Value::Str(s) => s,
                Value::Symbol(Symbol(s)) => s,
                _ => return Err(format!("Map keys must be keywords, strings or symbols, got {:?}", key)),
            };
            
            map.insert(key_str, value);
        }
        
        self.advance()?; // consume }
        Ok(Value::Map(map))
    }
    
    fn parse_set(&mut self) -> Result<Value, String> {
        let mut set = Vec::new();
        self.advance()?; // consume HashBrace token
        
        while self.current_token != Some(Token::RBrace) {
            if self.current_token.is_none() {
                return Err("Unexpected end of input in set".to_string());
            }
            let val = self.parse_value()?;
            // Simple deduplication
            if !set.iter().any(|v| v == &val) {
                set.push(val);
            }
            // Note: parse_value already advances past the parsed value
        }
        
        self.advance()?; // consume }
        Ok(Value::Set(set))
    }
    
    fn parse_metadata(&mut self) -> Result<Value, String> {
        self.advance()?; // consume ^
        
        // Parse metadata map
        let meta = match self.current_token {
            Some(Token::LBrace) => self.parse_map()?,
            _ => return Err("Metadata must be a map".to_string()),
        };
        
        // Parse the value to attach metadata to
        let value = self.parse_value()?;
        
        // Extract metadata as HashMap
        if let Value::Map(metadata) = meta {
            let string_meta = metadata;
            Ok(Value::WithMeta {
                value: Box::new(value),
                metadata: string_meta,
            })
        } else {
            Err("Invalid metadata".to_string())
        }
    }
    
    fn parse_quote(&mut self) -> Result<Value, String> {
        self.advance()?; // consume '
        let value = self.parse_value()?;
        Ok(Value::Quote(Box::new(value)))
    }
    
    pub fn parse_value(&mut self) -> Result<Value, String> {
        match &self.current_token {
            None => Err("Unexpected end of input".to_string()),
            Some(token) => {
                let result = match token {
                    Token::Nil => {
                        self.advance()?;
                        Ok(Value::Nil)
                    }
                    Token::True => {
                        self.advance()?;
                        Ok(Value::Bool(true))
                    }
                    Token::False => {
                        self.advance()?;
                        Ok(Value::Bool(false))
                    }
                    Token::Int(n) => {
                        let val = Value::Int(*n);
                        self.advance()?;
                        Ok(val)
                    }
                    Token::Float(f) => {
                        let val = Value::Float(*f);
                        self.advance()?;
                        Ok(val)
                    }
                    Token::Str(s) => {
                        let val = Value::Str(s.clone());
                        self.advance()?;
                        Ok(val)
                    }
                    Token::Symbol(s) => {
                        let val = Value::Symbol(Symbol(s.clone()));
                        self.advance()?;
                        Ok(val)
                    }
                    Token::Keyword(k) => {
                        let val = Value::Keyword(Keyword(k.clone()));
                        self.advance()?;
                        Ok(val)
                    }
                    Token::LParen => self.parse_list(),
                    Token::LBracket => self.parse_vector(),
                    Token::LBrace => self.parse_map(),
                    Token::HashBrace => self.parse_set(),
                    Token::Quote => self.parse_quote(),
                    Token::Metadata => self.parse_metadata(),
                    _ => Err(format!("Unexpected token: {:?}", token)),
                };
                result
            }
        }
    }
    
    pub fn parse(&mut self) -> Result<Vec<Value>, String> {
        let mut values = Vec::new();
        
        while self.current_token.is_some() {
            values.push(self.parse_value()?);
        }
        
        Ok(values)
    }
}

/// Parse a string into Values
pub fn parse(input: &str) -> Result<Vec<Value>, String> {
    let mut parser = Parser::new(input);
    parser.parse()
}

/// Parse a single expression
pub fn parse_one(input: &str) -> Result<Value, String> {
    let values = parse(input)?;
    if values.len() != 1 {
        return Err(format!("Expected single expression, got {}", values.len()));
    }
    Ok(values.into_iter().next().unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_primitives() {
        assert_eq!(parse_one("nil").unwrap(), Value::Nil);
        assert_eq!(parse_one("true").unwrap(), Value::Bool(true));
        assert_eq!(parse_one("false").unwrap(), Value::Bool(false));
        assert_eq!(parse_one("42").unwrap(), Value::Int(42));
        assert_eq!(parse_one("-42").unwrap(), Value::Int(-42));
        assert_eq!(parse_one("3.14").unwrap(), Value::Float(3.14));
        assert_eq!(parse_one("\"hello\"").unwrap(), Value::Str("hello".to_string()));
        assert_eq!(parse_one("foo").unwrap(), Value::symbol("foo"));
        assert_eq!(parse_one(":bar").unwrap(), Value::keyword("bar"));
    }
    
    #[test]
    fn test_parse_list() {
        let result = parse_one("(+ 1 2)").unwrap();
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::symbol("+"));
                assert_eq!(items[1], Value::Int(1));
                assert_eq!(items[2], Value::Int(2));
            }
            _ => panic!("Expected list"),
        }
    }
    
    #[test]
    fn test_parse_vector() {
        let result = parse_one("[1 2 3]").unwrap();
        match result {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
            }
            _ => panic!("Expected vector"),
        }
    }
    
    #[test]
    fn test_parse_map() {
        let result = parse_one("{:name \"John\" :age 30}").unwrap();
        match result {
            Value::Map(map) => {
                assert_eq!(map.get("name"), Some(&Value::Str("John".to_string())));
                assert_eq!(map.get("age"), Some(&Value::Int(30)));
            }
            _ => panic!("Expected map"),
        }
    }
    
    #[test]
    fn test_parse_set() {
        let result = parse_one("#{1 2 3}").unwrap();
        match result {
            Value::Set(set) => {
                assert!(set.iter().any(|v| v == &Value::Int(1)));
                assert!(set.iter().any(|v| v == &Value::Int(2)));
                assert!(set.iter().any(|v| v == &Value::Int(3)));
                assert_eq!(set.len(), 3);
            }
            _ => panic!("Expected set"),
        }
    }
    
    #[test]
    fn test_parse_quote() {
        let result = parse_one("'(+ 1 2)").unwrap();
        match result {
            Value::Quote(boxed) => {
                match boxed.as_ref() {
                    Value::List(items) => {
                        assert_eq!(items.len(), 3);
                    }
                    _ => panic!("Expected quoted list"),
                }
            }
            _ => panic!("Expected quote"),
        }
    }
    
    #[test]
    fn test_parse_with_comments() {
        let input = r#"
            ; This is a comment
            (defn add [x y]  ; Define addition function
              (+ x y))        ; Return sum
        "#;
        let values = parse(input).unwrap();
        assert_eq!(values.len(), 1);
    }
    
    #[test]
    fn test_parse_scene_dsl() {
        let input = r#"
            (defscene3d demo
              (camera 
                (position 0 5 10)
                (meta :preserve-runtime))
              (cube
                (position 0 0 0)))
        "#;
        let values = parse(input).unwrap();
        assert_eq!(values.len(), 1);
        match &values[0] {
            Value::List(items) => {
                assert_eq!(items[0], Value::symbol("defscene3d"));
                assert_eq!(items[1], Value::symbol("demo"));
            }
            _ => panic!("Expected list"),
        }
    }
}