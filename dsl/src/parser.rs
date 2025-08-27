//! S-expression parser for XR-DSL

use crate::ast::*;

#[derive(Clone, Debug)]
enum Tok {
    LPar,
    RPar,
    Sym(String),
    Num(f32),
    Int(i32),
    True,
    False,
}

#[derive(Clone)]
struct Lexer<'a> {
    s: &'a [u8],
    i: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            s: src.as_bytes(),
            i: 0,
        }
    }
    
    fn peek(&self) -> Option<Tok> {
        let mut clone = self.clone();
        clone.next()
    }
    
    fn skip_whitespace(&mut self) {
        while self.i < self.s.len() && self.s[self.i].is_ascii_whitespace() {
            self.i += 1;
        }
    }
    
    fn skip_comment(&mut self) {
        if self.i < self.s.len() && self.s[self.i] == b';' {
            while self.i < self.s.len() && self.s[self.i] != b'\n' {
                self.i += 1;
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Tok;
    
    fn next(&mut self) -> Option<Tok> {
        loop {
            self.skip_whitespace();
            if self.i >= self.s.len() {
                return None;
            }
            
            // Skip comments
            if self.s[self.i] == b';' {
                self.skip_comment();
                continue;
            }
            
            let c = self.s[self.i];
            return Some(match c {
                b'(' => {
                    self.i += 1;
                    Tok::LPar
                }
                b')' => {
                    self.i += 1;
                    Tok::RPar
                }
                b'0'..=b'9' | b'-' | b'+' => {
                    let start = self.i;
                    let mut has_dot = false;
                    
                    if self.s[self.i] == b'-' || self.s[self.i] == b'+' {
                        self.i += 1;
                    }
                    
                    while self.i < self.s.len() && (self.s[self.i].is_ascii_digit() || self.s[self.i] == b'.') {
                        if self.s[self.i] == b'.' {
                            if has_dot { break; }
                            has_dot = true;
                        }
                        self.i += 1;
                    }
                    
                    let s = std::str::from_utf8(&self.s[start..self.i]).unwrap();
                    if has_dot {
                        Tok::Num(s.parse::<f32>().unwrap_or(0.0))
                    } else {
                        Tok::Int(s.parse::<i32>().unwrap_or(0))
                    }
                }
                _ => {
                    let start = self.i;
                    while self.i < self.s.len() 
                        && !self.s[self.i].is_ascii_whitespace() 
                        && self.s[self.i] != b'(' 
                        && self.s[self.i] != b')' 
                        && self.s[self.i] != b';' {
                        self.i += 1;
                    }
                    
                    let s = std::str::from_utf8(&self.s[start..self.i]).unwrap().to_string();
                    match s.as_str() {
                        "#t" | "true" => Tok::True,
                        "#f" | "false" => Tok::False,
                        _ => Tok::Sym(s),
                    }
                }
            });
        }
    }
}

pub fn parse(src: &str) -> anyhow::Result<Vec<Top>> {
    let mut lexer = Lexer::new(src);
    let mut tops = vec![];
    
    while let Some(tok) = lexer.peek() {
        match tok {
            Tok::LPar => {
                let expr = parse_expr(&mut lexer)?;
                tops.push(desugar_top(expr)?);
            }
            _ => {
                lexer.next(); // skip
            }
        }
    }
    
    Ok(tops)
}

fn parse_expr(lexer: &mut Lexer) -> anyhow::Result<Expr> {
    match lexer.next() {
        Some(Tok::LPar) => {
            let mut v = vec![];
            while let Some(t) = lexer.peek() {
                if matches!(t, Tok::RPar) {
                    lexer.next();
                    break;
                }
                v.push(parse_expr(lexer)?);
            }
            Ok(Expr::List(v))
        }
        Some(Tok::RPar) => anyhow::bail!("unexpected )"),
        Some(Tok::Num(n)) => Ok(Expr::F32(n)),
        Some(Tok::Int(i)) => Ok(Expr::I32(i)),
        Some(Tok::True) => Ok(Expr::Bool(true)),
        Some(Tok::False) => Ok(Expr::Bool(false)),
        Some(Tok::Sym(s)) => Ok(Expr::Sym(s)),
        None => anyhow::bail!("unexpected EOF"),
    }
}

fn desugar_top(e: Expr) -> anyhow::Result<Top> {
    let list = match e {
        Expr::List(v) => v,
        _ => anyhow::bail!("top-level must be a list"),
    };
    
    let Expr::Sym(head) = &list[0] else {
        anyhow::bail!("expected symbol")
    };
    
    match head.as_str() {
        "defbehavior" => {
            let name = match &list[1] {
                Expr::Sym(s) => s.clone(),
                _ => anyhow::bail!("behavior name must be symbol"),
            };
            
            let mut state = vec![];
            let mut update = None;
            let mut on_select = None;
            
            for form in &list[2..] {
                let Expr::List(parts) = form else {
                    anyhow::bail!("behavior form must be list")
                };
                let Expr::Sym(tag) = &parts[0] else {
                    anyhow::bail!("behavior form must start with symbol")
                };
                
                match tag.as_str() {
                    "state" => {
                        for slot in &parts[1..] {
                            let Expr::List(pair) = slot else {
                                anyhow::bail!("state slot must be list")
                            };
                            let Expr::Sym(id) = &pair[0] else {
                                anyhow::bail!("state id must be symbol")
                            };
                            let val = match &pair[1] {
                                Expr::F32(x) => *x,
                                Expr::I32(x) => *x as f32,
                                _ => anyhow::bail!("state value must be number"),
                            };
                            state.push((id.clone(), val));
                        }
                    }
                    "update" => {
                        let Expr::List(ps) = &parts[1] else {
                            anyhow::bail!("update params must be list")
                        };
                        let mut params = vec![];
                        for p in ps {
                            if let Expr::Sym(s) = p {
                                params.push(s.clone());
                            }
                        }
                        update = Some(FnDef {
                            params,
                            body: parts[2].clone(),
                        });
                    }
                    "on_select" => {
                        let Expr::List(ps) = &parts[1] else {
                            anyhow::bail!("on_select params must be list")
                        };
                        let mut params = vec![];
                        for p in ps {
                            if let Expr::Sym(s) = p {
                                params.push(s.clone());
                            }
                        }
                        on_select = Some(FnDef {
                            params,
                            body: parts[2].clone(),
                        });
                    }
                    _ => anyhow::bail!("unknown behavior form: {}", tag),
                }
            }
            
            let update = update.ok_or_else(|| anyhow::anyhow!("behavior must have update"))?;
            
            Ok(Top::Behavior(Behavior {
                name,
                state,
                update,
                on_select,
            }))
        }
        "defscene3d" => {
            let name = match &list[1] {
                Expr::Sym(s) => s.clone(),
                _ => anyhow::bail!("scene name must be symbol"),
            };
            
            let mut objects = vec![];
            let mut camera = None;
            let mut lighting = None;
            
            for form in &list[2..] {
                let Expr::List(parts) = form else {
                    anyhow::bail!("scene form must be list")
                };
                let Expr::Sym(tag) = &parts[0] else {
                    anyhow::bail!("scene form must start with symbol")
                };
                
                match tag.as_str() {
                    "object" => {
                        objects.push(parse_object3d(&parts[1..])?);
                    }
                    "camera" => {
                        camera = Some(parse_camera(&parts[1..])?);
                    }
                    "lighting" => {
                        lighting = Some(parse_lighting(&parts[1..])?);
                    }
                    _ => anyhow::bail!("unknown scene form: {}", tag),
                }
            }
            
            Ok(Top::Scene3D(Scene3D {
                name,
                objects,
                camera,
                lighting,
            }))
        }
        _ => anyhow::bail!("unknown top-level form: {}", head),
    }
}

fn parse_object3d(parts: &[Expr]) -> anyhow::Result<Object3D> {
    let name = match &parts[0] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("object name must be symbol"),
    };
    
    let mesh_type = match &parts[1] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("mesh type must be symbol"),
    };
    
    let mut transform = TransformDef {
        position: [0.0, 0.0, 0.0],
        rotation: [0.0, 0.0, 0.0],
        scale: [1.0, 1.0, 1.0],
    };
    let material = None;
    let mut behavior = None;
    
    for form in &parts[2..] {
        if let Expr::List(prop) = form {
            if let Expr::Sym(tag) = &prop[0] {
                match tag.as_str() {
                    "position" => {
                        transform.position = parse_vec3(&prop[1..])?;
                    }
                    "rotation" => {
                        transform.rotation = parse_vec3(&prop[1..])?;
                    }
                    "scale" => {
                        transform.scale = parse_vec3(&prop[1..])?;
                    }
                    "behavior" => {
                        if let Expr::Sym(b) = &prop[1] {
                            behavior = Some(b.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    
    Ok(Object3D {
        name,
        mesh_type,
        transform,
        material,
        behavior,
    })
}

fn parse_camera(parts: &[Expr]) -> anyhow::Result<CameraDef> {
    let mut position = [0.0, 5.0, 10.0];
    let mut target = [0.0, 0.0, 0.0];
    let mut up = [0.0, 1.0, 0.0];
    let mut fov = 45.0; // Default FOV in degrees
    let mut near = 0.1;
    let mut far = 100.0;
    
    // Parse camera properties
    for part in parts {
        if let Expr::List(ref list) = part {
            if list.is_empty() { continue; }
            
            if let Some(prop_name) = list[0].as_symbol() {
                match prop_name {
                    "position" => {
                        if list.len() == 4 {
                            position = parse_vec3(&list[1..])?;
                        }
                    }
                    "target" => {
                        if list.len() == 4 {
                            target = parse_vec3(&list[1..])?;
                        }
                    }
                    "up" => {
                        if list.len() == 4 {
                            up = parse_vec3(&list[1..])?;
                        }
                    }
                    "fov" => {
                        if list.len() == 2 {
                            fov = parse_float(&list[1])?;
                        }
                    }
                    "near" => {
                        if list.len() == 2 {
                            near = parse_float(&list[1])?;
                        }
                    }
                    "far" => {
                        if list.len() == 2 {
                            far = parse_float(&list[1])?;
                        }
                    }
                    _ => {} // Ignore unknown properties
                }
            }
        }
    }
    
    Ok(CameraDef {
        position,
        target,
        up,
        fov: fov * std::f32::consts::PI / 180.0, // Convert degrees to radians
        near,
        far,
    })
}

fn parse_lighting(parts: &[Expr]) -> anyhow::Result<LightingDef> {
    let mut ambient = [0.3, 0.3, 0.3];
    let mut directional = None;
    
    // Parse lighting properties
    for part in parts {
        if let Expr::List(ref list) = part {
            if list.is_empty() { continue; }
            
            if let Some(prop_name) = list[0].as_symbol() {
                match prop_name {
                    "ambient" => {
                        if list.len() == 4 {
                            ambient = parse_vec3(&list[1..])?;
                        }
                    }
                    "directional" => {
                        // Parse directional light properties
                        let mut direction = [1.0, 1.0, 1.0];
                        let mut color = [1.0, 1.0, 1.0];
                        let mut intensity = 1.0;
                        
                        for dir_part in &list[1..] {
                            if let Expr::List(ref dir_list) = dir_part {
                                if dir_list.is_empty() { continue; }
                                
                                if let Some(dir_prop) = dir_list[0].as_symbol() {
                                    match dir_prop {
                                        "direction" => {
                                            if dir_list.len() == 4 {
                                                direction = parse_vec3(&dir_list[1..])?;
                                            }
                                        }
                                        "color" => {
                                            if dir_list.len() == 4 {
                                                color = parse_vec3(&dir_list[1..])?;
                                            }
                                        }
                                        "intensity" => {
                                            if dir_list.len() == 2 {
                                                intensity = parse_float(&dir_list[1])?;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        
                        directional = Some(DirectionalLight {
                            direction,
                            color,
                            intensity,
                        });
                    }
                    _ => {} // Ignore unknown properties
                }
            }
        }
    }
    
    Ok(LightingDef {
        ambient,
        directional,
    })
}

fn parse_float(expr: &Expr) -> anyhow::Result<f32> {
    match expr {
        Expr::F32(f) => Ok(*f),
        Expr::I32(i) => Ok(*i as f32),
        _ => anyhow::bail!("Expected a number, got {:?}", expr),
    }
}

fn parse_vec3(parts: &[Expr]) -> anyhow::Result<[f32; 3]> {
    if parts.len() != 3 {
        anyhow::bail!("vec3 needs exactly 3 components");
    }
    
    let mut result = [0.0; 3];
    for (i, part) in parts.iter().enumerate() {
        result[i] = match part {
            Expr::F32(f) => *f,
            Expr::I32(i) => *i as f32,
            _ => anyhow::bail!("vec3 components must be numbers"),
        };
    }
    
    Ok(result)
}