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
                b'0'..=b'9' => {
                    let start = self.i;
                    let mut has_dot = false;
                    
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
                b'-' | b'+' => {
                    let start = self.i;
                    let sign_char = self.s[self.i];
                    self.i += 1;
                    
                    // Check if this is a number (sign followed by digit)
                    if self.i < self.s.len() && self.s[self.i].is_ascii_digit() {
                        let mut has_dot = false;
                        
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
                    } else {
                        // It's just a symbol (operator)
                        Tok::Sym(if sign_char == b'+' { "+".to_string() } else { "-".to_string() })
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
        "defbehavior" | "behavior" => {
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
                        
                        // If there are multiple statements, wrap them in a begin block
                        let body = if parts.len() > 3 {
                            // Multiple statements - wrap in begin
                            let mut statements = vec![Expr::Sym("begin".to_string())];
                            for stmt in &parts[2..] {
                                statements.push(stmt.clone());
                            }
                            Expr::List(statements)
                        } else {
                            // Single statement
                            parts[2].clone()
                        };
                        
                        update = Some(FnDef {
                            params,
                            body,
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
                        
                        // If there are multiple statements, wrap them in a begin block
                        let body = if parts.len() > 3 {
                            // Multiple statements - wrap in begin
                            let mut statements = vec![Expr::Sym("begin".to_string())];
                            for stmt in &parts[2..] {
                                statements.push(stmt.clone());
                            }
                            Expr::List(statements)
                        } else {
                            // Single statement
                            parts[2].clone()
                        };
                        
                        on_select = Some(FnDef {
                            params,
                            body,
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
            let mut ui_elements = vec![];
            let mut camera = None;
            let mut lighting = None;
            let mut input = None;
            
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
                    "ui" | "ui-element" => {
                        ui_elements.push(parse_ui_element(&parts[1..])?);
                    }
                    "camera" => {
                        camera = Some(parse_camera(&parts[1..])?);
                    }
                    "lighting" => {
                        lighting = Some(parse_lighting(&parts[1..])?);
                    }
                    "input" => {
                        input = Some(parse_input(&parts[1..])?);
                    }
                    _ => anyhow::bail!("unknown scene form: {}", tag),
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
        _ => anyhow::bail!("unknown top-level form: {}", head),
    }
}

fn parse_object3d(parts: &[Expr]) -> anyhow::Result<Object3D> {
    let name = match &parts[0] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("object name must be symbol"),
    };
    
    // mesh_type can be either a primitive name (cube, sphere) or a model path
    let mesh_type = match &parts[1] {
        Expr::Sym(s) => s.clone(), // Both primitives and paths are symbols in our parser
        _ => anyhow::bail!("mesh type must be symbol (primitive name or model path)"),
    };
    
    let mut transform = TransformDef {
        position: [0.0, 0.0, 0.0],
        rotation: [0.0, 0.0, 0.0],
        scale: [1.0, 1.0, 1.0],
    };
    let material = None;
    let mut behavior = None;
    let mut interactive = false;
    let mut meta = None;
    
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
                            // Special case: "interactive" behavior enables gizmos
                            if b == "interactive" {
                                interactive = true;
                            }
                        }
                    }
                    "interactive" => {
                        // Allow explicit (interactive true/false) syntax
                        if prop.len() > 1 {
                            interactive = match &prop[1] {
                                Expr::Bool(b) => *b,
                                Expr::Sym(s) if s == "true" => true,
                                Expr::Sym(s) if s == "false" => false,
                                _ => true,
                            };
                        } else {
                            interactive = true;
                        }
                    }
                    "meta" => {
                        meta = Some(parse_meta_directive(&prop[1..])?);
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
        interactive,
        meta,
    })
}

fn parse_camera(parts: &[Expr]) -> anyhow::Result<CameraDef> {
    let mut position = [0.0, 5.0, 10.0];
    let mut target = [0.0, 0.0, 0.0];
    let mut up = [0.0, 1.0, 0.0];
    let mut fov = 45.0; // Default FOV in degrees
    let mut near = 0.1;
    let mut far = 100.0;
    let mut meta = None;
    
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
                    "meta" => {
                        meta = Some(parse_meta_directive(&list[1..])?);
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
        meta,
    })
}

fn parse_ui_element(parts: &[Expr]) -> anyhow::Result<UIElement> {
    if parts.len() < 2 {
        anyhow::bail!("ui element needs at least name and type");
    }
    
    let name = match &parts[0] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("ui element name must be symbol"),
    };
    
    let ui_type = match &parts[1] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("ui element type must be symbol"),
    };
    
    let mut position = [0.0, 0.0, -5.0];
    let mut size = [2.0, 2.0];
    let mut text = None;
    let mut color = [1.0, 1.0, 1.0, 1.0];
    let mut behavior = None;
    
    // Parse properties
    for part in &parts[2..] {
        if let Expr::List(ref list) = part {
            if list.is_empty() { continue; }
            
            if let Some(prop_name) = list[0].as_symbol() {
                match prop_name {
                    "position" => {
                        if list.len() == 4 {
                            position = parse_vec3(&list[1..])?;
                        }
                    }
                    "size" => {
                        if list.len() == 3 {
                            let x = parse_float(&list[1])?;
                            let y = parse_float(&list[2])?;
                            size = [x, y];
                        }
                    }
                    "text" => {
                        if list.len() >= 2 {
                            // Join all text parts
                            let text_parts: Vec<String> = list[1..].iter()
                                .filter_map(|e| match e {
                                    Expr::Sym(s) => Some(s.clone()),
                                    _ => None,
                                })
                                .collect();
                            text = Some(text_parts.join(" "));
                        }
                    }
                    "color" => {
                        if list.len() == 5 {
                            let r = parse_float(&list[1])?;
                            let g = parse_float(&list[2])?;
                            let b = parse_float(&list[3])?;
                            let a = parse_float(&list[4])?;
                            color = [r, g, b, a];
                        }
                    }
                    "behavior" => {
                        if list.len() == 2 {
                            behavior = Some(match &list[1] {
                                Expr::Sym(s) => s.clone(),
                                _ => anyhow::bail!("behavior must be symbol"),
                            });
                        }
                    }
                    _ => {} // Ignore unknown properties
                }
            }
        }
    }
    
    Ok(UIElement {
        name,
        ui_type,
        position,
        size,
        text,
        color,
        behavior,
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

fn parse_input(parts: &[Expr]) -> anyhow::Result<InputDef> {
    let mut camera_controls = None;
    let mut key_bindings = vec![];
    
    for form in parts {
        let Expr::List(prop) = form else {
            continue;
        };
        
        if prop.is_empty() {
            continue;
        }
        
        let Expr::Sym(tag) = &prop[0] else {
            continue;
        };
        
        match tag.as_str() {
            "camera-controls" => {
                camera_controls = Some(parse_camera_controls(&prop[1..])?);
            }
            "key" => {
                key_bindings.push(parse_key_binding(&prop[1..])?);
            }
            _ => {} // Ignore unknown properties
        }
    }
    
    Ok(InputDef {
        camera_controls,
        key_bindings,
    })
}

fn parse_camera_controls(parts: &[Expr]) -> anyhow::Result<CameraControls> {
    let mut move_speed = 5.0;
    let mut rotate_speed = 1.0;
    let mut movement_keys = MovementKeys {
        forward: "W".to_string(),
        backward: "S".to_string(),
        left: "A".to_string(),
        right: "D".to_string(),
        up: "Space".to_string(),
        down: "Shift".to_string(),
    };
    let mut rotation_keys = RotationKeys {
        pitch_up: "Up".to_string(),
        pitch_down: "Down".to_string(),
        yaw_left: "Left".to_string(),
        yaw_right: "Right".to_string(),
    };
    let mut orbit_controls = None;
    
    for form in parts {
        let Expr::List(prop) = form else {
            continue;
        };
        
        if prop.len() < 2 {
            continue;
        }
        
        let Expr::Sym(tag) = &prop[0] else {
            continue;
        };
        
        match tag.as_str() {
            "move-speed" => {
                move_speed = parse_float(&prop[1])?;
            }
            "rotate-speed" => {
                rotate_speed = parse_float(&prop[1])?;
            }
            "movement" => {
                for movement_prop in &prop[1..] {
                    if let Expr::List(mp) = movement_prop {
                        if mp.len() >= 2 {
                            if let (Expr::Sym(key), Expr::Sym(value)) = (&mp[0], &mp[1]) {
                                match key.as_str() {
                                    "forward" => movement_keys.forward = value.clone(),
                                    "backward" => movement_keys.backward = value.clone(),
                                    "left" => movement_keys.left = value.clone(),
                                    "right" => movement_keys.right = value.clone(),
                                    "up" => movement_keys.up = value.clone(),
                                    "down" => movement_keys.down = value.clone(),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
            "rotation" => {
                for rotation_prop in &prop[1..] {
                    if let Expr::List(rp) = rotation_prop {
                        if rp.len() >= 2 {
                            if let (Expr::Sym(key), Expr::Sym(value)) = (&rp[0], &rp[1]) {
                                match key.as_str() {
                                    "pitch-up" => rotation_keys.pitch_up = value.clone(),
                                    "pitch-down" => rotation_keys.pitch_down = value.clone(),
                                    "yaw-left" => rotation_keys.yaw_left = value.clone(),
                                    "yaw-right" => rotation_keys.yaw_right = value.clone(),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
            "orbit-controls" => {
                orbit_controls = Some(parse_orbit_controls(&prop[1..])?);
            }
            _ => {}
        }
    }
    
    Ok(CameraControls {
        move_speed,
        rotate_speed,
        movement_keys,
        rotation_keys,
        orbit_controls,
    })
}

fn parse_orbit_controls(parts: &[Expr]) -> anyhow::Result<OrbitControls> {
    let mut enabled = true;
    let mut sensitivity = 1.0;
    let mut damping = 0.05;
    let mut min_distance = 1.0;
    let mut max_distance = 100.0;
    let mut min_polar_angle = 0.0;
    let mut max_polar_angle = std::f32::consts::PI;
    let mut enable_zoom = true;
    let mut zoom_speed = 1.0;
    
    for form in parts {
        let Expr::List(prop) = form else {
            continue;
        };
        
        if prop.len() < 2 {
            continue;
        }
        
        let Expr::Sym(tag) = &prop[0] else {
            continue;
        };
        
        match tag.as_str() {
            "enabled" => {
                enabled = match &prop[1] {
                    Expr::Bool(b) => *b,
                    Expr::Sym(s) if s == "true" => true,
                    Expr::Sym(s) if s == "false" => false,
                    _ => true,
                };
            }
            "sensitivity" => {
                sensitivity = parse_float(&prop[1])?;
            }
            "damping" => {
                damping = parse_float(&prop[1])?;
            }
            "min-distance" => {
                min_distance = parse_float(&prop[1])?;
            }
            "max-distance" => {
                max_distance = parse_float(&prop[1])?;
            }
            "min-polar-angle" => {
                min_polar_angle = parse_float(&prop[1])?;
            }
            "max-polar-angle" => {
                max_polar_angle = parse_float(&prop[1])?;
            }
            "enable-zoom" => {
                enable_zoom = match &prop[1] {
                    Expr::Bool(b) => *b,
                    Expr::Sym(s) if s == "true" => true,
                    Expr::Sym(s) if s == "false" => false,
                    _ => true,
                };
            }
            "zoom-speed" => {
                zoom_speed = parse_float(&prop[1])?;
            }
            _ => {}
        }
    }
    
    Ok(OrbitControls {
        enabled,
        sensitivity,
        damping,
        min_distance,
        max_distance,
        min_polar_angle,
        max_polar_angle,
        enable_zoom,
        zoom_speed,
    })
}

fn parse_key_binding(parts: &[Expr]) -> anyhow::Result<KeyBinding> {
    if parts.len() < 2 {
        anyhow::bail!("key binding needs key and action");
    }
    
    let key = match &parts[0] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("key must be a symbol"),
    };
    
    let action = match &parts[1] {
        Expr::Sym(s) => s.clone(),
        _ => anyhow::bail!("action must be a symbol"),
    };
    
    let target = if parts.len() > 2 {
        match &parts[2] {
            Expr::Sym(s) => Some(s.clone()),
            _ => None,
        }
    } else {
        None
    };
    
    Ok(KeyBinding {
        key,
        action,
        target,
    })
}

fn parse_meta_directive(parts: &[Expr]) -> anyhow::Result<crate::ast::MetaDirective> {
    let mut preserve_mode = "reset-on-reload".to_string(); // Default mode
    let mut properties = Vec::new();
    
    for expr in parts {
        match expr {
            Expr::Sym(s) => {
                // Single symbols are preserve modes
                match s.as_str() {
                    "preserve-runtime" | "preserve" => {
                        preserve_mode = "preserve-runtime".to_string();
                    }
                    "sync-to-code" | "sync" => {
                        preserve_mode = "sync-to-code".to_string();
                    }
                    "reset-on-reload" | "reset" => {
                        preserve_mode = "reset-on-reload".to_string();
                    }
                    "volatile" => {
                        preserve_mode = "volatile".to_string();
                    }
                    _ => {
                        // Unknown symbol, treat as property to preserve
                        properties.push(s.clone());
                    }
                }
            }
            Expr::List(list) => {
                // Lists specify properties to preserve
                if let Some(Expr::Sym(tag)) = list.first() {
                    if tag == "preserve" || tag == "properties" {
                        for item in &list[1..] {
                            if let Expr::Sym(prop) = item {
                                properties.push(prop.clone());
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    // If no properties specified, default to preserving all
    if properties.is_empty() && preserve_mode != "reset-on-reload" {
        properties = vec!["position".to_string(), "rotation".to_string(), "scale".to_string()];
    }
    
    Ok(crate::ast::MetaDirective {
        preserve_mode,
        properties,
    })
}