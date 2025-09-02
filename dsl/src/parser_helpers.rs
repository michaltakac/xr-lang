//! Helper functions for the enhanced parser

use crate::ast::*;
use crate::error::{DslError, DslResult, ErrorKind};

/// Parse Object3D from expression list
pub fn parse_object3d_from_expr(exprs: &[Expr]) -> DslResult<Object3D> {
    if exprs.len() < 2 {
        return Err(DslError::new(
            ErrorKind::InvalidSyntax,
            "object requires name and type".to_string()
        ));
    }
    
    let name = match &exprs[0] {
        Expr::Sym(s) | Expr::Str(s) => s.clone(),
        _ => {
            return Err(DslError::new(
                ErrorKind::InvalidSyntax,
                "object name must be a symbol or string".to_string()
            ));
        }
    };
    
    let mesh_type = match &exprs[1] {
        Expr::Sym(s) => s.clone(),
        _ => {
            return Err(DslError::new(
                ErrorKind::InvalidSyntax,
                "object type must be a symbol".to_string()
            ));
        }
    };
    
    let mut transform = TransformDef {
        position: [0.0, 0.0, 0.0],
        rotation: [0.0, 0.0, 0.0],
        scale: [1.0, 1.0, 1.0],
    };
    let mut material = None;
    let mut behavior = None;
    let mut interactive = false;
    let mut meta = None;
    
    // Parse properties
    let mut i = 2;
    while i < exprs.len() {
        if let Expr::List(props) = &exprs[i] {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "position" => {
                            transform.position = parse_vec3(&props[1..])?;
                        }
                        "rotation" => {
                            transform.rotation = parse_vec3(&props[1..])?;
                        }
                        "scale" => {
                            transform.scale = parse_vec3(&props[1..])?;
                        }
                        "material" => {
                            material = Some(parse_material_def(&props[1..])?);
                        }
                        "behavior" => {
                            if props.len() > 1 {
                                if let Expr::Sym(b) = &props[1] {
                                    behavior = Some(b.clone());
                                }
                            }
                        }
                        "interactive" => {
                            interactive = true;
                        }
                        "meta" => {
                            meta = Some(parse_meta_directive(&props[1..])?);
                        }
                        "color" => {
                            // Support old-style direct color specification
                            // Convert (color r g b) to a mesh-basic material
                            let color = if props.len() >= 4 {
                                [
                                    parse_float(&props[1])?,
                                    parse_float(&props[2])?,
                                    parse_float(&props[3])?,
                                    if props.len() > 4 { parse_float(&props[4])? } else { 1.0 }
                                ]
                            } else {
                                parse_color(&props[1..])?
                            };
                            material = Some(MaterialDef::MeshBasic {
                                color,
                                opacity: color[3],
                                transparent: color[3] < 1.0,
                                side: "front".to_string(),
                                wireframe: false,
                            });
                        }
                        _ => {}
                    }
                }
            }
        }
        i += 1;
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

/// Parse UIElement from expression list
pub fn parse_ui_element_from_expr(exprs: &[Expr]) -> DslResult<UIElement> {
    if exprs.len() < 2 {
        return Err(DslError::new(
            ErrorKind::InvalidSyntax,
            "ui-element requires name and type".to_string()
        ));
    }
    
    let name = match &exprs[0] {
        Expr::Sym(s) | Expr::Str(s) => s.clone(),
        _ => {
            return Err(DslError::new(
                ErrorKind::InvalidSyntax,
                "ui-element name must be a symbol or string".to_string()
            ));
        }
    };
    
    let ui_type = match &exprs[1] {
        Expr::Sym(s) => s.clone(),
        _ => {
            return Err(DslError::new(
                ErrorKind::InvalidSyntax,
                "ui-element type must be a symbol".to_string()
            ));
        }
    };
    
    let mut position = [0.0, 0.0, 0.0];
    let mut size = [1.0, 1.0];
    let mut text = None;
    let mut color = [1.0, 1.0, 1.0, 1.0];
    let mut behavior = None;
    
    // Parse properties
    let mut i = 2;
    while i < exprs.len() {
        if let Expr::List(props) = &exprs[i] {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "position" => {
                            position = parse_vec3(&props[1..])?;
                        }
                        "size" => {
                            size = parse_vec2(&props[1..])?;
                        }
                        "text" => {
                            if props.len() > 1 {
                                if let Expr::Str(t) = &props[1] {
                                    text = Some(t.clone());
                                }
                            }
                        }
                        "color" => {
                            color = parse_color(&props[1..])?;
                        }
                        "behavior" => {
                            if props.len() > 1 {
                                if let Expr::Sym(b) = &props[1] {
                                    behavior = Some(b.clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        i += 1;
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

/// Parse CameraDef from expression list
pub fn parse_camera_from_expr(exprs: &[Expr]) -> DslResult<CameraDef> {
    let mut position = [0.0, 5.0, 10.0];
    let mut target = [0.0, 0.0, 0.0];
    let mut up = [0.0, 1.0, 0.0];
    let mut fov = 60.0;
    let mut near = 0.1;
    let mut far = 1000.0;
    let mut meta = None;
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "position" => {
                            position = parse_vec3(&props[1..])?;
                        }
                        "target" => {
                            target = parse_vec3(&props[1..])?;
                        }
                        "up" => {
                            up = parse_vec3(&props[1..])?;
                        }
                        "fov" => {
                            if props.len() > 1 {
                                fov = parse_float(&props[1])?;
                            }
                        }
                        "near" => {
                            if props.len() > 1 {
                                near = parse_float(&props[1])?;
                            }
                        }
                        "far" => {
                            if props.len() > 1 {
                                far = parse_float(&props[1])?;
                            }
                        }
                        "meta" => {
                            meta = Some(parse_meta_directive(&props[1..])?);
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(CameraDef {
        position,
        target,
        up,
        fov,
        near,
        far,
        meta,
    })
}

/// Parse LightingDef from expression list
pub fn parse_lighting_from_expr(exprs: &[Expr]) -> DslResult<LightingDef> {
    let mut ambient = [0.2, 0.2, 0.2];
    let mut directional = None;
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "ambient" => {
                            ambient = parse_vec3(&props[1..])?;
                        }
                        "directional" => {
                            directional = Some(parse_directional_light(&props[1..])?);
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(LightingDef {
        ambient,
        directional,
    })
}

/// Parse InputDef from expression list
pub fn parse_input_from_expr(exprs: &[Expr]) -> DslResult<InputDef> {
    let mut camera_controls = None;
    let mut key_bindings = Vec::new();
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "camera-controls" => {
                            camera_controls = Some(parse_camera_controls(&props[1..])?);
                        }
                        "key-binding" => {
                            if props.len() >= 3 {
                                if let (Expr::Str(key), Expr::Str(action)) = (&props[1], &props[2]) {
                                    let target = if props.len() > 3 {
                                        if let Expr::Str(t) = &props[3] {
                                            Some(t.clone())
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };
                                    key_bindings.push(KeyBinding {
                                        key: key.clone(),
                                        action: action.clone(),
                                        target,
                                    });
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(InputDef {
        camera_controls,
        key_bindings,
    })
}

// Helper functions for parsing basic types

fn parse_vec3(exprs: &[Expr]) -> DslResult<[f32; 3]> {
    if exprs.len() != 3 {
        return Err(DslError::new(
            ErrorKind::InvalidSyntax,
            "expected 3 values for vec3".to_string()
        ));
    }
    
    Ok([
        parse_float(&exprs[0])?,
        parse_float(&exprs[1])?,
        parse_float(&exprs[2])?,
    ])
}

fn parse_vec2(exprs: &[Expr]) -> DslResult<[f32; 2]> {
    if exprs.len() != 2 {
        return Err(DslError::new(
            ErrorKind::InvalidSyntax,
            "expected 2 values for vec2".to_string()
        ));
    }
    
    Ok([
        parse_float(&exprs[0])?,
        parse_float(&exprs[1])?,
    ])
}

fn parse_color(exprs: &[Expr]) -> DslResult<[f32; 4]> {
    if exprs.is_empty() {
        return Ok([1.0, 1.0, 1.0, 1.0]);
    }
    
    // Use the color module's parser
    crate::color::parse_color(exprs)
        .map_err(|e| DslError::new(ErrorKind::InvalidSyntax, e.to_string()))
}

fn parse_float(expr: &Expr) -> DslResult<f32> {
    match expr {
        Expr::F32(f) => Ok(*f),
        Expr::I32(i) => Ok(*i as f32),
        _ => Err(DslError::new(
            ErrorKind::TypeMismatch,
            "expected number".to_string()
        ))
    }
}

fn parse_material_def(exprs: &[Expr]) -> DslResult<MaterialDef> {
    let mut material_type = "mesh-basic".to_string();
    let mut color = [1.0, 1.0, 1.0, 1.0];
    let mut opacity = 1.0;
    let mut transparent = false;
    let mut side = "front".to_string();
    let mut wireframe = false;
    let mut base_color = [1.0, 1.0, 1.0, 1.0];
    let mut metallic = 0.0;
    let mut roughness = 0.5;
    let mut emissive = [0.0, 0.0, 0.0];
    
    if !exprs.is_empty() {
        if let Expr::Sym(t) = &exprs[0] {
            material_type = t.clone();
        }
    }
    
    let mut i = 1;
    while i < exprs.len() {
        if let Expr::List(props) = &exprs[i] {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "color" | "base-color" => {
                            let c = parse_color(&props[1..])?;
                            color = c;
                            base_color = c;
                        }
                        "opacity" => {
                            if props.len() > 1 {
                                opacity = parse_float(&props[1])?;
                                transparent = opacity < 1.0;
                            }
                        }
                        "transparent" => {
                            if props.len() > 1 {
                                if let Expr::Bool(b) = &props[1] {
                                    transparent = *b;
                                }
                            }
                        }
                        "side" => {
                            if props.len() > 1 {
                                if let Expr::Sym(s) = &props[1] {
                                    side = s.clone();
                                }
                            }
                        }
                        "wireframe" => {
                            if props.len() > 1 {
                                if let Expr::Bool(b) = &props[1] {
                                    wireframe = *b;
                                }
                            }
                        }
                        "metallic" => {
                            if props.len() > 1 {
                                metallic = parse_float(&props[1])?;
                            }
                        }
                        "roughness" => {
                            if props.len() > 1 {
                                roughness = parse_float(&props[1])?;
                            }
                        }
                        "emissive" => {
                            if props.len() >= 4 {
                                emissive = parse_vec3(&props[1..])?;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        i += 1;
    }
    
    match material_type.as_str() {
        "standard" | "mesh-standard" => {
            Ok(MaterialDef::Standard {
                base_color,
                metallic,
                roughness,
                emissive,
            })
        }
        _ => {
            Ok(MaterialDef::MeshBasic {
                color,
                opacity,
                transparent,
                side,
                wireframe,
            })
        }
    }
}

fn parse_meta_directive(exprs: &[Expr]) -> DslResult<MetaDirective> {
    let mut preserve_mode = "preserve-runtime".to_string();
    let mut properties = Vec::new();
    
    for expr in exprs {
        if let Expr::Sym(s) = expr {
            match s.as_str() {
                "preserve" | "preserve-runtime" => preserve_mode = "preserve-runtime".to_string(),
                "sync-to-code" => preserve_mode = "sync-to-code".to_string(),
                "reset-on-reload" => preserve_mode = "reset-on-reload".to_string(),
                "volatile" => preserve_mode = "volatile".to_string(),
                prop => properties.push(prop.to_string()),
            }
        }
    }
    
    Ok(MetaDirective {
        preserve_mode,
        properties,
    })
}

fn parse_directional_light(exprs: &[Expr]) -> DslResult<DirectionalLight> {
    let mut direction = [1.0, 1.0, 1.0];
    let mut color = [1.0, 1.0, 1.0];
    let mut intensity = 1.0;
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "direction" => {
                            direction = parse_vec3(&props[1..])?;
                        }
                        "color" => {
                            color = parse_vec3(&props[1..])?;
                        }
                        "intensity" => {
                            if props.len() > 1 {
                                intensity = parse_float(&props[1])?;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(DirectionalLight {
        direction,
        color,
        intensity,
    })
}

fn parse_camera_controls(exprs: &[Expr]) -> DslResult<CameraControls> {
    let mut move_speed = 5.0;
    let mut rotate_speed = 2.0;
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
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if !props.is_empty() {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "move-speed" => {
                            if props.len() > 1 {
                                move_speed = parse_float(&props[1])?;
                            }
                        }
                        "rotate-speed" => {
                            if props.len() > 1 {
                                rotate_speed = parse_float(&props[1])?;
                            }
                        }
                        "movement" => {
                            movement_keys = parse_movement_keys(&props[1..])?;
                        }
                        "rotation" => {
                            rotation_keys = parse_rotation_keys(&props[1..])?;
                        }
                        "orbit-controls" => {
                            orbit_controls = Some(parse_orbit_controls(&props[1..])?);
                        }
                        _ => {}
                    }
                }
            }
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

fn parse_movement_keys(exprs: &[Expr]) -> DslResult<MovementKeys> {
    let mut keys = MovementKeys {
        forward: "W".to_string(),
        backward: "S".to_string(),
        left: "A".to_string(),
        right: "D".to_string(),
        up: "Space".to_string(),
        down: "Shift".to_string(),
    };
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if props.len() >= 2 {
                if let (Expr::Sym(key_name), Expr::Sym(key_value)) = (&props[0], &props[1]) {
                    match key_name.as_str() {
                        "forward" => keys.forward = key_value.clone(),
                        "backward" => keys.backward = key_value.clone(),
                        "left" => keys.left = key_value.clone(),
                        "right" => keys.right = key_value.clone(),
                        "up" => keys.up = key_value.clone(),
                        "down" => keys.down = key_value.clone(),
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(keys)
}

fn parse_rotation_keys(exprs: &[Expr]) -> DslResult<RotationKeys> {
    let mut keys = RotationKeys {
        pitch_up: "Up".to_string(),
        pitch_down: "Down".to_string(),
        yaw_left: "Left".to_string(),
        yaw_right: "Right".to_string(),
    };
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if props.len() >= 2 {
                if let (Expr::Sym(key_name), Expr::Sym(key_value)) = (&props[0], &props[1]) {
                    match key_name.as_str() {
                        "pitch-up" => keys.pitch_up = key_value.clone(),
                        "pitch-down" => keys.pitch_down = key_value.clone(),
                        "yaw-left" => keys.yaw_left = key_value.clone(),
                        "yaw-right" => keys.yaw_right = key_value.clone(),
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(keys)
}

fn parse_orbit_controls(exprs: &[Expr]) -> DslResult<OrbitControls> {
    let mut orbit = OrbitControls {
        enabled: false,
        sensitivity: 1.0,
        damping: 0.05,
        min_distance: 1.0,
        max_distance: 100.0,
        min_polar_angle: 0.0,
        max_polar_angle: std::f32::consts::PI,
        enable_zoom: true,
        zoom_speed: 1.0,
    };
    
    for expr in exprs {
        if let Expr::List(props) = expr {
            if props.len() >= 2 {
                if let Expr::Sym(prop_name) = &props[0] {
                    match prop_name.as_str() {
                        "enabled" => {
                            if let Expr::Bool(b) = &props[1] {
                                orbit.enabled = *b;
                            }
                        }
                        "sensitivity" => {
                            orbit.sensitivity = parse_float(&props[1])?;
                        }
                        "damping" => {
                            orbit.damping = parse_float(&props[1])?;
                        }
                        "min-distance" => {
                            orbit.min_distance = parse_float(&props[1])?;
                        }
                        "max-distance" => {
                            orbit.max_distance = parse_float(&props[1])?;
                        }
                        "min-polar-angle" => {
                            orbit.min_polar_angle = parse_float(&props[1])?;
                        }
                        "max-polar-angle" => {
                            orbit.max_polar_angle = parse_float(&props[1])?;
                        }
                        "enable-zoom" => {
                            if let Expr::Bool(b) = &props[1] {
                                orbit.enable_zoom = *b;
                            }
                        }
                        "zoom-speed" => {
                            orbit.zoom_speed = parse_float(&props[1])?;
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(orbit)
}