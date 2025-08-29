//! Model loading functionality for various 3D formats

use crate::mesh_gen::{MeshData, Vertex};
use crate::entity::ModelSource;
use anyhow::{Result, Context};
use std::path::Path;
use std::fs::File;

/// Load a 3D model from file based on its format
pub fn load_model(source: &ModelSource) -> Result<MeshData> {
    match source {
        ModelSource::OBJ { path } => load_obj(path),
        ModelSource::STL { path } => load_stl(path),
        ModelSource::PLY { path } => load_ply(path),
        ModelSource::GLB { path } | ModelSource::GLTF { path } => load_gltf(path),
        _ => Err(anyhow::anyhow!("Model format not yet supported")),
    }
}

/// Load an OBJ model file
fn load_obj(path: &Path) -> Result<MeshData> {
    let (models, _materials) = tobj::load_obj(
        path,
        &tobj::LoadOptions {
            single_index: true,
            triangulate: true,
            ignore_points: true,
            ignore_lines: true,
        },
    )
    .context("Failed to load OBJ file")?;

    if models.is_empty() {
        return Err(anyhow::anyhow!("OBJ file contains no models"));
    }

    // Take the first model for now
    let model = &models[0];
    let mesh = &model.mesh;

    let mut vertices = Vec::new();
    
    // OBJ can have separate position/normal/texcoord indices, but we requested single_index
    for i in 0..mesh.positions.len() / 3 {
        let pos_idx = i * 3;
        
        // Position
        let position = [
            mesh.positions[pos_idx],
            mesh.positions[pos_idx + 1],
            mesh.positions[pos_idx + 2],
        ];

        // Normal (if available)
        let normal = if mesh.normals.len() > pos_idx + 2 {
            [
                mesh.normals[pos_idx],
                mesh.normals[pos_idx + 1],
                mesh.normals[pos_idx + 2],
            ]
        } else {
            // Calculate a default normal if not provided
            [0.0, 1.0, 0.0]
        };

        // Texture coordinates (if available)
        let tex_coords = if i * 2 + 1 < mesh.texcoords.len() {
            [
                mesh.texcoords[i * 2],
                1.0 - mesh.texcoords[i * 2 + 1], // Flip V coordinate for most renderers
            ]
        } else {
            [0.0, 0.0]
        };

        vertices.push(Vertex {
            position,
            normal,
            tex_coords,
        });
    }

    // If no normals were provided, calculate them from faces
    if mesh.normals.is_empty() && !mesh.indices.is_empty() {
        calculate_normals(&mut vertices, &mesh.indices);
    }

    Ok(MeshData {
        vertices,
        indices: mesh.indices.clone(),
    })
}

/// Load an STL model file (binary or ASCII)
fn load_stl(path: &Path) -> Result<MeshData> {
    let mut file = File::open(path).context("Failed to open STL file")?;
    
    // Read STL as indexed mesh  
    let indexed_mesh = stl_io::read_stl(&mut file).context("Failed to parse STL file")?;
    
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    
    // Convert stl_io vertices to our vertex format
    for vertex in indexed_mesh.vertices {
        vertices.push(Vertex {
            position: [vertex[0], vertex[1], vertex[2]],
            normal: [0.0, 1.0, 0.0], // Will be calculated later
            tex_coords: [0.0, 0.0], // STL doesn't have texture coordinates
        });
    }
    
    // Convert face indices to our index buffer
    for face in indexed_mesh.faces {
        // Each face has 3 vertex indices
        for &vertex_index in &face.vertices {
            indices.push(vertex_index as u32);
        }
    }
    
    // Recalculate smooth normals based on shared vertices
    calculate_normals(&mut vertices, &indices);
    
    Ok(MeshData { vertices, indices })
}

/// Load a PLY model file (placeholder for now)
fn load_ply(_path: &Path) -> Result<MeshData> {
    // PLY loading would require additional dependencies or manual parsing
    // For now, return a placeholder cube
    log::warn!("PLY loading not yet implemented, returning placeholder cube");
    Ok(MeshData::from_primitive(&crate::entity::PrimitiveType::cube()))
}

/// Load a glTF/GLB model file (placeholder for now)
fn load_gltf(_path: &Path) -> Result<MeshData> {
    // glTF loading would require the gltf crate
    // For now, return a placeholder cube
    log::warn!("glTF/GLB loading not yet implemented, returning placeholder cube");
    Ok(MeshData::from_primitive(&crate::entity::PrimitiveType::cube()))
}

/// Calculate smooth normals for vertices based on face connectivity
fn calculate_normals(vertices: &mut [Vertex], indices: &[u32]) {
    // Reset all normals to zero
    for vertex in vertices.iter_mut() {
        vertex.normal = [0.0, 0.0, 0.0];
    }
    
    // Calculate face normals and add to vertex normals
    for face in indices.chunks(3) {
        if face.len() != 3 {
            continue;
        }
        
        let i0 = face[0] as usize;
        let i1 = face[1] as usize;
        let i2 = face[2] as usize;
        
        let v0 = vertices[i0].position;
        let v1 = vertices[i1].position;
        let v2 = vertices[i2].position;
        
        // Calculate face normal using cross product
        let edge1 = [
            v1[0] - v0[0],
            v1[1] - v0[1],
            v1[2] - v0[2],
        ];
        let edge2 = [
            v2[0] - v0[0],
            v2[1] - v0[1],
            v2[2] - v0[2],
        ];
        
        let normal = [
            edge1[1] * edge2[2] - edge1[2] * edge2[1],
            edge1[2] * edge2[0] - edge1[0] * edge2[2],
            edge1[0] * edge2[1] - edge1[1] * edge2[0],
        ];
        
        // Add face normal to each vertex
        for &idx in &[i0, i1, i2] {
            vertices[idx].normal[0] += normal[0];
            vertices[idx].normal[1] += normal[1];
            vertices[idx].normal[2] += normal[2];
        }
    }
    
    // Normalize all vertex normals
    for vertex in vertices.iter_mut() {
        let len = (vertex.normal[0].powi(2) + 
                  vertex.normal[1].powi(2) + 
                  vertex.normal[2].powi(2)).sqrt();
        
        if len > 0.0001 {
            vertex.normal[0] /= len;
            vertex.normal[1] /= len;
            vertex.normal[2] /= len;
        } else {
            // Default to up vector if degenerate
            vertex.normal = [0.0, 1.0, 0.0];
        }
    }
}

/// Detect model format from file extension
pub fn detect_format(path: &Path) -> Option<ModelSource> {
    let extension = path.extension()?.to_str()?.to_lowercase();
    let path_buf = path.to_path_buf();
    
    match extension.as_str() {
        "obj" => Some(ModelSource::OBJ { path: path_buf }),
        "stl" => Some(ModelSource::STL { path: path_buf }),
        "ply" => Some(ModelSource::PLY { path: path_buf }),
        "gltf" => Some(ModelSource::GLTF { path: path_buf }),
        "glb" => Some(ModelSource::GLB { path: path_buf }),
        "fbx" => Some(ModelSource::FBX { path: path_buf }),
        "usd" | "usda" => Some(ModelSource::USD { path: path_buf }),
        "usdc" | "usdz" => Some(ModelSource::USDC { path: path_buf }),
        _ => None,
    }
}