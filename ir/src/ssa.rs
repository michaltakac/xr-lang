//! SSA (Static Single Assignment) IR for XR-DSL

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FnId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Block(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Val(pub u32);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Inst {
    // Constants
    ConstF32 { out: Val, k: f32 },
    ConstI32 { out: Val, k: i32 },
    
    // Arithmetic
    AddF32 { out: Val, a: Val, b: Val },
    SubF32 { out: Val, a: Val, b: Val },
    MulF32 { out: Val, a: Val, b: Val },
    DivF32 { out: Val, a: Val, b: Val },
    
    AddI32 { out: Val, a: Val, b: Val },
    SubI32 { out: Val, a: Val, b: Val },
    MulI32 { out: Val, a: Val, b: Val },
    DivI32 { out: Val, a: Val, b: Val },
    
    // Function calls
    Call { out: Option<Val>, func: FnId, args: Vec<Val> },
    CallExtern { out: Option<Val>, name: String, args: Vec<Val> },
    
    // Control flow
    If { cond: Val, then_bb: Block, else_bb: Block },
    Jump { target: Block },
    Ret { val: Option<Val> },
    
    // Memory operations (for later)
    Load { out: Val, addr: Val },
    Store { addr: Val, val: Val },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BlockData {
    pub params: Vec<(super::Ty, Val)>,
    pub insts: Vec<Inst>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Func {
    pub name: String,
    pub sig: super::Sig,
    pub blocks: Vec<BlockData>,
    pub extern_calls: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Module {
    pub funcs: Vec<Func>,
}