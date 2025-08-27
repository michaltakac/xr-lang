//! Lower DSL AST to SSA IR

use crate::ast::*;
use ir::{Module as IrMod, Func as IrFunc, Inst, Ty, Sig, Val, ssa::BlockData};

pub struct LowerCtx {
    pub module: IrMod,
    locals: Vec<(String, Val, Ty)>,
    state_slots: Vec<(String, u32)>,
    next_val: u32,
}

pub fn lower_file(tops: &[Top]) -> anyhow::Result<IrMod> {
    let mut lc = LowerCtx::new();
    for top in tops {
        match top {
            Top::Behavior(b) => lc.lower_behavior(b)?,
            Top::Scene3D(_) => {
                // Scene3D is handled by the renderer, not lowered to IR
            }
        }
    }
    Ok(lc.module)
}

impl LowerCtx {
    pub fn new() -> Self {
        Self {
            module: IrMod { funcs: vec![] },
            locals: vec![],
            state_slots: vec![],
            next_val: 0,
        }
    }
    
    fn lower_behavior(&mut self, b: &Behavior) -> anyhow::Result<()> {
        self.state_slots.clear();
        for (i, (name, _)) in b.state.iter().enumerate() {
            self.state_slots.push((name.clone(), i as u32));
        }
        
        // Generate init function
        let mut f_init = IrFunc {
            name: format!("{}_init", b.name),
            sig: Sig {
                params: vec![Ty::I32, Ty::Ptr], // entity, world
                ret: Ty::I32, // bool as i32
            },
            blocks: vec![BlockData {
                params: vec![],
                insts: vec![],
            }],
            extern_calls: vec![],
        };
        
        // Initialize state slots
        for (i, (_, val)) in b.state.iter().enumerate() {
            let world = self.param_val(1);
            let ent = self.param_val(0);
            let slot = self.const_i32(&mut f_init, i as i32);
            let v = self.const_f32(&mut f_init, *val);
            f_init.blocks[0].insts.push(Inst::CallExtern {
                out: None,
                name: "state_set_f32".into(),
                args: vec![world, ent, slot, v],
            });
        }
        
        let one = self.const_i32(&mut f_init, 1);
        f_init.blocks[0].insts.push(Inst::Ret { val: Some(one) });
        self.module.funcs.push(f_init);
        
        // Generate update function
        let mut f_upd = IrFunc {
            name: format!("{}_update", b.name),
            sig: Sig {
                params: vec![Ty::I32, Ty::Ptr, Ty::F32], // entity, world, dt
                ret: Ty::Unit,
            },
            blocks: vec![BlockData {
                params: vec![],
                insts: vec![],
            }],
            extern_calls: vec![],
        };
        
        self.locals.clear();
        let ent = self.param_val(0);
        let world = self.param_val(1);
        let dt = self.param_val(2);
        
        self.lower_expr(&mut f_upd, &b.update.body, world, ent, dt)?;
        f_upd.blocks[0].insts.push(Inst::Ret { val: None });
        self.module.funcs.push(f_upd);
        
        // Generate on_select if present
        if let Some(ref on_sel) = b.on_select {
            let mut f_sel = IrFunc {
                name: format!("{}_on_select", b.name),
                sig: Sig {
                    params: vec![Ty::I32, Ty::Ptr], // entity, world
                    ret: Ty::Unit,
                },
                blocks: vec![BlockData {
                    params: vec![],
                    insts: vec![],
                }],
                extern_calls: vec![],
            };
            
            self.locals.clear();
            let ent = self.param_val(0);
            let world = self.param_val(1);
            
            self.lower_expr(&mut f_sel, &on_sel.body, world, ent, self.param_val(2))?;
            f_sel.blocks[0].insts.push(Inst::Ret { val: None });
            self.module.funcs.push(f_sel);
        }
        
        Ok(())
    }
    
    fn lower_expr(
        &mut self,
        f: &mut IrFunc,
        e: &Expr,
        world: Val,
        ent: Val,
        _dt: Val,
    ) -> anyhow::Result<Option<Val>> {
        match e {
            Expr::F32(x) => Ok(Some(self.const_f32(f, *x))),
            Expr::I32(x) => Ok(Some(self.const_i32(f, *x))),
            Expr::Bool(x) => Ok(Some(self.const_i32(f, if *x { 1 } else { 0 }))),
            Expr::Sym(s) => {
                // Check locals first
                if let Some((_, v, _)) = self.locals.iter().rev().find(|(n, _, _)| n == s) {
                    return Ok(Some(*v));
                }
                
                // Check state slots
                if let Some((_, slot)) = self.state_slot_id(s) {
                    let slotv = self.const_i32(f, slot as i32);
                    let out = self.tmp_val();
                    f.blocks[0].insts.push(Inst::CallExtern {
                        out: Some(out),
                        name: "state_get_f32".into(),
                        args: vec![world, ent, slotv],
                    });
                    return Ok(Some(out));
                }
                
                anyhow::bail!("unknown symbol: {}", s)
            }
            Expr::List(v) => {
                if v.is_empty() {
                    return Ok(None);
                }
                
                let head = &v[0];
                
                // Special forms
                if head.is_symbol("let") {
                    let Expr::List(bindings) = &v[1] else {
                        anyhow::bail!("let bindings must be list")
                    };
                    
                    let mut temps = vec![];
                    for b in bindings {
                        let Expr::List(pair) = b else {
                            anyhow::bail!("let binding must be pair")
                        };
                        let Expr::Sym(name) = &pair[0] else {
                            anyhow::bail!("let binding name must be symbol")
                        };
                        let val = self.lower_expr(f, &pair[1], world, ent, _dt)?.unwrap();
                        temps.push((name.clone(), val));
                    }
                    
                    // Extend environment
                    let base_len = self.locals.len();
                    for (n, v) in temps {
                        self.locals.push((n, v, Ty::F32)); // assume f32 for now
                    }
                    
                    let res = self.lower_expr(f, &v[2], world, ent, _dt)?;
                    self.locals.truncate(base_len);
                    return Ok(res);
                }
                
                if head.is_symbol("set!") {
                    let Expr::Sym(var) = &v[1] else {
                        anyhow::bail!("set! variable must be symbol")
                    };
                    let value = self.lower_expr(f, &v[2], world, ent, _dt)?.unwrap();
                    
                    if let Some((_, slot)) = self.state_slot_id(var) {
                        let slotv = self.const_i32(f, slot as i32);
                        f.blocks[0].insts.push(Inst::CallExtern {
                            out: None,
                            name: "state_set_f32".into(),
                            args: vec![world, ent, slotv, value],
                        });
                    } else {
                        anyhow::bail!("unknown state variable: {}", var);
                    }
                    return Ok(None);
                }
                
                // Arithmetic operations
                if head.is_symbol("+") {
                    let a = self.lower_expr(f, &v[1], world, ent, _dt)?.unwrap();
                    let b = self.lower_expr(f, &v[2], world, ent, _dt)?.unwrap();
                    let out = self.tmp_val();
                    f.blocks[0].insts.push(Inst::AddF32 { out, a, b });
                    return Ok(Some(out));
                }
                
                if head.is_symbol("*") {
                    let a = self.lower_expr(f, &v[1], world, ent, _dt)?.unwrap();
                    let b = self.lower_expr(f, &v[2], world, ent, _dt)?.unwrap();
                    let out = self.tmp_val();
                    f.blocks[0].insts.push(Inst::MulF32 { out, a, b });
                    return Ok(Some(out));
                }
                
                // Math functions
                if head.is_symbol("sin") {
                    let x = self.lower_expr(f, &v[1], world, ent, _dt)?.unwrap();
                    let out = self.tmp_val();
                    f.blocks[0].insts.push(Inst::CallExtern {
                        out: Some(out),
                        name: "fast_sin_f32".into(),
                        args: vec![x],
                    });
                    return Ok(Some(out));
                }
                
                // GPU operations
                if head.is_symbol("gpu.panel.color") {
                    let r = self.lower_expr(f, &v[1], world, ent, _dt)?.unwrap();
                    let g = self.lower_expr(f, &v[2], world, ent, _dt)?.unwrap();
                    let b = self.lower_expr(f, &v[3], world, ent, _dt)?.unwrap();
                    let a = self.lower_expr(f, &v[4], world, ent, _dt)?.unwrap();
                    f.blocks[0].insts.push(Inst::CallExtern {
                        out: None,
                        name: "gpu_panel_set_color".into(),
                        args: vec![world, r, g, b, a],
                    });
                    return Ok(None);
                }
                
                anyhow::bail!("unknown form: {:?}", head)
            }
        }
    }
    
    fn state_slot_id(&self, name: &str) -> Option<(String, u32)> {
        self.state_slots
            .iter()
            .find(|(n, _)| n == name)
            .cloned()
    }
    
    fn param_val(&self, idx: usize) -> Val {
        Val(idx as u32)
    }
    
    fn tmp_val(&mut self) -> Val {
        let v = Val(self.next_val);
        self.next_val += 1;
        v
    }
    
    fn const_f32(&mut self, f: &mut IrFunc, k: f32) -> Val {
        let out = self.tmp_val();
        f.blocks[0].insts.push(Inst::ConstF32 { out, k });
        out
    }
    
    fn const_i32(&mut self, f: &mut IrFunc, k: i32) -> Val {
        let out = self.tmp_val();
        f.blocks[0].insts.push(Inst::ConstI32 { out, k });
        out
    }
}