use std::collections::HashMap;

use stag_ast::{typed, Arena, GcStress, InterpValue};

use super::Stamps;

pub struct Interp {
    pub arena: Arena,
    pub ident_info: HashMap<typed::Ident, typed::IdentInfo>,
    pub sccs: Vec<typed::Scc>,
    pub gc_stress: GcStress,
}

fn not_implemented<T>(op: &str) -> Result<T, String> {
    Err(format!("not implemented: {op}"))
}

impl Interp {
    pub fn new(gc_stress: GcStress) -> Interp {
        Interp {
            arena: Arena::default(),
            ident_info: HashMap::new(),
            sccs: Vec::new(),
            gc_stress,
        }
    }

    pub fn summary(&self) -> String {
        let members: usize = self.sccs.iter().map(|scc| scc.members.len()).sum();
        format!(
            "sccs: {}, members: {members}, ident infos: {}, arena: {:?}",
            self.sccs.len(),
            self.ident_info.len(),
            self.arena.store_lens()
        )
    }

    pub fn splice(
        &mut self,
        _code: &typed::Expr,
        _level: i64,
        _stamps: &mut Stamps,
    ) -> Result<typed::Expr, String> {
        not_implemented("splice")
    }

    pub fn run(&mut self, _expr: &typed::Expr, _level: i64) -> Result<InterpValue, String> {
        not_implemented("run")
    }

    pub fn global_info(&self, _ident: &typed::Ident) -> Option<typed::IdentInfo> {
        None
    }

    pub fn eval(&mut self) -> Result<(), String> {
        not_implemented("eval")
    }

    pub fn run_expect_test(&mut self, _test: &typed::ExpectTest) -> Result<(), String> {
        not_implemented("run_expect_test")
    }

    pub fn eval_expr(
        &mut self,
        _bindings: &[(typed::Ident, InterpValue)],
        _expr: &typed::Expr,
    ) -> Result<InterpValue, String> {
        not_implemented("eval_expr")
    }

    pub fn eval_toplevel(&mut self, _ident: &typed::Ident) -> Result<(), String> {
        not_implemented("eval_toplevel")
    }
}
