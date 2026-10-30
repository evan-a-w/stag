use std::collections::HashMap;

use ocaml_sys::{caml_alloc_custom, custom_operations, field, Value as OcamlValue};
use stag_ast::{
    convert_into, interp_value::Outcome, to_ocaml, typed, Arena, Error, GcStress, InterpValue,
    ToOcaml,
};

use crate::ocaml_string;
use crate::stamps::Stamps;

extern "C" {
    fn caml_failwith_value(message: OcamlValue) -> !;
}

pub struct Interp {
    pub arena: Arena,
    pub ident_info: HashMap<typed::Ident, typed::IdentInfo>,
    pub sccs: Vec<typed::Scc>,
    gc_stress: GcStress,
}

fn not_implemented<T>(op: &str) -> Result<T, String> {
    Err(format!("not implemented: {op}"))
}

impl Interp {
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

struct Ops(custom_operations);

unsafe impl Sync for Ops {}

static OPS: Ops = Ops(custom_operations {
    identifier: c"stag_rust.interp".as_ptr(),
    finalize: Some(finalize),
    compare: None,
    hash: None,
    serialize: None,
    deserialize: None,
    compare_ext: None,
    fixed_length: std::ptr::null(),
});

unsafe fn slot(handle: OcamlValue) -> *mut *mut Interp {
    field(handle, 1).cast()
}

unsafe extern "C" fn finalize(handle: OcamlValue) {
    drop(Box::from_raw(*slot(handle)));
}

unsafe fn interp<'a>(handle: OcamlValue) -> &'a mut Interp {
    &mut **slot(handle)
}

/// Runs `f`, which converts OCaml arguments, raising `Failure` if it fails.
/// Nothing may allocate on the OCaml heap within it, so that the arguments
/// do not move.
unsafe fn converted<T>(f: impl FnOnce() -> Result<T, Error>) -> T {
    let message = match f() {
        Ok(x) => return x,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    caml_failwith_value(message)
}

/// `x` built on the OCaml heap, unrooted, so to be returned at once.
unsafe fn reply<T: ToOcaml>(interp: &Interp, x: T) -> OcamlValue {
    let message = match to_ocaml(&x, &interp.arena, interp.gc_stress) {
        Ok(v) => return v,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    drop(x);
    caml_failwith_value(message)
}

fn outcome<T>(result: Result<T, String>) -> Outcome<T> {
    match result {
        Ok(x) => Outcome::Ok(x),
        Err(error) => Outcome::Error(error.into()),
    }
}

/// # Safety
///
/// `gc_stress` must be a `Debug.Gc_stress.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_create(gc_stress: OcamlValue) -> OcamlValue {
    let interp = Box::new(Interp {
        arena: Arena::default(),
        ident_info: HashMap::new(),
        sccs: Vec::new(),
        gc_stress: GcStress::of_ocaml(gc_stress),
    });
    let handle = caml_alloc_custom(&OPS.0, std::mem::size_of::<*mut Interp>(), 0, 1);
    *slot(handle) = Box::into_raw(interp);
    handle
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`, `ident` a
/// `Typed.Ident.t` and `info` a `Typed.Ident_info.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_add_ident_info(
    handle: OcamlValue,
    ident: OcamlValue,
    info: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let (ident, info) = converted(|| {
        Ok((
            convert_into(ident, &mut interp.arena)?,
            convert_into(info, &mut interp.arena)?,
        ))
    });
    interp.ident_info.insert(ident, info);
    ocaml_sys::UNIT
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create` and `scc` a
/// `Typed.Scc.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_add_scc(
    handle: OcamlValue,
    scc: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let scc = converted(|| convert_into(scc, &mut interp.arena));
    interp.sccs.push(scc);
    ocaml_sys::UNIT
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`, `stamps` a
/// `Stamp_counter.t`, `code` a `Typed.Expr.t` and `level` an `int`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_splice(
    handle: OcamlValue,
    stamps: OcamlValue,
    code: OcamlValue,
    level: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let (code, level) = converted(|| {
        Ok((
            convert_into(code, &mut interp.arena)?,
            convert_into(level, &mut interp.arena)?,
        ))
    });
    let mut stamps = Stamps::new(stamps);
    let result = outcome(interp.splice(&code, level, &mut stamps));
    drop(stamps);
    reply(interp, result)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`, `expr` a
/// `Typed.Expr.t` and `level` an `int`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_run(
    handle: OcamlValue,
    expr: OcamlValue,
    level: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let (expr, level) = converted(|| {
        Ok((
            convert_into(expr, &mut interp.arena)?,
            convert_into(level, &mut interp.arena)?,
        ))
    });
    let result = outcome(interp.run(&expr, level));
    reply(interp, result)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create` and `ident` a
/// `Typed.Ident.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_global_info(
    handle: OcamlValue,
    ident: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let ident = converted(|| convert_into(ident, &mut interp.arena));
    let info = interp.global_info(&ident);
    reply(interp, info)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_eval(handle: OcamlValue) -> OcamlValue {
    let interp = interp(handle);
    let result = outcome(interp.eval());
    reply(interp, result)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create` and `test` a
/// `Typed.Expect_test.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_run_expect_test(
    handle: OcamlValue,
    test: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let test = converted(|| convert_into(test, &mut interp.arena));
    let result = outcome(interp.run_expect_test(&test));
    reply(interp, result)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`, `bindings` a
/// `(Typed.Ident.t * Interp_value.t) list` and `expr` a `Typed.Expr.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_eval_expr(
    handle: OcamlValue,
    bindings: OcamlValue,
    expr: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let (bindings, expr): (Vec<_>, _) = converted(|| {
        Ok((
            convert_into(bindings, &mut interp.arena)?,
            convert_into(expr, &mut interp.arena)?,
        ))
    });
    let result = outcome(interp.eval_expr(&bindings, &expr));
    reply(interp, result)
}

/// # Safety
///
/// `handle` must be from `stag_rust_interp_create` and `ident` a
/// `Typed.Ident.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_eval_toplevel(
    handle: OcamlValue,
    ident: OcamlValue,
) -> OcamlValue {
    let interp = interp(handle);
    let ident = converted(|| convert_into(ident, &mut interp.arena));
    let result = outcome(interp.eval_toplevel(&ident));
    reply(interp, result)
}

/// How much is stored, for tests.
///
/// # Safety
///
/// `handle` must be from `stag_rust_interp_create`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_summary(handle: OcamlValue) -> OcamlValue {
    let interp = interp(handle);
    let members: usize = interp.sccs.iter().map(|scc| scc.members.len()).sum();
    ocaml_string::of_str(&format!(
        "sccs: {}, members: {members}, ident infos: {}, arena: {:?}",
        interp.sccs.len(),
        interp.ident_info.len(),
        interp.arena.store_lens()
    ))
}
