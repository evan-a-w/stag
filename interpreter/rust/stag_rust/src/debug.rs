//! OCaml externals showing what values are converted to, for tests.

use std::fmt::Debug;

use ocaml_sys::Value;
use stag_ast::{convert, type_, typed, FromOcaml};

use crate::ocaml_string;

/// `v` converted, and the arena of the conversion, or why it failed.
unsafe fn show<T: FromOcaml + Debug + Send>(v: Value) -> Value {
    let shown = match convert::<T>(v) {
        Ok((x, arena)) => format!("{x:#?}\n{arena:#?}"),
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

/// # Safety
///
/// `v` must be a `Typed.Toplevel.t list`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_typed_toplevels(v: Value) -> Value {
    show::<Vec<typed::Toplevel>>(v)
}

/// How many toplevels and types there are, for programs too big to print.
///
/// # Safety
///
/// `v` must be a `Typed.Toplevel.t list`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_typed_toplevels_summary(v: Value) -> Value {
    let shown = match convert::<Vec<typed::Toplevel>>(v) {
        Ok((toplevels, arena)) => {
            format!(
                "toplevels: {}, tys: {}",
                toplevels.len(),
                arena.type_ty.len()
            )
        }
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

/// How much of the program there is, as it is too big to print.
///
/// # Safety
///
/// `v` must be a `Typed.Program.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_typed_program_summary(v: Value) -> Value {
    let shown = match convert::<typed::Program>(v) {
        Ok((program, arena)) => format!(
            "sccs: {}\nexpect_tests: {}\nbenches: {}\nident_info: {}\n\
             arena: prims {}, record types {}, tys {}, rows {}, ty vars {}, row vars {}, \
             presence vars {}",
            program.sccs.len(),
            program.expect_tests.len(),
            program.benches.len(),
            program.ident_info.len(),
            arena.prim.len(),
            arena.typed_record_type.len(),
            arena.type_ty.len(),
            arena.type_row.len(),
            arena.type_var_ty_nothing.len(),
            arena.type_var_row_bound.len(),
            arena.type_var_presence_presence_bound.len(),
        ),
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

/// # Safety
///
/// `v` must be a `Type.ty`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_ty(v: Value) -> Value {
    show::<stag_ast::Ref<type_::Ty>>(v)
}

/// The number of types in the arena of `v`'s conversion.
///
/// # Safety
///
/// `v` must be a `Type.ty`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_ty_count(v: Value) -> Value {
    let shown = match convert::<stag_ast::Ref<type_::Ty>>(v) {
        Ok((_, arena)) => format!("tys: {}", arena.type_ty.len()),
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

/// # Safety
///
/// `v` must be a `Syntax.Program.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_syntax_program(v: Value) -> Value {
    show::<stag_ast::syntax::Program>(v)
}

/// # Safety
///
/// `v` must be a `Typed.Program.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_typed_ident_info(v: Value) -> Value {
    let shown = match convert::<typed::Program>(v) {
        Ok((program, _)) => format!("{:#?}", program.ident_info),
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}
