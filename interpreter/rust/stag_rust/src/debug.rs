//! OCaml externals showing what values are converted to, for tests.

use std::ffi::CString;
use std::fmt::Debug;

use ocaml_sys::Value;
use stag_ast::{convert, to_ocaml, type_, typed, FromOcaml, ToOcaml};

use crate::ocaml_string;

/// `v` converted, and the arena of the conversion, or why it failed.
unsafe fn show<T: FromOcaml + Debug>(v: Value) -> Value {
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

/// `v` converted to Rust and built back, raising `Failure` if either fails.
/// With `gc_stress`, the GC runs before every allocation of the building.
unsafe fn round_trip<T: FromOcaml + ToOcaml>(v: Value, gc_stress: Value) -> Value {
    let built = convert::<T>(v)
        .and_then(|(x, arena)| to_ocaml(&x, &arena, ocaml_sys::int_val(gc_stress) != 0));
    match built {
        Ok(v) => v,
        Err(error) => {
            let message = CString::new(error.to_string()).unwrap_or_default();
            ocaml_sys::caml_failwith(message.as_ptr());
            unreachable!()
        }
    }
}

/// Whether `a` and `b` convert to equal values with equal arenas, so are
/// equal, and share and are cyclic through stored values alike.
unsafe fn same<T: FromOcaml + PartialEq>(a: Value, b: Value) -> Value {
    let shown = match (convert::<T>(a), convert::<T>(b)) {
        (Ok(a), Ok(b)) if a == b => "same".to_owned(),
        (Ok(_), Ok(_)) => "different".to_owned(),
        (Err(error), _) | (_, Err(error)) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

macro_rules! round_trip {
    ($ty:ty, $round_trip:ident, $same:ident, $ocaml:literal) => {
        #[doc = concat!("# Safety\n\n`v` must be a `", $ocaml, "`.")]
        #[no_mangle]
        pub unsafe extern "C" fn $round_trip(v: Value, gc_stress: Value) -> Value {
            round_trip::<$ty>(v, gc_stress)
        }

        #[doc = concat!("# Safety\n\n`a` and `b` must be `", $ocaml, "`s.")]
        #[no_mangle]
        pub unsafe extern "C" fn $same(a: Value, b: Value) -> Value {
            same::<$ty>(a, b)
        }
    };
}

round_trip!(
    Vec<typed::Toplevel>,
    stag_rust_debug_round_trip_typed_toplevels,
    stag_rust_debug_same_typed_toplevels,
    "Typed.Toplevel.t list"
);
round_trip!(
    typed::Program,
    stag_rust_debug_round_trip_typed_program,
    stag_rust_debug_same_typed_program,
    "Typed.Program.t"
);
round_trip!(
    stag_ast::Ref<type_::Ty>,
    stag_rust_debug_round_trip_ty,
    stag_rust_debug_same_ty,
    "Type.ty"
);
round_trip!(
    stag_ast::syntax::Program,
    stag_rust_debug_round_trip_syntax_program,
    stag_rust_debug_same_syntax_program,
    "Syntax.Program.t"
);
