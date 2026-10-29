//! OCaml externals showing what values are converted to, for tests.

use std::collections::HashMap;
use std::fmt::Debug;

use ocaml_sys::Value;
use stag_ast::{
    convert, convert_into, to_ocaml, type_, typed, Arena, FromOcaml, GcStress, ToOcaml,
};
use stag_ast_example as example;

use crate::ocaml_string;

extern "C" {
    fn caml_failwith_value(message: Value) -> !;
}

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
unsafe fn round_trip<T: FromOcaml + ToOcaml>(v: Value, gc_stress: Value) -> Value {
    let gc_stress = GcStress::of_ocaml(gc_stress);
    let message = match convert::<T>(v).and_then(|(x, arena)| to_ocaml(&x, &arena, gc_stress)) {
        Ok(v) => return v,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    caml_failwith_value(message)
}

/// `a` and `b` converted one after the other into one arena, the number of
/// types it has, and both built back.
///
/// # Safety
///
/// `a` and `b` must be `Type.ty`s.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_round_trip_tys_into_one_arena(
    a: Value,
    b: Value,
    gc_stress: Value,
) -> Value {
    let gc_stress = GcStress::of_ocaml(gc_stress);
    let mut arena = Arena::default();
    let message = match convert_into::<stag_ast::Ref<type_::Ty>>(a, &mut arena)
        .and_then(|a| Ok((a, convert_into::<stag_ast::Ref<type_::Ty>>(b, &mut arena)?)))
        .and_then(|(a, b)| to_ocaml(&(arena.type_ty.len() as i64, a, b), &arena, gc_stress))
    {
        Ok(v) => return v,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    drop(arena);
    caml_failwith_value(message)
}

/// As [`round_trip`], of the example declarations' crate.
unsafe fn example_round_trip<T: example::FromOcaml + example::ToOcaml>(
    v: Value,
    gc_stress: Value,
) -> Value {
    let gc_stress = match GcStress::of_ocaml(gc_stress) {
        GcStress::Off => example::GcStress::Off,
        GcStress::Minor => example::GcStress::Minor,
        GcStress::Compact => example::GcStress::Compact,
    };
    let message = match example::convert::<T>(v)
        .and_then(|(x, arena)| example::to_ocaml(&x, &arena, gc_stress))
    {
        Ok(v) => return v,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    caml_failwith_value(message)
}

/// As [`same`], of the example declarations' crate.
unsafe fn example_same<T: example::FromOcaml + PartialEq>(a: Value, b: Value) -> Value {
    let shown = match (example::convert::<T>(a), example::convert::<T>(b)) {
        (Ok(a), Ok(b)) if a == b => "same".to_owned(),
        (Ok(_), Ok(_)) => "different".to_owned(),
        (Err(error), _) | (_, Err(error)) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
}

/// A key type whose OCaml map callback is not registered.
#[derive(PartialEq, Eq, Hash)]
struct Unregistered;

impl ToOcaml for Unregistered {
    unsafe fn to_ocaml(&self, _: &mut stag_ast::Builder) -> Value {
        ocaml_sys::val_int(0)
    }
}

impl stag_ast::OcamlKey for Unregistered {
    const MAP: &'static str = "ppx_rust.map.unregistered";
    const SET: &'static str = "ppx_rust.set.unregistered";
}

/// Why building a map of a key type without callbacks fails.
///
/// # Safety
///
/// Must be called from OCaml.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_debug_missing_callback(_: Value) -> Value {
    let map = stag_ast::Map(HashMap::from([(Unregistered, 1i64)]));
    let shown = match to_ocaml(&map, &stag_ast::Arena::default(), GcStress::Off) {
        Ok(_) => "built".to_owned(),
        Err(error) => format!("error: {error}"),
    };
    ocaml_string::of_str(&shown)
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

macro_rules! example_round_trip {
    ($ty:ty, $round_trip:ident, $same:ident, $ocaml:literal) => {
        #[doc = concat!("# Safety\n\n`v` must be a `", $ocaml, "`.")]
        #[no_mangle]
        pub unsafe extern "C" fn $round_trip(v: Value, gc_stress: Value) -> Value {
            example_round_trip::<$ty>(v, gc_stress)
        }

        #[doc = concat!("# Safety\n\n`a` and `b` must be `", $ocaml, "`s.")]
        #[no_mangle]
        pub unsafe extern "C" fn $same(a: Value, b: Value) -> Value {
            example_same::<$ty>(a, b)
        }
    };
}

example_round_trip!(
    example::example::Expr,
    stag_rust_debug_round_trip_example_expr,
    stag_rust_debug_same_example_expr,
    "Example.Expr.t"
);
example_round_trip!(
    example::Ref<example::example::Ty>,
    stag_rust_debug_round_trip_example_ty,
    stag_rust_debug_same_example_ty,
    "Example.ty"
);
example_round_trip!(
    example::example::defs::Expr,
    stag_rust_debug_round_trip_example_defs_expr,
    stag_rust_debug_same_example_defs_expr,
    "Example.Defs.expr"
);
example_round_trip!(
    example::example::Chained,
    stag_rust_debug_round_trip_example_chained,
    stag_rust_debug_same_example_chained,
    "Example.chained"
);
