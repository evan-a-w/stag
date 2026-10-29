use ocaml_sys::Value as OcamlValue;
use stag_ast::{convert, interp_value, to_ocaml, typed, GcStress, Str};

use super::{error, Interp, Result};
use crate::ocaml_string;

extern "C" {
    fn caml_failwith_value(message: OcamlValue) -> !;
}

/// # Safety
///
/// `program` must be a `Typed.Program.t`, `name` a `string` and
/// `gc_stress` a `Debug.Gc_stress.t`.
#[no_mangle]
pub unsafe extern "C" fn stag_rust_interp_eval_toplevel(
    program: OcamlValue,
    name: OcamlValue,
    gc_stress: OcamlValue,
) -> OcamlValue {
    let gc_stress = GcStress::of_ocaml(gc_stress);
    // Nothing allocates on the OCaml heap until [to_ocaml], so the OCaml
    // arguments stay where they are while converted.
    let converted = convert::<typed::Program>(program)
        .and_then(|program| Ok((program, convert::<Str>(name)?.0)));
    let ((program, arena), name) = match converted {
        Ok(converted) => converted,
        Err(error) => caml_failwith_value(ocaml_string::of_str(&error.0)),
    };
    let outcome = match eval_toplevel(&program, &arena, &name) {
        Ok(value) => interp_value::Outcome::Value(value),
        Err(error) => interp_value::Outcome::Error(error.0.into()),
    };
    // [Code] values refer to types in [arena], so it is built with it.
    let message = match to_ocaml(&outcome, &arena, gc_stress) {
        Ok(outcome) => return outcome,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    drop((program, arena, outcome));
    caml_failwith_value(message)
}

fn eval_toplevel(
    program: &typed::Program,
    arena: &stag_ast::Arena,
    name: &Str,
) -> Result<stag_ast::InterpValue> {
    let toplevel = program
        .sccs
        .iter()
        .flat_map(|scc| &scc.members)
        .rev()
        .find(|toplevel| toplevel.ident.name == *name);
    let Some(toplevel) = toplevel else {
        return error(format!("no toplevel {name:?}"));
    };
    let mut interp = Interp::new(program, arena);
    let value = interp.toplevel(&toplevel.ident)?;
    Ok(value.to_interp_value(arena))
}
