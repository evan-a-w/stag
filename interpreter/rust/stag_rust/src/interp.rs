use ocaml_sys::Value as OcamlValue;
use stag_ast::{convert, interp_value, to_ocaml, typed, Arena, GcStress, InterpValue, Str};

use crate::ocaml_string;

extern "C" {
    fn caml_failwith_value(message: OcamlValue) -> !;
}

pub struct Interp<'p> {
    pub program: &'p typed::Program,
    pub arena: &'p Arena,
}

impl<'p> Interp<'p> {
    pub fn new(program: &'p typed::Program, arena: &'p Arena) -> Self {
        Interp { program, arena }
    }

    pub fn eval_toplevel(&mut self, name: &Str) -> Result<InterpValue, String> {
        Err(format!("not implemented: eval_toplevel {name:?}"))
    }
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
    let outcome = match Interp::new(&program, &arena).eval_toplevel(&name) {
        Ok(value) => interp_value::Outcome::Value(value),
        Err(error) => interp_value::Outcome::Error(error.into()),
    };
    // [Code] values refer to types in [arena], so it is built with it.
    let message = match to_ocaml(&outcome, &arena, gc_stress) {
        Ok(outcome) => return outcome,
        Err(error) => ocaml_string::of_str(&error.0),
    };
    drop((program, arena, outcome));
    caml_failwith_value(message)
}
