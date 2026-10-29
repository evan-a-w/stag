open! Import

external eval_toplevel
  :  Typed.Program.t
  -> string
  -> Debug_intf.Gc_stress.t
  -> Interp_value.Outcome.t
  = "stag_rust_interp_eval_toplevel"

let eval_toplevel ?(gc_stress = Debug_intf.Gc_stress.Off) program ~name =
  match eval_toplevel program name gc_stress with
  | Value value -> Ok value
  | Error message -> Or_error.error_string message
;;
