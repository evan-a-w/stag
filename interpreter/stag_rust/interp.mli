(** The Rust interpreter ([interpreter/rust/stag_rust/src/interp]), a stub
    for now. *)

open! Import

(** Evaluates the last toplevel of [program] named [name], and the toplevels
    it uses. The [Error]s are the interpreter's: failing to convert raises. *)
val eval_toplevel
  :  ?gc_stress:Debug_intf.Gc_stress.t
  -> Typed.Program.t
  -> name:string
  -> Interp_value.t Or_error.t
