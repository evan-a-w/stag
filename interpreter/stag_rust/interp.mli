open! Import

(** Evaluates in Rust ([interpreter/rust/stag_rust/src/interp.rs]). Failing to
    convert raises. *)
include Stag_eval.Interpreter_intf.S with module Value = Interp_value

val create_with_gc_stress
  :  Debug_intf.Gc_stress.t
  -> stamps:Stamp_counter.t
  -> t

(** How much the Rust side stores, eg. the components added and the arena's
    stores' lengths. *)
val summary : t -> string
