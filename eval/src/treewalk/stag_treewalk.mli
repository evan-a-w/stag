open! Import

(** The tree-walking interpreter: evaluates [Typed] programs directly. *)

include module type of struct
  include Treewalk
end

val interpreter : (module Interpreter_intf.S)
val to_interpreter : t -> Interpreter.t

(** Raises if [interpreter] is not a [t]. *)
val of_interpreter : Interpreter.t -> t
