open! Import
include Syntax_value_intf.Syntax_value with type value := Value.t

(** A prim's result: lists and options as the prelude's. The inverse of
    [Value.to_prim]. *)
val of_prim : Prim.Value.t -> Value.t
