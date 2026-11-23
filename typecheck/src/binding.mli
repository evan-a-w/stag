open! Import

(** The mode of a variable, as bound by the rules of [Mode_rules.Binding]. *)
type t = private Mode.Var.t [@@deriving sexp_of]

(** Only for [Mode_rules.Binding]. *)
val of_mode : Mode.Var.t -> t
