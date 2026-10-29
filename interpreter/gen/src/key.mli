open! Import

(** A declaration's key, see [Ppx_rust_runtime.Key]. *)

type t = R.Key.t [@@deriving sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

(** As [Typed.Expr.t]. *)
val to_string : t -> string
