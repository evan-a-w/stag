open! Core

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Construct of string * t list (** By the constructor's canonical name. *)
  | Record of (string * t) list (** By the fields' canonical names. *)
  | Code of Stag_ir.Typed.Expr.t
  | Closure
  | Prim of string (** Partially applied. *)
[@@deriving rust, sexp_of]

module Outcome : sig
  type 'a t =
    | Ok of 'a
    | Error of string
  [@@deriving rust]

  val to_or_error : 'a t -> 'a Or_error.t
end
