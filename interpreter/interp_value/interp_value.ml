open! Core

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Construct of string * t list
  | Record of (string * t) list
  | Code of Stag_ir.Typed.Expr.t
  | Closure
  | Prim of string
[@@deriving rust, sexp_of]

module Outcome = struct
  type nonrec t =
    | Value of t
    | Error of string
  [@@deriving rust]
end
