(** The values the Rust interpreter ([interpreter/rust/stag_rust/src/interp])
    returns to OCaml: a printable mirror of its own values, built by
    [stag_ast::to_ocaml]. *)

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
  type nonrec t =
    | Value of t
    | Error of string
  [@@deriving rust]
end
