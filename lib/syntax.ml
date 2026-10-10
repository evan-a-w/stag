open! Core

module Lit = struct
  type t =
    | Int of string
    | Bool of bool
    | String of string
    | Char of char
    | Unit
  [@@deriving sexp, compare, equal, variants]
end

module Binding = struct
  type t = string * Type.ty option [@@deriving sexp, compare, equal]
end

module Expr = struct
  type t =
    | Lit of Lit.t
    | Var of Binding.t
    | App of t * t
    | Let of string * Binding.t list * Type.ty option * t * t
    | Splice of
        { expr : t
        ; subs :
            (string * t) list
              option (* [None] is all in scope, so is diff to [] *)
        }
    | Quote of t
  [@@deriving sexp, compare, equal, variants]
end

module Toplevel = struct
  type t = Let of string * Binding.t list * Type.ty option * Expr.t
  [@@deriving sexp, compare, equal, variants]
end

module Program = struct
  type t = Toplevel.t list [@@deriving sexp, compare, equal]
end
