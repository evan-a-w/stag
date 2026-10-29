(** Declarations deriving [rust], for tests: see [interpreter/gen/test], the
    [stag_ast_example] crate, and their round trips in
    [interpreter/stag_rust/test]. A [Record_type.t] named ["raise"] raises
    when rebuilt. *)

open! Core

type id = int [@@deriving rust, sexp_of]

module Pos : sig
  type t =
    { line : int
    ; col : int
    }
  [@@deriving rust, sexp_of]
end

type 'a located =
  { txt : 'a
  ; pos : Pos.t
  }
[@@deriving rust, sexp_of]

module Name : sig
  type t = string located [@@deriving rust, sexp_of]
end

module Record_type : sig
  type t =
    { name : string
    ; fields : string array
    ; ignored : unit -> unit [@rust.skip] [@sexp.opaque]
    ; arity : int
    }
  [@@deriving rust, sexp_of] [@@rust.shared]
end

type 'a var =
  { id : id
  ; mutable link : 'a option
  }

and ty =
  | Int
  | Arrow of ty * ty
  | Var of ty var
  | Record of
      { record : Record_type.t
      ; skipped : int -> int [@rust.skip] [@sexp.opaque]
      ; fields : (string * ty) list
      }
  | Unit
  | Tuple of (ty * Name.t)
  | Chars of char * bool array
[@@deriving rust, sexp_of] [@@rust.shared]

module Expr : sig
  module Desc : sig
    type 'e t =
      | Lit of int
      | App of 'e * 'e
      | Let of
          { name : Name.t
          ; body : 'e
          ; in_ : 'e
          }
    [@@deriving rust, sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : ty
    ; env : ty String.Map.t
    ; free : String.Set.t
    ; ids : int Core.Int.Map.t
    ; next : t option
    }
  [@@deriving rust, sexp_of]

  type nonrec ty = ty [@@deriving rust, sexp_of]
end

module Defs : sig
  type expr = expr_desc located

  and expr_desc =
    | Const of char * bool
    | Pair of expr * expr option
    | Never of Core.Nothing.t
    | Absurd of Nothing.t list
  [@@deriving rust, sexp_of]
end

module Re : sig
  type t = Defs.expr [@@deriving rust, sexp_of]
end

module Var : sig
  type 'a t = 'a var [@@deriving rust, sexp_of]
end

module Chain : sig
  type 'a t = 'a Var.t [@@deriving rust, sexp_of]
end

type chained = { var : ty Chain.t } [@@boxed] [@@deriving rust, sexp_of]
