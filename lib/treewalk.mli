open! Core

module Env : sig
  type 'value t =
    { prev : 'value t option
    ; values : 'value Typed.Ident.Table.t
    }
  [@@deriving sexp_of, fields]

  val lookup : 'value t -> ident:Typed.Ident.t -> 'value option
end

module Value : sig
  type t =
    | Int of string
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | Record of t String.Map.t
    | Variant of string * t
    | Function of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; env : t Env.t
        }
  [@@deriving sexp_of, variants]
end

type t

val create : program:Typed.Program.t -> t

(** The binders from outside the closure [ident] that it reads when called,
    including itself if it is recursive. Uses in splices that run when a quote
    in [body] is built, rather than when the closure is called, are not
    captures. Cached by [ident]. *)
val captures
  :  t
  -> ident:Typed.Ident.t
  -> args:Typed.Ident.t list
  -> body:Typed.Expr.t
  -> Typed.Ident.Hash_set.t

val eval : t -> unit
