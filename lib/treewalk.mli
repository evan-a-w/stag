open! Core

module Env : sig
  type 'value t 
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

val create : unit -> t
val create' : program:Typed.Program.t -> t

val add_program : t -> program:Typed.Program.t -> unit

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
