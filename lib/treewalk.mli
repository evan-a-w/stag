open! Core

(** The bindings of one activation: a closure's captures and arguments, and
    the locals bound while evaluating its body (or the toplevels). Idents are
    unique, so a binding is never shadowed or removed. *)
module Env : sig
  type 'value t

  val create : unit -> _ t

  (** Errors if [ident] is unbound, or declared but not yet [set]. *)
  val lookup : 'value t -> ident:Typed.Ident.t -> 'value Or_error.t

  (** Binds [ident] to an empty slot, to be [set] later, eg. for each member
      of a recursive binding before evaluating their bodies. *)
  val declare : _ t -> ident:Typed.Ident.t -> unit

  (** Fills the slot of [ident], binding it first if needed. *)
  val set : 'value t -> ident:Typed.Ident.t -> value:'value -> unit

  (** A new env sharing the slots of [captures] with [t], so slots filled
      later are seen by both. *)
  val of_captures
    :  'value t
    -> captures:Typed.Ident.Hash_set.t
    -> 'value t Or_error.t
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

(** [stamps] should be the counter that made the idents of the programs
    added, so idents minted while evaluating are distinct from theirs. *)
val create : stamps:Stamp_counter.t -> unit -> t

val create' : stamps:Stamp_counter.t -> program:Typed.Program.t -> t
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
