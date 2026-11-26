open! Import

(** The C signature of an [external]: how its stag type maps to C types, by the
    C ABI. A curried [a -> b -> r] is the C function [r f(a, b)], and
    [unit -> r] is [r f(void)]. *)

module Arg : sig
  type t =
    | Int (** [int64_t]. *)
    | Bool (** [bool]. *)
    | Char (** [char]. *)
  [@@deriving sexp_of, equal, compare, hash]
end

module Result : sig
  type t =
    | Int
    | Bool
    | Char
    | Unit (** [void]. *)
  [@@deriving sexp_of, equal, compare, hash]
end

type t =
  { args : Arg.t list (** Empty for [unit -> r]. *)
  ; result : Result.t
  }
[@@deriving sexp_of, equal, compare, hash]

(** The number of arguments of its stag type, at least one. *)
val arity : t -> int

module Problem : sig
  type t =
    | Not_a_function
    | Argument of Type.ty (** Of a type C functions can't take. *)
    | Result of Type.ty (** Of a type C functions can't return. *)
    | Unit_argument (** [unit] as an argument, but not the only one. *)
    | Pointer of Type.ty
    (** A [string] or [ptr]: passing pointers to C is not supported yet. *)
  [@@deriving sexp_of]
end

val of_type : Type.ty -> (t, Problem.t) Core.Result.t
