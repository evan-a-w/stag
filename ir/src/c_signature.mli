open! Import

(** The C signature of an [external]: how its stag type maps to C types, by the
    C ABI. A curried [a -> b -> r] is the C function [r f(a, b)], and
    [unit -> r] is [r f(void)]. *)

module Arg : sig
  type t =
    | Int (** [int64_t]. *)
    | Bool (** [bool]. *)
    | Char (** [char]. *)
    | C_int of C_int.t (** A stag [int], checked to be in its range. *)
    | String
    (** [const char *]: a NUL-terminated copy, valid only during the call. *)
  [@@deriving sexp_of, equal, compare, hash]
end

module Result : sig
  type t =
    | Int
    | Bool
    | Char
    | Unit (** [void]. *)
    | C_int of C_int.t (** Sign- or zero-extended to a stag [int]. *)
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
    | Pointer of Type.ty (** A [ptr] argument: not supported yet. *)
    | Pointer_result of Type.ty
    (** A [string] or [ptr] result: not supported yet. *)
  [@@deriving sexp_of]
end

val of_type : Type.ty -> (t, Problem.t) Core.Result.t

(** The type of the external as a stag value, of a type [of_type] accepts: [ty]
    with its C integer types ([C.int] etc.) as [int], and the same modes. *)
val value_type : Type.ty -> Type.ty
