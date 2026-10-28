open! Import

(** Built-in functions, implemented in OCaml. They are in scope in every
    program, outside its root module, so any definition of the same name
    shadows them: [print_int] at the root, and modules such as [Int], whose
    members are named by paths ([Int.add]). *)

(** The values prims take and return. *)
module Value : sig
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Char of char
    | Unit
  [@@deriving sexp_of]
end

(** The exceptions prims raise, declared by the prelude. *)
module Exception : sig
  type t =
    | Division_by_zero
    | Invalid_argument of string
    | Failure of string
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of, equal]

(** Monomorphic, so needs no instantiation. *)
val ty : t -> Type.ty

(** The number of arguments, at least one. *)
val arity : t -> int

(** Applies [t] to [arity t] arguments, of the types of [ty]. Printing goes to
    [output]. *)
val apply
  :  t
  -> output:(string -> unit)
  -> Value.t list
  -> (Value.t, Exception.t) Result.t

(** By full path. *)
val find : Syntax.Path.t -> t option

(** Whether [name] is a built-in module, eg. [Int]. *)
val is_module : string -> bool
