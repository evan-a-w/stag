open! Core

(** User type declarations, the constructors and fields they define, and the
    elaboration of surface types. Names are canonical. *)
type t

(** The builtin types, as declarations of the root module. *)
val builtins : Syntax.Type_decl.t list

(** Checks the type declarations of every module of the program [root], which
    may refer to each other in any order. *)
val create
  :  Module_scope.t
  -> fresh_id:(unit -> Type.id)
  -> (t, Typecheck_error.t) Result.t

(** The parameters of the declaring type are [Generic] in [args] and [result],
    to be instantiated together on each use. *)
module Constructor : sig
  type t =
    { name : string
    ; args : Type.ty list
    ; result : Type.ty
    }
  [@@deriving sexp_of]
end

(** Like [Constructor], with the record type's parameters [Generic] in
    [params] and [ty]. The record type is [TCon (type_name, params)]. *)
module Field : sig
  type t =
    { name : string
    ; type_name : string
    ; params : Type.arg list
    ; ty : Type.ty
    ; all_fields : string list
    }
  [@@deriving sexp_of]
end

val find_constructor
  :  t
  -> Module_scope.t
  -> Syntax.Path.t
  -> (Constructor.t, Typecheck_error.t) Result.t

val find_field
  :  t
  -> Module_scope.t
  -> Syntax.Path.t
  -> (Field.t, Typecheck_error.t) Result.t

(** The checked declaration of the type of canonical name [name]. *)
val find_decl_exn : t -> string -> Signature.Type_decl.t

(** The type and context variables of the annotations of one toplevel
    definition: each name stands for the same variable throughout. *)
module Annotation_vars : sig
  type t

  (** Variables are created at [rank] on first use. *)
  val create : fresh_id:(unit -> Type.id) -> rank:Type.rank -> t
end

val elaborate
  :  t
  -> Module_scope.t
  -> Annotation_vars.t
  -> Syntax.Type_expr.t
  -> (Type.ty, Typecheck_error.t) Result.t
