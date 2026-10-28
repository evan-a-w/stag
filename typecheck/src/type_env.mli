open! Import

(** User type declarations, the constructors and fields they define, and the
    elaboration of surface types. Names are canonical. A declaration is
    checked on first use of its constructors, fields or checked declaration:
    elaborating a type only needs its parameters, so checking one declaration
    never needs another one checked. *)
type t

(** The builtin types, as declarations of the root module. *)
val builtins : Syntax.Type_decl.t list

val create : Fresh.t -> t

(** Declares [decl], a member of [module_], whose parameters are distinct.
    Types may refer to each other in any order, once declared. With [lazily],
    its parameters' ids are taken when first needed rather than now. Errors
    checking it are passed through [wrap_error]. *)
val declare
  :  ?lazily:bool
  -> ?wrap_error:(Typecheck_error.t -> Typecheck_error.t)
  -> t
  -> Module_scope.t
  -> Syntax.Type_decl.t
  -> unit

(** Declares the exception [name], a constructor of [exn], a member of
    [module_]. Its constructor's [index] is [0]: exceptions are told apart by
    canonical name. *)
val declare_exception
  :  ?wrap_error:(Typecheck_error.t -> Typecheck_error.t)
  -> t
  -> Module_scope.t
  -> Syntax.Name.t
  -> Syntax.Type_expr.t list
  -> unit

(** The checked declaration of the type of canonical name [name]. *)
val check : t -> string -> (Signature.Type_decl.t, Typecheck_error.t) Result.t

(** The parameters of the declaring type are [Generic] in [args] and [result],
    to be instantiated together on each use. *)
module Constructor : sig
  type t =
    { name : string
    ; type_name : string
    ; index : int
    ; args : Type.ty list
    ; result : Type.ty
    }
  [@@deriving sexp_of]
end

(** Like [Constructor], with the record type's parameters [Generic] in
    [record] and [ty]. *)
module Field : sig
  type t =
    { name : string
    ; type_name : string
    ; record : Type.ty
    ; index : int
    ; ty : Type.ty
    ; all_fields : string list
    }
  [@@deriving sexp_of]
end

val find_constructor
  :  t
  -> Module_scope.t
  -> Syntax.Decl_path.t Located.t
  -> (Constructor.t, Typecheck_error.t) Result.t

val find_field
  :  t
  -> Module_scope.t
  -> Syntax.Decl_path.t Located.t
  -> (Field.t, Typecheck_error.t) Result.t

(** The exception of canonical name [name], checking it. *)
val check_exception : t -> string -> (Constructor.t, Typecheck_error.t) Result.t

(** By canonical name, erring at [loc]. *)
val constructor
  :  t
  -> string
  -> loc:Loc.t
  -> (Constructor.t, Typecheck_error.t) Result.t

val field : t -> string -> loc:Loc.t -> (Field.t, Typecheck_error.t) Result.t

(** Like [check], for a type already checked. *)
val find_decl_exn : t -> string -> Signature.Type_decl.t

(** Whether the type of canonical name [name] is a variant with no
    constructors, checking its declaration. *)
val is_empty_variant : t -> string -> (bool, Typecheck_error.t) Result.t

(** The type and row variables of the annotations of one toplevel
    definition: each name stands for the same variable throughout. *)
module Annotation_vars : sig
  type t

  (** Variables are created at [rank] on first use. *)
  val create : Fresh.t -> rank:Type.rank -> t
end

(** The type, and the syntax with each type it names by its canonical name
    ([Resolved]), so that it names the same types wherever it is. *)
val elaborate
  :  t
  -> Module_scope.t
  -> Annotation_vars.t
  -> Syntax.Type_expr.t
  -> (Type.ty * Syntax.Type_expr.t, Typecheck_error.t) Result.t

(** The declaration of the type of canonical name [name], checking it, its
    types [Resolved] where it is declared. [None] if it is abstract, as are
    the builtin types. *)
val reflect
  :  t
  -> string
  -> (Reflected_decl.t option, Typecheck_error.t) Result.t
