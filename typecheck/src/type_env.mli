open! Import

(** User type declarations, the constructors and fields they define, and the
    elaboration of surface types. Names are canonical. A declaration is checked
    on first use of its constructors, fields or checked declaration. Elaboration
    of nominal references does not check their declarations; layout checking
    inspects by-value dependencies without publishing their checked
    declarations. *)
type t

(** The builtin types, as declarations of the root module. *)
val builtins : Syntax.Type_decl.t list

(** With [record_references], [references] are recorded. *)
val create
  :  ?record_references:bool
  -> Fresh.t
  -> report:(Typecheck_error.t -> unit)
  -> t

(** Declares [decl], a member of [module_], whose parameters are distinct. Types
    may refer to each other in any order, once declared. With [lazily], its
    parameters' ids are taken when first needed rather than now. Errors checking
    it are passed through [wrap_error]. *)
val declare
  :  ?lazily:bool
  -> ?wrap_error:(Typecheck_error.t -> Typecheck_error.t)
  -> t
  -> Module_scope.t
  -> Syntax.Type_decl.t
  -> unit

(** Declares the exception [name], a constructor of [exn], a member of
    [module_]. Its constructor's [index] is [0]: exceptions are told apart by
    canonical name. A [malformed] one, whose syntax error is the parser's, is
    [Already_reported] when checked. *)
val declare_exception
  :  ?wrap_error:(Typecheck_error.t -> Typecheck_error.t)
  -> t
  -> Module_scope.t
  -> Syntax.Name.t
  -> Syntax.Type_expr.t list
  -> malformed:bool
  -> unit

(** The checked declaration of the type of canonical name [name]. Its error, the
    first time, is given to [create]'s [report], and [Already_reported]
    returned. *)
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
    ; global_args : bool list
    }
  [@@deriving sexp_of]
end

(** Like [Constructor], with the record type's parameters [Generic] in [record]
    and [ty]. *)
module Field : sig
  type t =
    { record_type : Typed.Record_type.t
    ; record : Type.ty
    ; index : int
    ; ty : Type.ty
    ; is_global : bool
    }
  [@@deriving sexp_of]

  (** Canonical. *)
  val name : t -> string
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

(** The fields of the type of canonical name [name], in order, if it is a
    nominal record, checking its declaration. *)
val record_fields
  :  t
  -> string
  -> (Field.t list option, Typecheck_error.t) Result.t

(** The record type of canonical name [name], which must be a nominal record. *)
val record_type
  :  t
  -> string
  -> (Typed.Record_type.t, Typecheck_error.t) Result.t

(** Likewise, its constructors if it is a variant. *)
val variant_constructors
  :  t
  -> string
  -> (Constructor.t list option, Typecheck_error.t) Result.t

(** Like [check], for a type already checked. *)
val find_decl_exn : t -> string -> Signature.Type_decl.t

(** Whether the type of canonical name [name] is a variant with no constructors,
    checking its declaration. *)
val is_empty_variant : t -> string -> (bool, Typecheck_error.t) Result.t

(** The type and row variables of the annotations of one toplevel definition:
    each name stands for the same variable throughout. *)
module Annotation_vars : sig
  type t

  (** Variables are created at [rank] on first use. *)
  val create : Fresh.t -> rank:Type.rank -> t

  (** The mode a mode variable stands for. *)
  val mode : t -> string -> Mode.Var.t
end

(** The type, and the syntax with each type it names by its canonical name
    ([Resolved]), so that it names the same types wherever it is. With
    [splices], each type splice [.~e] is the type [e] evaluates to (once per
    splice), which may not itself contain a splice; without, it is an error, as
    in quotes and compile-time code. *)
val elaborate
  :  t
  -> Module_scope.t
  -> Annotation_vars.t
  -> Syntax.Type_expr.t
  -> splices:bool
  -> (Type.ty * Syntax.Type_expr.t, Typecheck_error.t) Result.t

(** As a declaration's type, [expr] having no type variables and its arrows
    fixed modes, unwritten ones the default. *)
val elaborate_declared
  :  t
  -> Module_scope.t
  -> Syntax.Type_expr.t
  -> (Type.ty, Typecheck_error.t) Result.t

(** How a type splice [.~e] at [loc] is evaluated, in the scope it is written
    in, to the type it gives (located at [loc]). Types and exceptions are always
    declarations, so evaluate theirs. *)
val set_evaluate_splice
  :  t
  -> (Module_scope.t
      -> Syntax.Expr.t
      -> loc:Loc.t
      -> (Syntax.Type_expr.t, Typecheck_error.t) Result.t)
  -> unit

(** Gives the type [name], declared as [type t = .~e], the definition [e]
    evaluated to at [loc]. Errs (reported to [create]'s [report], the type then
    failed) if it contains a type splice, or is an alias while the type was used
    while computing it. Errors checking it are then passed through [wrap_error],
    then as before. *)
val set_definition
  :  t
  -> string
  -> Syntax.Type_decl.Kind.t
  -> wrap_error:(Typecheck_error.t -> Typecheck_error.t)
  -> loc:Loc.t
  -> unit

(** The declaration of the type of canonical name [name], checking it, its types
    [Resolved] where it is declared. [None] if it is abstract, as are the
    builtin types. *)
val reflect
  :  t
  -> string
  -> (Reflected_decl.t option, Typecheck_error.t) Result.t

(** Of the field of canonical name [name]. *)
val field_modality : t -> string -> Modality.t

(** Whether values of [ty] hold no pointers by value: immediates, and tuples,
    records and variants of such types. [false] while unknown, e.g. for a
    variable or an open row. *)
val pointer_free : t -> Type.ty -> bool

(** Records that the name at [loc] is the constructor or field [name], declared
    in [t] (else nothing is recorded). Declared types, types named in type
    expressions and constructors and fields found by path are recorded by [t]
    itself. *)
val record_member_reference : t -> string -> loc:Loc.t -> unit

(** By location of the name, if [create]d to record them. *)
val references : t -> (Loc.t * Name_reference.t) list option
