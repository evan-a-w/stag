open! Import

(** The scope names are resolved in at a position in a module of a program, or
    at its end (as seen from outside it).

    A module's members are those of its declarations, and those its declaration
    splices generate, once forced. An include's declarations are visible only
    after it: from a position, the members visible are those declared other than
    by includes, and those of the includes before it. Those not found there are
    looked for in the enclosing module, at the position of the module's
    declaration. *)
type t [@@deriving sexp_of]

(** A declaration splice: [module M = .~e], or [include .~e]. *)
module Splice : sig
  type scope := t
  type t

  (** [e], which generates the declarations, or for the definition of a type
      [type t = .~e], the definition, a [Stag.Type_decl.Kind.t]. *)
  val expr : t -> Syntax.Expr.t

  (** The [Module_splice], [Include] or [Type] it is. *)
  val toplevel : t -> Syntax.Toplevel.t

  (** That of [toplevel], up to the end of [expr] if it has an expansion; for a
      type's definition, of [.~e]. *)
  val loc : t -> Loc.t

  (** Of a type's definition: it generates that type, defined. *)
  val defines_type : t -> bool

  (** Where the splice is written, where [expr] is checked: for an include, just
      before it. *)
  val module_ : t -> scope

  (** What it generated, once it has. *)
  val generated : t -> Syntax.Toplevel.t list option
end

(** Why a splice generated no declarations. [Uses_unfinished]: its expression
    uses a toplevel whose checking was underway when it was forced, so it is
    left to be forced again. *)
module Force_error : sig
  type t =
    | Failed of Typecheck_error.t
    | Uses_unfinished
end

(** The end of a root module, of canonical names under [path] (none by default),
    whose only members are the types [types]. Names not found in it are looked
    up in [enclosing].

    [declare] is called on each declaration added to it or its modules, with the
    scope at its position and the splice that generated it, if any, before its
    names become visible. [force] generates a splice's declarations, once, the
    first time they are needed: when a path goes through a spliced module, when
    a name is not found among a module's members known so far, from a position
    after includes that may define it, and by [iter_declarations]. Needing a
    splice's declarations while forcing it is an error.

    Errors not returned are passed to [report]: a splice's error, once, after
    which it generates nothing, and those of declarations whose names are not
    unique, which are dropped. Names not found where a failed splice may have
    generated them are [Typecheck_error.already_reported]. *)
val create_root
  :  ?enclosing:t
  -> ?path:string list
  -> types:string list
  -> declare:(t -> Syntax.Toplevel.t -> generated_by:Splice.t option -> unit)
  -> force:(Splice.t -> (Syntax.Toplevel.t list, Force_error.t) Result.t)
  -> report:(Typecheck_error.t -> unit)
  -> unit
  -> t

(** Adds the members of [toplevels], declared in the root module [t], once,
    creating their modules. Those whose names are not unique (a member per
    module, a parameter per type) are reported instead. *)
val add : t -> Syntax.Toplevel.t list -> unit

(** The declarations of [t]'s module, in order, an include's in its place, once
    generated. *)
val declarations : t -> Syntax.Toplevel.t list

(** Calls [f] on each declaration of [t]'s module and its modules, depth first
    in order, with the scope at its position and the splice that generated it,
    if any. Splices are forced when reached, and what they generate is visited
    in their place: a spliced module's declarations as those of a module, an
    include's as if written instead of it. *)
val iter_declarations
  :  t
  -> f:(t -> Syntax.Toplevel.t -> generated_by:Splice.t option -> unit)
  -> unit

(** The full path of member [name] of [t]'s module, joined with [.]. *)
val canonical_name : t -> string -> string

(** The end of the member module [name] of [t]'s module, which must exist. *)
val enter : t -> string -> t

module Namespace : sig
  type t =
    | Value
    | Type
    | Constructor
    | Field
end

(** The canonical name the written path [path] refers to from [t], or from the
    program's root module for a root path, forcing the splices it needs, the
    need located at [loc]. Errs with [unbound path] if the last name of [path]
    is not a member, [Unbound_module] if a module on the way is not, noting an
    include after [t] that generates its first name. Needing what a running
    splice generates is an error: not found, noting the include that may define
    it, or, found past (or as a prim outside) a running include that may shadow
    it, [Declaration_splice_cycle]. *)
val find
  :  t
  -> Namespace.t
  -> Syntax.Decl_path.t
  -> loc:Loc.t
  -> unbound:(Syntax.Decl_path.t -> Typecheck_error.Kind.t)
  -> (string, Typecheck_error.t) Result.t

module Value : sig
  type t =
    | Toplevel of string (** By canonical name. *)
    | Prim of Prim.t
    | Intrinsic of Intrinsic.t (** Found only from within the prelude. *)
    | Unbound (** A name, not a path or a root path. *)
end

(** As [find] for a value, then a prim, found only where the program has nothing
    of its name: no value, or no module for a path. *)
val find_value
  :  t
  -> Syntax.Decl_path.t
  -> loc:Loc.t
  -> (Value.t, Typecheck_error.t) Result.t

(** The error for the name [path] that [find_value] finds [Unbound]. *)
val unbound_value : t -> Syntax.Decl_path.t -> loc:Loc.t -> Typecheck_error.t

(** [error] with the types and modules in scope at [t] that shadow the
    prelude's, unless it has them: those of the modules enclosing [t] in the
    program not generated by includes, so that whether checking reached an
    include does not change messages. *)
val within : t -> Typecheck_error.t -> Typecheck_error.t

(** Computes the definition of the type [name] of [t]'s module, declared as
    [type name = .~e], unless done: [force] generates the type, defined, which
    is declared as generated by the splice, and its constructors or fields are
    members of the module. They are also computed when a constructor or field is
    not found in the module. Errs at [loc] if it is being computed. *)
val force_definition
  :  t
  -> string
  -> loc:Loc.t
  -> (unit, Typecheck_error.t) Result.t

(** Whether the definition of [name] is being computed. *)
val defining : t -> string -> bool

(** The declaration splices of [t]'s module and its modules, those generated
    included. *)
val splices : t -> Splice.t list

(** A shortest path naming what [path] names (a [Decl_path.Resolved] its
    canonical name) where it is used in [toplevels], which [splice] generated,
    within their modules [modules] (outermost first) where the values [locals]
    are bound, as if [toplevels] were written in place of [splice] and their
    declarations added there: a suffix of its full path naming it there, else
    its root path if that does, if any. *)
val path_from_splice
  :  Splice.t
  -> Syntax.Toplevel.t list
  -> Namespace.t
  -> modules:string list
  -> locals:String.Set.t
  -> Syntax.Decl_path.t
  -> Syntax.Decl_path.t option
