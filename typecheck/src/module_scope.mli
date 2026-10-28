open! Import

(** The scope names are resolved in at a position in a module of a program, or
    at its end (as seen from outside it).

    A module's members are those of its declarations, and those its
    declaration splices generate, once forced. An include's declarations are
    visible only after it: from a position, the members visible are those
    declared other than by includes, and those of the includes before it. Those
    not found there are looked for in the enclosing module, at the position of
    the module's declaration. *)
type t [@@deriving sexp_of]

(** A declaration splice: [module M = .~e], or [include .~e]. *)
module Splice : sig
  type scope := t
  type t

  (** [e], which generates the declarations. *)
  val expr : t -> Syntax.Expr.t

  val loc : t -> Loc.t

  (** Where the splice is written, where [expr] is checked: for an include,
      just before it. *)
  val module_ : t -> scope
end

(** The end of a root module, of canonical names under [path] (none by
    default), whose only members are the types [types]. Names not found in it
    are looked up in [enclosing].

    [declare] is called on each declaration added to it or its modules, with
    the scope at its position and the splice that generated it, if any, before
    its names become visible. [force] generates a splice's declarations, once,
    the first time they are needed: when a path goes through a spliced module,
    when a name is not found among a module's members known so far, from a
    position after includes that may define it, and by [iter_declarations]. A
    splice that failed fails again with the same error. Needing a splice's
    declarations while forcing it is an error. *)
val create_root
  :  ?enclosing:t
  -> ?path:string list
  -> types:string list
  -> declare:(t -> Syntax.Toplevel.t -> generated_by:Splice.t option -> unit)
  -> force:(Splice.t -> (Syntax.Toplevel.t list, Typecheck_error.t) Result.t)
  -> unit
  -> t

(** Adds the members of [toplevels], declared in the root module [t], once,
    creating their modules, unless a name would not be unique: a member per
    module, a parameter per type. *)
val add : t -> Syntax.Toplevel.t list -> (unit, Typecheck_error.t) Result.t

(** The declarations of [t]'s module, in order, an include's in its place, once
    generated. *)
val declarations : t -> Syntax.Toplevel.t list

(** Calls [f] on each declaration of [t]'s module and its modules, depth first
    in order, with the scope at its position and the splice that generated it,
    if any, stopping at the first error. Splices are forced when reached, and
    what they generate is visited in their place: a spliced module's
    declarations as those of a module, an include's as if written instead of
    it. *)
val iter_declarations
  :  t
  -> f:
       (t
        -> Syntax.Toplevel.t
        -> generated_by:Splice.t option
        -> (unit, Typecheck_error.t) Result.t)
  -> (unit, Typecheck_error.t) Result.t

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

(** The canonical name [path] refers to from [t], forcing the splices it
    needs, the need located at [loc]. Errs with [unbound path] if the last name
    of [path] is not a member, [Unbound_module] if a module on the way is not.
    Needing what a running splice generates is an error, as is going past a
    running include to find [path] further out, which it may shadow. *)
val find
  :  t
  -> Namespace.t
  -> Syntax.Path.t
  -> loc:Loc.t
  -> unbound:(Syntax.Path.t -> Typecheck_error.Kind.t)
  -> (string, Typecheck_error.t) Result.t

module Value : sig
  type t =
    | Toplevel of string (** By canonical name. *)
    | Prim of Prim.t
    | Unbound (** A name, not a path. *)
end

(** As [find] for a value, then a prim, found only where the program has
    nothing of its name: no value, or no module for a path. *)
val find_value
  :  t
  -> Syntax.Path.t
  -> loc:Loc.t
  -> (Value.t, Typecheck_error.t) Result.t

(** Whether a module of the program in the root module [t] has a type or module
    that would shadow the prelude's name [relative] ([Prelude_path.shadowed]). *)
val shadows_prelude : t -> string -> bool
