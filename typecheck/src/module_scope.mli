open! Import

(** A module of a program, as the scope names are resolved in. Its members are
    those of the declarations added to it so far, and those its declaration
    splices generate, once forced. *)
type t [@@deriving sexp_of]

(** A declaration splice: [module M = .~e], or [include .~e]. *)
module Splice : sig
  type module_ := t
  type t

  (** [e], which generates the declarations. *)
  val expr : t -> Syntax.Expr.t

  val loc : t -> Loc.t

  (** The module the splice is written in, where [expr] is checked. *)
  val module_ : t -> module_
end

(** A root module, of canonical names under [path] (none by default), whose
    only members are the types [types]. Names not found in it are looked up in
    [enclosing].

    [declare] is called on each declaration added to it or its modules, with
    the module it is directly in and the splice that generated it, if any,
    before its names become visible. [force]
    generates a splice's declarations, the first time they are needed: when a
    path goes through a spliced module, when a name is not found among the
    members of a module known so far (the module's includes may define it),
    and by [iter_declarations]. Needing a splice's declarations while forcing
    it is an error.

    While a splice is being forced, a name already known in an enclosing
    module, or a prim, is found there without forcing the includes of the
    modules within it (they may need what is being forced): an include then
    defining the name is an error. *)
val create_root
  :  ?enclosing:t
  -> ?path:string list
  -> types:string list
  -> declare:(t -> Syntax.Toplevel.t -> generated_by:Splice.t option -> unit)
  -> force:(Splice.t -> (Syntax.Toplevel.t list, Typecheck_error.t) Result.t)
  -> unit
  -> t

(** Adds the members of [toplevels], declared in [t], creating their modules,
    unless a name would not be unique: a member per module, a parameter per
    type. *)
val add : t -> Syntax.Toplevel.t list -> (unit, Typecheck_error.t) Result.t

(** The declarations of [t], in order, an include's in its place, once
    generated. *)
val declarations : t -> Syntax.Toplevel.t list

(** Calls [f] on each declaration of [t] and its modules, depth first in order,
    with the module it is directly in and the splice that generated it, if
    any, stopping at the first error. Splices
    are forced when reached, and what they generate is visited in their place:
    a spliced module's declarations as those of a module, an include's as if
    written instead of it. Declarations added to a module during the walk are
    visited, unless the walk is already past that module. *)
val iter_declarations
  :  t
  -> f:
       (t
        -> Syntax.Toplevel.t
        -> generated_by:Splice.t option
        -> (unit, Typecheck_error.t) Result.t)
  -> (unit, Typecheck_error.t) Result.t

(** The full path of member [name] of [t], joined with [.]. *)
val canonical_name : t -> string -> string

(** The member module [name] of [t], which must exist. *)
val enter : t -> string -> t

module Namespace : sig
  type t =
    | Value
    | Type
    | Constructor
    | Field
end

(** The canonical name [path] refers to from [t], resolved lexically, forcing
    the splices it needs, the need located at [loc]. [None] if the last name
    of [path] is not a member, [Unbound_module] at [loc] if a module on the
    way is not. *)
val find
  :  t
  -> Namespace.t
  -> Syntax.Path.t
  -> loc:Loc.t
  -> (string option, Typecheck_error.t) Result.t
