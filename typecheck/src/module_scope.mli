open! Import

(** A module of a program, as the scope names are resolved in. Its members are
    those of the declarations added to it so far. *)
type t [@@deriving sexp_of]

(** The root module, whose only members are the types [types]. *)
val create_root : types:string list -> t

(** Adds the members of [toplevels], declared in [t], creating their modules,
    unless a name would not be unique: a member per module, a parameter per
    type. Calls [declare] on each declaration, depth first in order, with the
    module it is directly in, before its names become visible. *)
val add
  :  t
  -> Syntax.Toplevel.t list
  -> declare:(t -> Syntax.Toplevel.t -> unit)
  -> (unit, Typecheck_error.t) Result.t

(** The declarations added to [t], in order. *)
val declarations : t -> Syntax.Toplevel.t list

(** Calls [f] on each declaration of [t] and its modules, depth first in order,
    with the module it is directly in, stopping at the first error.
    Declarations added to a module during the walk are visited, unless the
    walk is already past that module. *)
val iter_declarations
  :  t
  -> f:(t -> Syntax.Toplevel.t -> (unit, 'e) Result.t)
  -> (unit, 'e) Result.t

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

(** The canonical name [path] refers to from [t], resolved lexically. [None] if
    the last name of [path] is not a member, [Unbound_module] at [loc] if a
    module on the way is not. *)
val find
  :  t
  -> Namespace.t
  -> Syntax.Path.t
  -> loc:Loc.t
  -> (string option, Typecheck_error.t) Result.t
