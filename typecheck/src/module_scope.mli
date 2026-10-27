open! Import

(** A module of a program, as the scope names are resolved in. Its members are
    those of the declarations added to it so far. *)
type t [@@deriving sexp_of]

(** The root module, with no members. *)
val create_root : unit -> t

(** Adds the members of [toplevels], declared in [t], creating their modules.
    Checks member names are unique per module. Returns the added declarations,
    depth first in order, with the module each is directly in. *)
val add
  :  t
  -> Syntax.Toplevel.t list
  -> ((t * Syntax.Toplevel.t) list, Typecheck_error.t) Result.t

(** The declarations added to [t], in order. *)
val declarations : t -> Syntax.Toplevel.t list

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
