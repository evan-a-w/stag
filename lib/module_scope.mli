open! Core

(** A module of a program, as the scope names are resolved in. *)
type t [@@deriving sexp_of]

(** The root module of [program]. Checks member names are unique per module. *)
val create : Syntax.Program.t -> (t, Typecheck_error.t) Result.t

(** The full path of member [name] of [t], joined with [.]. *)
val canonical_name : t -> string -> string

(** Every toplevel of the program, depth first in program order, with the
    module it is directly in. *)
val toplevels : t -> (t * Syntax.Toplevel.t) list

module Namespace : sig
  type t =
    | Value
    | Type
    | Constructor
    | Field
end

(** The canonical name [path] refers to from [t], resolved lexically. [None] if
    the last name of [path] is not a member, [Unbound_module] if a module on
    the way is not. *)
val find
  :  t
  -> Namespace.t
  -> Syntax.Path.t
  -> (string option, Typecheck_error.t) Result.t
