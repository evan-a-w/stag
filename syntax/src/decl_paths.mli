open! Import

module Namespace : sig
  type t =
    | Type
    | Constructor
    | Field
end

(** [toplevel] with [f] applied to each of its [Decl_path]s, including those of
    the declarations of its module if it is one, but not those of expansions. *)
val map
  :  Syntax.Toplevel.t
  -> f:
       (Namespace.t
        -> Syntax.Decl_path.t Located.t
        -> Syntax.Decl_path.t Located.t)
  -> Syntax.Toplevel.t
