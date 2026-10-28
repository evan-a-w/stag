open! Import

module Namespace : sig
  type t =
    | Type
    | Constructor
    | Field
    | Value
end

(** [toplevel] with [f] applied to each of its [Decl_path]s, including those of
    the declarations of its module if it is one, but not those of expansions,
    given the [locals] in scope there (empty but for values), nor those of
    list literals, which name the prelude's [Cons] and [Nil] anywhere. *)
val map
  :  Syntax.Toplevel.t
  -> f:
       (Namespace.t
        -> locals:String.Set.t
        -> Syntax.Decl_path.t Located.t
        -> Syntax.Decl_path.t Located.t)
  -> Syntax.Toplevel.t
