open! Import

module Point : sig
  type t = T : 'a Mode.Axis.t * 'a -> t
end

val point : Syntax.Mode_expr.Atom.t -> Point.t

(** Why the atoms can't be written together: [const] with a runtime provenance. *)
val invalid : Syntax.Mode_expr.Atom.t list -> string option

(** The constant the atoms write: the axes not written are [default]'s, but a
    [const] one is [static]. *)
val declared
  :  default:Mode.Const.t
  -> Syntax.Mode_expr.Atom.t list
  -> loc:Loc.t
  -> (Mode.Const.t, Typecheck_error.t) Result.t

(** The locality written with a mode variable, as [local] in [@ local 'm]. *)
val locality : Syntax.Mode_expr.Atom.t -> Mode.Locality.t
