open! Import

(** The allocation sites of a toplevel's run time code, as it is checked, placed
    once the function they belong to is: the toplevel, or a function a [let] in
    it binds and generalizes. *)
type t

val create : Typed.Ident.t -> t
val add : t -> Mode_rules.Site.t -> unit

(** A mark, before the sites of a function bound by a [let]. *)
val mark : t -> int

(** Places the sites added since [mark] as those of a function of type [ty],
    before it is generalized: of the [let] binding [local], else the toplevel. *)
val place
  :  t
  -> since:int
  -> ty:Type.ty
  -> local:(string * Loc.t) option
  -> unit

(** Every site placed, in program order. *)
val placed : t -> Typed.Allocation.t list
