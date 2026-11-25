open! Import

(** The allocation sites of a toplevel's run time code, as it is checked, placed
    once its component is. *)
type t

(** A [let] being checked: the sites in its function, or in the functions of its
    value, are its own if it is generalized, as each of its instances decides
    where they are made. *)
module Frame : sig
  type t

  val set_generalized : t -> unit
end

val create : Typed.Ident.t -> t

(** A [let] of type [ty], in [parent] (else directly in the toplevel). *)
val frame
  :  t
  -> parent:Frame.t option
  -> name:string
  -> loc:Loc.t
  -> ty:Type.ty
  -> Frame.t

(** A site, in the function of [frame] (else of the toplevel). *)
val add : t -> Mode_rules.Site.t -> frame:Frame.t option -> unit

(** Where each site of a component's toplevels (each with its type) is made, in
    program order: once they are checked and before they are generalized, so
    that every constraint on their modes, but those of instances, holds.

    A site is made as locally as the upper bound of its mode allows in the least
    instance of its owners, with every side of their types, and every other mode
    its bound depends on that isn't internal to the component (those of schemes,
    and those code outside may still constrain), at its lower bound. The modes
    internal to the component have every constraint they will have, so that
    bound is at most what any instance gives. A site also follows a side of an
    owner it is in when the instances with that side [local] (and the rest at
    their least) make it more locally. *)
val place : (t * Type.ty) list -> Typed.Allocation.t list
