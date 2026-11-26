open! Import

(** The allocation sites of a toplevel's run time code, as it is checked, placed
    once its component is. *)
type t

(** A [let] being checked: the sites in its function, or in the functions of its
    value, are its own if it is generalized, as each of its instances decides
    where they are made. *)
module Frame : sig
  type t

  (** As the [let]'s [Typed.Expr.Made.owns]. *)
  val id : t -> Typed.Allocation.Id.t

  val set_generalized : t -> unit
end

(** Each site and frame is named by [fresh]. *)
val create : Typed.Ident.t -> fresh:(unit -> Typed.Allocation.Id.t) -> t

(** A [let] of type [ty], in [parent] (else directly in the toplevel). *)
val frame
  :  t
  -> parent:Frame.t option
  -> name:string
  -> loc:Loc.t
  -> ty:Type.ty
  -> Frame.t

(** A site, in the function of [frame] (else of the toplevel), named. *)
val add
  :  t
  -> Mode_rules.Site.t
  -> frame:Frame.t option
  -> Typed.Allocation.Id.t

(** Where each site of a component's toplevels (each with its type) is made, in
    program order: once they are checked and before they are generalized at
    [rank], so that every constraint on their modes, but those of instances,
    holds.

    A site is made as locally as the upper bound of its mode allows in the least
    instance of its owners: every mode its bound depends on that generalizing
    their types wouldn't summarize away ([Mode.Var.Node.Fate.Eliminated]) is at
    its lower bound. Those it would have every constraint they will have, so
    that bound is at most what any instance gives. A site also follows a side of
    an owner it is in when the instances with that side [local] (and the rest at
    their least) make it more locally.

    [awaiting] are the types of the component's uses, in quotes, of toplevels
    still being checked, generalized with it: refining its schemes once those
    finish may yet constrain their modes, which are pinned like the schemes'. *)
val place
  :  rank:int
  -> (t * Type.ty) list
  -> awaiting:Type.ty list
  -> Typed.Allocation.t list
