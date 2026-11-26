open! Import

(** The uses some code makes of toplevels being checked (see [Quoted_use]), by
    the [let] whose definition each is in. A [let] generalized generalizes the
    uses of its definition that hold variables it generalizes with its scheme,
    as a toplevel's component does (see [Awaiting]): each instance of the local
    has instances of them, of its own, unified with the toplevel's type once it
    is known. The uses are the enclosing [let]'s too, or the code's, which
    refines those generalized in place once it is checked, when the locals can
    no longer be used ([take_generalized]). *)
type t

(** For code outside any [let]: its uses are lowered to [rank] and handed to
    [dispatch]. *)
val create : rank:Type.Rank.t -> dispatch:(Quoted_use.t -> unit) -> t

(** For the definition of a [let] checked with [t]. *)
val enter : t -> t

val add : t -> Quoted_use.t -> unit

(** The [let] of [t], binding [ident] of type [ty], is checked. If it is
    generalized, by [generalize], that is given [ty] and the types of the uses
    of its definition that hold variables of rank greater than [rank], type or
    mode: those are its templates. Other uses are lowered to [rank] first, so
    are never generalized. All are then [add]ed to the code enclosing the [let]. *)
val leave
  :  t
  -> rank:Type.Rank.t
  -> ident:Typed.Ident.t
  -> ty:Type.ty
  -> generalize:(Type.ty list -> unit) option
  -> unit

(** The uses [ident]'s scheme was generalized with, of which each instance of it
    has its own. *)
val templates : t -> Typed.Ident.t -> Quoted_use.t list

module Generalized : sig
  type t =
    { scheme : Type.ty
    ; templates : Quoted_use.t Nonempty_list.t
    }
end

(** The schemes of the [let]s that generalized uses, oldest first, once the code
    is checked: their variables are to be made ones of the code again, as the
    [let]s are no longer used. *)
val take_generalized : t -> Generalized.t list
