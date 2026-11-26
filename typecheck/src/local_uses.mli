open! Import

(** The uses some code makes of toplevels being checked (see [Quoted_use]), by
    the [let] whose definition each is in. A [let] generalized generalizes the
    uses of its definition with its scheme, as a toplevel's component does (see
    [Awaiting]): each instance of the local has instances of them, of its own,
    unified with the toplevel's type once it is known. The uses it generalized
    are the code's too, refined in place once the code is checked, when the
    local can no longer be used ([take_schemes]). *)
type t

(** For code outside any [let]: its uses are lowered to [rank] and handed to
    [dispatch], as are those a [let] generalized, once generalized. *)
val create : rank:Type.Rank.t -> dispatch:(Quoted_use.t -> unit) -> t

(** For the definition of a [let] checked with [t]. *)
val enter : t -> t

val add : t -> Quoted_use.t -> unit

(** Those [add]ed to a [let]'s definition, oldest first. *)
val uses : t -> Quoted_use.t list

(** The [let] of [t], binding [ident], is checked: [templates], of its [uses],
    were generalized with [scheme], so are the code's; the others are of the
    code enclosing the [let], lowered to [rank]. *)
val leave
  :  t
  -> rank:Type.Rank.t
  -> ident:Typed.Ident.t
  -> scheme:Type.ty
  -> templates:Quoted_use.t list
  -> unit

(** Those [ident]'s scheme was generalized with. *)
val generalized : t -> Typed.Ident.t -> Quoted_use.t list

(** The schemes of the [let]s that generalized uses, each with them, oldest
    first, once the code is checked: their variables are to be made ones of the
    code again, as the [let]s are no longer used. *)
val take_schemes : t -> (Type.ty * Quoted_use.t list) list
