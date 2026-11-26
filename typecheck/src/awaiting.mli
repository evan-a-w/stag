open! Import

(** The schemes of a finished component whose code used toplevels still being
    checked, in quotes: the uses' types are generalized with the schemes, so
    each instance of a scheme has its own instances of the uses, which are
    unified with the toplevels' types like the code's own uses. Once a toplevel
    used is finished, the schemes are refined in place: their variables are
    reopened, the uses of it unified with instances of its scheme, and they are
    generalized anew, so the code, whose types share the variables, is refined
    too. *)
type t =
  { schemes : (string * Type.ty) list (** Each member's name and type. *)
  ; locals : Type.ty list
  (** The types of the locals of the members' code generalized with uses (see
      [Local_uses]), which share variables with them: refined with them. *)
  ; mutable uses : Quoted_use.t list
  }
[@@deriving sexp_of]
