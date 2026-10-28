open! Import

(** An [include%inline] or [module%inline] whose splice ran: the declarations
    written as its expansion, and those the splice generated, which are what
    it declares. *)
type t =
  { toplevel : Syntax.Toplevel.t (** As written, with its expansion. *)
  ; generated : Syntax.Toplevel.t list
  }
[@@deriving sexp_of]
