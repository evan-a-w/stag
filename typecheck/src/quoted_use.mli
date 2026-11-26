open! Import

(** A use of a toplevel being checked, [awaited], in a quote of other code: it
    is referred to by symbol, at a type of its own, unified with the toplevel's
    once it is known (see [finish_scc] and [settle]). *)

(** The use is in the quote of [toplevel] at [loc], in an instance of its
    scheme. *)
module Via : sig
  type t =
    { toplevel : string
    ; loc : Loc.t
    }
end

type t =
  { awaited : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  (** Of the use, or of the instance of the scheme it is in ([via]). *)
  ; via : Via.t list
  (** Outermost first: the quote of the scheme instantiated at [loc] first. *)
  ; reporter : Reporter.t (** Of the code at [loc]. *)
  }
[@@deriving sexp_of]

(** Of a quote of other code, in an instance of its scheme (see [Awaiting]), so
    never a recursive use. *)
val in_instance : t -> bool

(** [t], awaited by the scheme of [toplevel], in an instance of it at [loc]
    (itself [via] quotes) by code [reporter] reports in. *)
val instance
  :  t
  -> toplevel:string
  -> ty:Type.ty
  -> loc:Loc.t
  -> via:Via.t list
  -> reporter:Reporter.t
  -> t

val error : t -> Unification_error.t -> Typecheck_error.t
