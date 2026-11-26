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
  ; failed : bool ref (** It turned out wrong, and was reported. *)
  ; templates : bool ref list
  (** The [failed] of the uses it is an instance of (see [local_instance]): if
      one of them failed, it is wrong as they are, so isn't reported again. *)
  }
[@@deriving sexp_of]

val create
  :  awaited:Typed.Ident.t
  -> ty:Type.ty
  -> loc:Loc.t
  -> reporter:Reporter.t
  -> t

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

(** [t], generalized with the scheme of a local, in an instance of it at [loc]
    by code [reporter] reports in, as if [t] were written at [loc]. *)
val local_instance : t -> ty:Type.ty -> loc:Loc.t -> reporter:Reporter.t -> t

val fail : t -> unit
val template_failed : t -> bool
val error : t -> Unification_error.t -> Typecheck_error.t
