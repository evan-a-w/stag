open! Import

(** A value with the span of source it was written at. *)
type 'a t =
  { txt : 'a
  ; loc : Loc.t
  }
[@@deriving rust]

(** Locations are ignored, so syntax built without them (eg. by tests) equals
    parsed syntax. *)
include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t

include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t

(** As [txt]'s sexp, followed by [@ loc] unless the location is [Loc.none]. *)
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

val create : ?loc:Loc.t -> 'a -> 'a t
