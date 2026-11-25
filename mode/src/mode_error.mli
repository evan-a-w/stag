open! Import

(** A mode whose lower bound exceeds its upper bound on an axis: a variable's
    (by its id), or two constants required equal. The reasons explain each
    bound: the first is where the bound came from, the rest the constraints that
    carried it to [var], in order. *)
type t =
  | Mismatch :
      { var : int option
      ; axis : 'a Axis.t
      ; lower : 'a
      ; upper : 'a
      ; lower_because : Reason.t list
      ; upper_because : Reason.t list
      }
      -> t
[@@deriving sexp_of]

(** Two different constants on [axis] that [reason] requires equal. *)
val unequal : 'a Axis.t -> 'a -> 'a -> Reason.t -> t

(** Raises on [Error]: for constraints that cannot fail. *)
val ok_exn : (unit, t) Result.t -> unit
