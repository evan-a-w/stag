open! Import

(** A mode variable whose lower bound exceeds its upper bound on an axis. The
    reasons explain each bound: the first is where the bound came from, the rest
    the constraints that carried it to [var], in order. *)
type t =
  | Mismatch :
      { var : int
      ; axis : 'a Axis.t
      ; lower : 'a
      ; upper : 'a
      ; lower_because : Reason.t list
      ; upper_because : Reason.t list
      }
      -> t
[@@deriving sexp_of]

val create
  :  var:int
  -> axis:Axis.Any.t
  -> lower:int
  -> upper:int
  -> lower_because:Reason.t list
  -> upper_because:Reason.t list
  -> t

(** Raises on [Error]: for constraints that cannot fail. *)
val ok_exn : (unit, t) Result.t -> unit
