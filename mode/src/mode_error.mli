open! Import

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
  (** A variable (by its id) whose lower bound exceeds its upper bound on an
      axis. The reasons explain each bound: the first is where the bound came
      from, the rest the constraints that carried it to [var], in order. *)
  | Unequal :
      { axis : 'a Axis.t
      ; first : 'a
      ; second : 'a
      ; because : Reason.t
      }
      -> t (** Two different constants that [because] requires equal. *)
  | Exceeds :
      { axis : 'a Axis.t
      ; lower : 'a
      ; upper : 'a
      ; because : Reason.t
      }
      -> t (** A constant above another that [because] requires below it. *)
[@@deriving sexp_of]

(** [Unequal] unless the points are equal. *)
val equal_on : 'a Axis.t -> 'a -> 'a -> Reason.t -> (unit, t) Result.t

(** [Exceeds] unless [lower <= upper]. *)
val le_on : 'a Axis.t -> lower:'a -> upper:'a -> Reason.t -> (unit, t) Result.t

(** Raises on [Error]: for constraints that cannot fail. *)
val ok_exn : (unit, t) Result.t -> unit
