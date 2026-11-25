open! Import

(** How a part relates to the block it is read from, as the steps from the block
    to the part, outermost first. Of values, a step is a field's or a
    constructor argument's modality: what is read is at most the meet of the
    block's mode and it. Through storage, a step is a pointer's cell: what is
    read is exactly as its [ptr[t @@ m]] says, whatever the pointer's mode, as
    the cell's aliases may store anything that fits it. *)
module Step : sig
  type t =
    | Meet of Mode.Const.t (** A [@@ atoms] modality. *)
    | Param of Mode_arg.t
    (** A [@@ 'p] modality, of the type's argument: on stage and provenance. *)
    | Cell of Mode_arg.t
    (** A pointer's cell, of what its type says it holds: on stage and
        provenance, as a cell's locality is [global]. *)
  [@@deriving sexp_of]
end

type t = Step.t list [@@deriving sexp_of]

val id : t
val of_step : Step.t -> t

(** Of the cell of a [ptr[_ @@ held]]. *)
val of_cell : Mode_arg.t -> t

(** [compose outer inner]: of a part [inner] of a part [outer] of a block. *)
val compose : t -> t -> t

(** Whether a part's locality is [global] whatever the block's. *)
val is_global : t -> bool

(** The steps of a store into the part: what is stored must fit them all, from
    the last cell on, as earlier cells hold the pointer to it, not it. *)
val from_last_cell : t -> t
