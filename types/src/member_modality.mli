open! Import

(** The modality of a record field or a constructor's argument, written
    [t @@ m]: what is read from it is at most the meet of the container's mode
    and [m], and what it is built of must be at most [m]. *)
type t =
  | Id
  | Meet of Mode.Const.t (** Of the atoms written: the others are the top. *)
  | Param of int
  (** [t @@ 'p], of the type's [index]th parameter, a mode: on stage and
      provenance. *)
[@@deriving sexp_of, equal]

(** The constant of atoms, the axes not written being the top, but a [const] one
    is [static]. *)
val of_atoms : Mode.Const.t -> t

(** [m] as written after [@@], [None] for [Id]; [param] renders a [Param]. *)
val to_string : t -> param:(int -> string) -> string option
