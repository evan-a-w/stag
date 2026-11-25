open! Import

(** Which axes the values of a type cross: on those, a value may be used at any
    mode. On stage, crossing is to [portable] only, never to [const]. *)

module When : sig
  type t =
    | Never
    | Always
    | While_pointer_free of Graph.Layout.t
    (** For a type not yet known: until the layout has pointers, when what the
        crossing left out is added. *)
  [@@deriving sexp_of]
end

type t =
  { stage : When.t
  ; provenance : When.t
  ; locality : When.t
  }
[@@deriving sexp_of]

val none : t

(** Plain data such as [int]: crosses every axis. *)
val immediate : t

(** Of a type that crosses every axis if its layout stays pointer-free. *)
val while_pointer_free : Graph.Layout.t -> t
