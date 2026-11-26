open! Import

(** Toplevels are checked on demand, so strongly connected components are found
    with Tarjan's algorithm as we go. A frame is of the code of a toplevel,
    whose component [finish_scc] finishes once complete, or of other code, eg.
    run at compile time, maybe [within] the checking of a toplevel. [pending]
    are the visited toplevels whose component is not yet complete. [component]
    is that of the frame's code. *)
module Kind : sig
  type 'frame t =
    | Toplevel
    | Code of { within : 'frame option }
end

type t = private
  { id : int
  ; kind : t Kind.t
  ; component : Component.t
  ; mutable lowlink : int
  ; mutable pending : Toplevel_binding.t list
  ; mutable quoting : Quoted_use.t list
  (** The uses a toplevel's code made of toplevels being checked, latest first. *)
  ; mutable reopened : Local_uses.Generalized.t list
  (** The locals of a toplevel's code whose schemes were reopened (see
      [Uses.code_checked]), generalized with its component, if it is. *)
  }

val create : int -> t Kind.t -> t
val add_pending : t -> Toplevel_binding.t -> unit
val add_quoting : t -> Quoted_use.t -> unit
val add_reopened : t -> Local_uses.Generalized.t -> unit

(** Its component is finished or abandoned. *)
val clear : t -> unit

(** The rank a toplevel's code is checked at. *)
val toplevel_rank : int

(** The rank of the types of the uses its code makes of toplevels being checked:
    a toplevel's are generalized with its component, if it is. Other code's are
    left to [settle]. *)
val use_rank : t -> int

val depends_on : t -> lowlink:int -> unit

(** What [child], of code [t] checked a toplevel for, left unfinished is part of
    [t]'s component. A finished [child] is of a component of its own. *)
val absorb : t -> child:t -> unit

(** The code of [t] other than a toplevel's is checked and run: it is part of
    the component of the toplevel it was within, if any. Code within none is
    checked only where no toplevel is being checked, so leaves none unfinished. *)
val hand_over : t -> unit
