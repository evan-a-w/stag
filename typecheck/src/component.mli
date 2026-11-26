open! Import

(** The component of toplevels some code is part of, found as it is checked (see
    [Scc_frame]): its errors are dropped if it is abandoned, and once finished,
    whether it failed is that of its toplevels. Code a frame absorbs while
    unfinished is [Merged] into its component; code finished first, of another
    component, never is. *)
type t

val create : unit -> t
val failed : t -> bool
val fail : t -> unit
val is_abandoned : t -> bool
val is_open : t -> bool
val merge : t -> into:t -> unit
val abandon : t -> unit
val finish : t -> unit
