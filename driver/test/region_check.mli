open! Import

(** An interpreter that makes each allocation where the placement record
    ([Typed.Allocation]) says, and checks no allocation is used after its region
    ends: an oracle for the placements, independent of how they are computed.

    A region is a function's (from its call to its return, or to a tail call or
    [exclave_] in it), or a [region_]'s. An allocation placed in one is freed
    with it; reading it after (applying it, matching it, reading through it or
    storing into it) is a use after free, as is one left reachable from a
    toplevel once [entry] returns. A site that follows a side of an owner's type
    is made where [if_local] says when that side, in the instance running, is
    [local]: each instance has every side of its owner as local as the modes
    where it is used allow, given the instances it is used from (the greatest
    instance, so as many sites as can be are made in regions).

    It runs the run time subset of stag: no quotes, exceptions or externals.
    Applications of prims are calls into [Prim]. *)

module Outcome : sig
  type t =
    | Returned of
        { value : string
        ; on_heap : int
        ; in_regions : int
        }
    | Use_after_free of string
    | Unsupported of string
    | Rejected of string list
  [@@deriving sexp_of]
end

(** Checks [source] (after [Source_helpers.prelude]) and runs its toplevel
    [entry], a function of [unit]. [override] replaces the placements the
    typechecker gives, eg. by a broken placement, to see what goes wrong. *)
val run
  :  ?override:(Typed.Allocation.t -> Typed.Allocation.t)
  -> string
  -> entry:string
  -> Outcome.t
