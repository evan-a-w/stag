open! Import

type t =
  { iterations : int
  ; time : Time_ns.Span.t
  ; alloc : Alloc.t option
  }
[@@deriving sexp]

let per_iteration_ns t = Time_ns.Span.to_ns t.time /. Float.of_int t.iterations
