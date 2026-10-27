open! Core

type t =
  { fuel : int option
  ; max_depth : int option
  ; time_limit : Time_ns.Span.t option
  }

let unlimited = { fuel = None; max_depth = None; time_limit = None }
