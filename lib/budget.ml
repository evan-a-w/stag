open! Core

type t =
  { limits : Limits.t
  ; deadline : Time_ns.t option
  ; mutable steps : int
  ; mutable depth : int
  }

let create (limits : Limits.t) =
  { limits
  ; deadline = Option.map limits.time_limit ~f:(Time_ns.add (Time_ns.now ()))
  ; steps = 0
  ; depth = 0
  }
;;

let steps_per_clock_check = 4096

let enter t =
  match t.limits, t.deadline with
  | { fuel = Some fuel; _ }, _ when t.steps >= fuel ->
    Or_error.error_s [%message "Out of fuel" (fuel : int)]
  | { max_depth = Some max_depth; _ }, _ when t.depth >= max_depth ->
    Or_error.error_s [%message "Stack depth exceeded" (max_depth : int)]
  | { time_limit = Some time_limit; _ }, Some deadline
    when t.steps % steps_per_clock_check = 0
         && Time_ns.( > ) (Time_ns.now ()) deadline ->
    Or_error.error_s
      [%message "Time limit exceeded" (time_limit : Time_ns.Span.t)]
  | _ ->
    t.steps <- t.steps + 1;
    t.depth <- t.depth + 1;
    Ok ()
;;

let step t ~f =
  let%bind.Or_error () = enter t in
  let result = f () in
  t.depth <- t.depth - 1;
  result
;;
