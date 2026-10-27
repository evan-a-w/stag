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

let exceeded t =
  match t.limits, t.deadline with
  | { fuel = Some fuel; _ }, _ when t.steps >= fuel ->
    Some (sprintf "Out of fuel: evaluation took more than %d steps" fuel)
  | { max_depth = Some max_depth; _ }, _ when t.depth >= max_depth ->
    Some
      (sprintf
         "Stack depth exceeded: evaluation nested more than %d steps deep"
         max_depth)
  | { time_limit = Some time_limit; _ }, Some deadline
    when t.steps % steps_per_clock_check = 0
         && Time_ns.( > ) (Time_ns.now ()) deadline ->
    Some
      (sprintf
         "Time limit exceeded: evaluation took more than %s"
         (Time_ns.Span.to_string_hum time_limit))
  | _ -> None
;;

let step t ~loc ~f =
  match exceeded t with
  | Some message -> Error (Runtime_error.to_error { loc; message })
  | None ->
    t.steps <- t.steps + 1;
    t.depth <- t.depth + 1;
    let result = f () in
    t.depth <- t.depth - 1;
    result
;;
