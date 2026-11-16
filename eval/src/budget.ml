open! Import

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

(* The clock doesn't allocate. *)
let[@zero_alloc assume] past_deadline t =
  match t.deadline with
  | Some deadline -> Time_ns.( > ) (Time_ns.now ()) deadline
  | None -> false
;;

let[@inline] out_of_fuel t =
  match t.limits.fuel with
  | Some fuel -> Int_prim.(t.steps >= fuel)
  | None -> false
;;

let[@inline] too_deep t =
  match t.limits.max_depth with
  | Some max_depth -> Int_prim.(t.depth >= max_depth)
  | None -> false
;;

let[@inline] out_of_time t =
  Int_prim.(t.steps land (steps_per_clock_check - 1) = 0) && past_deadline t
;;

let[@cold] [@zero_alloc assume error] exceeded t ~loc =
  let message =
    if out_of_fuel t
    then
      sprintf
        "Out of fuel: evaluation took more than %d steps"
        (Option.value_exn t.limits.fuel)
    else if too_deep t
    then
      sprintf
        "Stack depth exceeded: evaluation nested more than %d steps deep"
        (Option.value_exn t.limits.max_depth)
    else
      sprintf
        "Time limit exceeded: evaluation took more than %s"
        (Time_ns.Span.to_string_hum (Option.value_exn t.limits.time_limit))
  in
  Error (Runtime_error.to_error (Runtime_error.create ~loc message))
;;

let[@zero_alloc] tail_step t ~loc =
  if out_of_fuel t || too_deep t || out_of_time t
  then exceeded t ~loc
  else (
    t.steps <- t.steps + 1;
    Ok ())
;;

let step t ~loc ~f =
  match tail_step t ~loc with
  | Error _ as error -> error
  | Ok () ->
    t.depth <- t.depth + 1;
    let result = f () in
    t.depth <- t.depth - 1;
    result
;;
