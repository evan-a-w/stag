open! Import

type t =
  { mutable next_id : int
  ; mutable value_parameters : Value_layout.parameters
  ; mutable variances : Variance.of_con
  ; modes : Mode.Id_source.t
  ; mutable settling : (unit -> (unit, Mode.Error.t) Result.t) Queue.t option
  ; mutable unreported : Unification_error.t option
  }

let create () =
  { next_id = 0
  ; value_parameters = (fun _ -> None)
  ; variances = (fun _ -> None)
  ; modes = Mode.Id_source.create ()
  ; settling = None
  ; unreported = None
  }
;;

let modes t = t.modes
let set_value_parameters t f = t.value_parameters <- f
let value_parameters t = t.value_parameters
let set_variances t f = t.variances <- f
let variances t = t.variances

let settle t f =
  match t.settling with
  | Some queue ->
    Queue.enqueue queue f;
    Ok ()
  | None ->
    let queue = Queue.create () in
    Queue.enqueue queue f;
    t.settling <- Some queue;
    let rec drain first_error =
      match Queue.dequeue queue with
      | None -> first_error
      | Some f ->
        drain
          (match first_error, f () with
           | (Error _ as error), _ | Ok (), error -> error)
    in
    Exn.protect
      ~f:(fun () -> drain (Ok ()))
      ~finally:(fun () -> t.settling <- None)
;;

let fail_unreported t error =
  if Option.is_none t.unreported then t.unreported <- Some error
;;

let take_unreported t =
  let error = t.unreported in
  t.unreported <- None;
  error
;;

let id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

let var t ~rank = Type.Var.create (id t) (Unbound rank)

let mark_in_record row =
  Option.iter (Type.tail_var row) ~f:(fun (var : _ Type.Var.t) ->
    var.in_record <- true)
;;

let record t row =
  mark_in_record row;
  Type.TRecord { id = id t; row }
;;

let variant t row =
  mark_in_record row;
  Type.TVariant { id = id t; row }
;;
