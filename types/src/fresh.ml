open! Import

module Cons = struct
  type t =
    { value_parameters : Value_layout.parameters
    ; variances : Variance.of_con
    }

  let conservative =
    { value_parameters = (fun _ -> None); variances = (fun _ -> None) }
  ;;
end

type t =
  { mutable next_id : int
  ; mutable cons : Cons.t option
  ; modes : Mode.Id_source.t
  ; mutable settling :
      (unit -> (unit, Unification_error.t) Result.t) Queue.t option
  }

let make cons =
  { next_id = 0; cons; modes = Mode.Id_source.create (); settling = None }
;;

let create () = make None
let conservative () = make (Some Cons.conservative)
let modes t = t.modes
let set_cons t cons = t.cons <- Some cons

let cons t =
  match t.cons with
  | Some cons -> cons
  | None ->
    raise_s
      [%message
        "Fresh.cons: no type env was created with it (without one, make it \
         with [Fresh.conservative])"]
;;

let next_id t = t.next_id

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

let id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

let var t ~rank = Type.Var.create (id t) (Unbound rank)

let mark_in_record row =
  Option.iter (Type.tail_var row) ~f:(fun (var : _ Type.Var.t) ->
    Type.Var.mark_in_record var)
;;

let record t row =
  mark_in_record row;
  Type.TRecord { id = id t; row }
;;

let variant t row =
  mark_in_record row;
  Type.TVariant { id = id t; row }
;;
