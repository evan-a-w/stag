open! Import

type t = { mutable next_id : int }

let create () = { next_id = 0 }

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
