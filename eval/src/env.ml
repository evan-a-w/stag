open! Import
open Or_error.Let_syntax

module Slot = struct
  type 'value t = { mutable value : 'value or_null }
end

type 'value t =
  { values : 'value Slot.t Typed.Ident.Table.t
  ; parent : 'value t or_null
  }

let create () = { values = Typed.Ident.Table.create ~size:4 (); parent = Null }
let extend t = { values = Typed.Ident.Table.create ~size:4 (); parent = This t }

(* Ident's hash and compare don't allocate. *)
let[@inline] find_own t ident =
  (Hashtbl.find_or_null [@zero_alloc assume]) t.values ident
;;

let rec find_slot_or_null t ident =
  match find_own t ident with
  | This _ as slot -> slot
  | Null ->
    (match t.parent with
     | Null -> Null
     | This parent -> find_slot_or_null parent ident)
;;

let mem t ident =
  match find_slot_or_null t ident with
  | This _ -> true
  | Null -> false
;;

let[@zero_alloc] find_slot t ~fallback ident =
  match find_slot_or_null t ident with
  | This _ as slot -> slot
  | Null -> find_own fallback ident
;;

let lookup ?fallback t ~ident =
  let slot =
    match fallback with
    | Some fallback -> find_slot t ~fallback ident
    | None -> find_slot_or_null t ident
  in
  match slot with
  | Null -> Or_error.error_s [%message "Unbound ident" (ident : Typed.Ident.t)]
  | This { value = Null } ->
    Or_error.error_s
      [%message
        "Used value before it was defined (eg. because of recursion on values)"
          (ident : Typed.Ident.t)]
  | This { value = This value } -> Ok value
;;

let declare t ~ident =
  Hashtbl.set t.values ~key:ident ~data:{ Slot.value = Null }
;;

let set t ~ident ~value =
  match find_slot_or_null t ident with
  | This slot -> slot.value <- This value
  | Null -> Hashtbl.set t.values ~key:ident ~data:{ Slot.value = This value }
;;

let of_captures t ~fallback ~captures =
  let env = create () in
  let%map () =
    Hash_set.to_list captures
    |> List.fold_result ~init:() ~f:(fun () ident ->
      match find_slot t ~fallback ident with
      | This slot -> Ok (Hashtbl.set env.values ~key:ident ~data:slot)
      | Null ->
        Or_error.error_s
          [%message "Captured ident not in scope" (ident : Typed.Ident.t)])
  in
  env
;;
