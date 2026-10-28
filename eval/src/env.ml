open! Import
open Or_error.Let_syntax

type 'value t = { values : 'value option ref Typed.Ident.Table.t }

let create () = { values = Typed.Ident.Table.create () }
let copy t = { values = Hashtbl.copy t.values }
let mem t ident = Hashtbl.mem t.values ident

let find_slot ?fallback t ident =
  match Hashtbl.find t.values ident with
  | Some _ as slot -> slot
  | None ->
    Option.bind fallback ~f:(fun fallback -> Hashtbl.find fallback.values ident)
;;

let lookup ?fallback t ~ident =
  match find_slot ?fallback t ident with
  | None -> Or_error.error_s [%message "Unbound ident" (ident : Typed.Ident.t)]
  | Some { contents = None } ->
    Or_error.error_s
      [%message
        "Used value before it was defined (eg. because of recursion on values)"
          (ident : Typed.Ident.t)]
  | Some { contents = Some value } -> Ok value
;;

let declare t ~ident = Hashtbl.set t.values ~key:ident ~data:(ref None)

let set t ~ident ~value =
  let slot = Hashtbl.find_or_add t.values ident ~default:(fun () -> ref None) in
  slot := Some value
;;

let of_captures ?fallback t ~captures =
  let env = create () in
  let%map () =
    Hash_set.to_list captures
    |> List.fold_result ~init:() ~f:(fun () ident ->
      match find_slot ?fallback t ident with
      | Some slot -> Ok (Hashtbl.set env.values ~key:ident ~data:slot)
      | None ->
        Or_error.error_s
          [%message "Captured ident not in scope" (ident : Typed.Ident.t)])
  in
  env
;;
