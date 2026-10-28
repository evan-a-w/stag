open! Import

let root = "prelude"
let prefix = root ^ "."
let canonical path = prefix ^ path
let relative name = String.chop_prefix name ~prefix

let shadowed relative ~has =
  match String.split relative ~on:'.' with
  | [ name ] -> has `Type name
  | first :: _ :: _ -> has `Module first
  | [] -> false
;;

let to_string_hum ?(shadowed = fun _ -> false) name =
  match relative name with
  | Some relative when not (shadowed relative) -> relative
  | Some _ | None -> name
;;
