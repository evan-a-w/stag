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

let first_module relative =
  match String.split relative ~on:'.' with
  | first :: _ :: _ -> Some first
  | [ _ ] | [] -> None
;;

let to_string_hum
  ?(shadowed = fun _ -> false)
  ?(root_modules = String.Set.empty)
  name
  =
  match relative name with
  | Some relative when not (shadowed relative) -> relative
  | Some relative
    when not (Option.exists (first_module relative) ~f:(Set.mem root_modules))
    -> "." ^ relative
  | Some _ | None -> name
;;

let prim_prefix = "Prim."

let root_path name =
  match relative name with
  | None -> name
  | Some relative ->
    Option.value
      (String.chop_prefix relative ~prefix:prim_prefix)
      ~default:relative
;;

let short_name name =
  Option.value_map (String.rsplit2 name ~on:'.') ~default:name ~f:snd
;;
