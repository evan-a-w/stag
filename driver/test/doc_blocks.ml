open! Import

module Kind = struct
  type t =
    | Program
    | Error
    | Project
  [@@deriving sexp_of]
end

module Block = struct
  type t =
    { kind : Kind.t
    ; modes : bool
    ; line : int
    ; code : string
    }
  [@@deriving sexp_of]
end

let kind_of_info info ~line =
  match
    String.split info ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [ "stag" ] -> Some (Kind.Program, false)
  | [ "stag"; "modes" ] -> Some (Program, true)
  | [ "stag-error" ] -> Some (Error, false)
  | [ "stag-error"; "modes" ] -> Some (Error, true)
  | [ "stag-project" ] -> Some (Project, false)
  | first :: _ when String.is_prefix first ~prefix:"stag" ->
    raise_s [%message "unknown code block" (info : string) (line : int)]
  | _ -> None
;;

let parse document =
  let lines = String.split_lines document |> Array.of_list in
  let rec closing i =
    if i >= Array.length lines
    then raise_s [%message "unclosed code block"]
    else if String.equal (String.strip lines.(i)) "```"
    then i
    else closing (i + 1)
  in
  let rec blocks i acc =
    if i >= Array.length lines
    then List.rev acc
    else (
      match String.chop_prefix lines.(i) ~prefix:"```" with
      | None -> blocks (i + 1) acc
      | Some info ->
        let close = closing (i + 1) in
        let acc =
          match kind_of_info info ~line:(i + 1) with
          | None -> acc
          | Some (kind, modes) ->
            let body =
              Array.sub lines ~pos:(i + 1) ~len:(close - i - 1)
              |> Array.to_list
              |> String.concat ~sep:"\n"
            in
            let code = String.make (i + 1) '\n' ^ body ^ "\n" in
            { Block.kind; modes; line = i + 1; code } :: acc
        in
        blocks (close + 1) acc)
  in
  blocks 0 []
;;

let project_files code =
  let rec files lines acc =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
      (match String.chop_prefix line ~prefix:"-- " with
       | None -> files rest acc
       | Some path ->
         let body, rest =
           List.split_while rest ~f:(fun line ->
             not (String.is_prefix line ~prefix:"-- "))
         in
         files rest ((path, String.concat ~sep:"\n" body ^ "\n") :: acc))
  in
  files (String.split_lines code) []
;;

module Claim = struct
  type t =
    | Item of string
    | Value of
        { name : string
        ; value : string
        }
    | Prints of string
    | Error of string
    | Warning of string
  [@@deriving sexp_of]
end

let normalize s =
  String.split_on_chars s ~on:[ ' '; '\n'; '\t'; '\r' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

(* The bodies of the outermost comments, nested ones included in them. *)
let comments code =
  let n = String.length code in
  let rec scan i depth start acc =
    if i + 1 >= n
    then List.rev acc
    else (
      match code.[i], code.[i + 1] with
      | '(', '*' ->
        scan (i + 2) (depth + 1) (if depth = 0 then i + 2 else start) acc
      | '*', ')' when depth > 0 ->
        let acc =
          if depth = 1
          then String.sub code ~pos:start ~len:(i - start) :: acc
          else acc
        in
        scan (i + 2) (depth - 1) start acc
      | _ -> scan (i + 1) depth start acc)
  in
  scan 0 0 0 []
;;

let claim comment : Claim.t list =
  let comment = normalize comment in
  let after prefix = String.chop_prefix comment ~prefix in
  match after "val " with
  | Some rest ->
    let name = String.take_while rest ~f:(Fn.non Char.is_whitespace) in
    (match String.substr_index comment ~pattern:" = " with
     | None -> [ Item comment ]
     | Some i ->
       [ Item (String.prefix comment i)
       ; Value { name; value = String.drop_prefix comment (i + 3) }
       ])
  | None ->
    if String.is_prefix comment ~prefix:"type "
       || String.is_prefix comment ~prefix:"exception "
    then [ Item comment ]
    else (
      match after "prints:", after "Error:", after "Warning:" with
      | Some s, _, _ -> [ Prints (String.strip s) ]
      | None, Some s, _ -> [ Error (String.strip s) ]
      | None, None, Some s -> [ Warning (String.strip s) ]
      | None, None, None -> [])
;;

let claims code = List.concat_map (comments code) ~f:claim

let signature_items signature =
  let starts_item line =
    List.exists [ "val "; "type "; "exception "; "module "; "end" ] ~f:(fun p ->
      String.is_prefix (String.lstrip line) ~prefix:p)
  in
  List.fold (String.split_lines signature) ~init:[] ~f:(fun acc line ->
    match acc with
    | last :: rest when not (starts_item line) -> (last ^ " " ^ line) :: rest
    | _ -> line :: acc)
  |> List.rev_map ~f:normalize
;;
