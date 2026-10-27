open! Core

type t =
  { loc : Loc.t
  ; message : string
  }
[@@deriving sexp_of]

let of_parse_error error =
  { loc = Parser.Error.loc error
  ; message = String.capitalize (Parser.Error.message error)
  }
;;

let of_source_file_error (error : Source_file.Error.t) =
  match error with
  | Parse error -> of_parse_error error
  | Invalid_file_name { message; path = _ } ->
    { loc = Source_file.Error.loc error; message = String.capitalize message }
;;

let of_typecheck_error (error : Typecheck_error.t) =
  { loc = error.loc; message = Typecheck_error.message error }
;;

let of_driver_error : Driver.Error.t -> t = function
  | Source error -> of_source_file_error error
  | Typecheck error -> of_typecheck_error error
;;

let of_runtime_error error =
  match Runtime_error.of_error error with
  | Some { loc; message } -> { loc; message }
  | None -> { loc = Loc.none; message = Error.to_string_hum error }
;;

let header (loc : Loc.t) =
  if loc.start.line = loc.stop.line
  then
    sprintf
      "File %S, line %d, characters %d-%d:"
      loc.file
      loc.start.line
      loc.start.col
      loc.stop.col
  else
    sprintf
      "File %S, lines %d-%d, characters %d-%d:"
      loc.file
      loc.start.line
      loc.stop.line
      loc.start.col
      loc.stop.col
;;

let max_quoted_lines = 6

let quote (loc : Loc.t) contents =
  let lines = String.split contents ~on:'\n' |> Array.of_list in
  if loc.stop.line > Array.length lines
  then []
  else (
    let width = String.length (Int.to_string loc.stop.line) in
    let numbered n = sprintf "%*d | %s" width n (String.rstrip lines.(n - 1)) in
    if loc.start.line = loc.stop.line
    then (
      let line = lines.(loc.start.line - 1) in
      let stop = Int.min loc.stop.col (String.length line) in
      let carets = Int.max 1 (stop - loc.start.col) in
      [ numbered loc.start.line
      ; String.make (width + 3 + loc.start.col) ' ' ^ String.make carets '^'
      ])
    else (
      let all = List.range loc.start.line loc.stop.line ~stop:`inclusive in
      if List.length all <= max_quoted_lines
      then List.map all ~f:numbered
      else
        List.map (List.take all 3) ~f:numbered
        @ [ String.make width ' ' ^ " | ..." ]
        @ List.map (List.drop all (List.length all - 2)) ~f:numbered))
;;

let to_string_hum ?(contents = fun _ -> None) { loc; message } =
  let located =
    if Loc.is_none loc
    then []
    else
      header loc
      :: Option.value_map (contents loc.file) ~default:[] ~f:(quote loc)
  in
  String.concat ~sep:"\n" (located @ [ "Error: " ^ message ]) ^ "\n"
;;
