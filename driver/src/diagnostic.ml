open! Import

module Note = struct
  type t =
    { loc : Loc.t
    ; message : string
    }
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t
  ; whole_file : bool
  ; message : string
  ; notes : Note.t list
  }
[@@deriving sexp_of]

let create ?(notes = []) ?(whole_file = false) ~loc message =
  { loc; whole_file; message; notes }
;;

let relocate t ~f =
  { t with
    loc = f t.loc
  ; notes =
      List.map t.notes ~f:(fun (note : Note.t) ->
        { note with loc = f note.loc })
  }
;;

let of_parse_error error =
  create
    ~loc:(Parser.Error.loc error)
    (String.capitalize (Parser.Error.message error))
;;

let of_source_file_error (error : Source_file.Error.t) =
  match error with
  | Parse error -> of_parse_error error
  | Invalid_file_name { message; path = _ } ->
    { (create ~loc:(Source_file.Error.loc error) message) with
      whole_file = true
    }
;;

let max_declaration_lines = 12

(* Indented, its first lines only if it is long. *)
let declaration_source ?root_modules toplevel =
  let lines =
    String.split_lines (Printer.toplevel ~width:78 ?root_modules toplevel)
  in
  let lines =
    if List.length lines <= max_declaration_lines
    then lines
    else (
      let shown = max_declaration_lines - 1 in
      List.take lines shown
      @ [ sprintf "... (%d more lines)" (List.length lines - shown) ])
  in
  List.map lines ~f:(fun line ->
    if String.is_empty line then line else "  " ^ line)
  |> String.concat ~sep:"\n"
;;

let of_typecheck_error (error : Typecheck_error.t) =
  let root_modules =
    Option.map error.in_scope ~f:(fun in_scope -> in_scope.root_modules)
  in
  let rec notes : Typecheck_error.Kind.t -> Note.t list = function
    | Splice_evaluation_failed { error; code = _ }
    | Declaration_splice_failed error ->
      (match Runtime_error.of_error error with
       | Some { loc; kind = _ } when not (Loc.is_none loc) ->
         [ { Note.loc; message = "The error was raised here" } ]
       | Some _ | None -> [])
    | Free_in_spliced_code { name; unusable } ->
      [ { Note.loc = unusable.loc
        ; message = sprintf "%s is a variable of the code's context here" name
        }
      ]
    | Declaration_splice_cycle { splice; name = _; module_ = _ } ->
      [ { Note.loc = splice; message = "The declaration splice is here" } ]
    | Generated_after_include { error; include_ } ->
      notes error
      @ [ { Note.loc = include_
          ; message =
              "This include generates it, but it is visible only after the \
               include"
          }
        ]
    | Maybe_generated_by_running_include { error; include_ } ->
      notes error
      @ [ { Note.loc = include_
          ; message =
              "This include, still being generated because its expression uses \
               this code, may define it: move this code before the include or \
               into another module"
          }
        ]
    | Shadowed_prelude_module { error; module_; shadowed_by } ->
      notes error
      @ [ { Note.loc = shadowed_by
          ; message = sprintf "The program's module %s is here" module_
          }
        ]
    | In_generated_declaration { generated; error; declaration = _ } ->
      notes error
      @ [ { Note.loc = Loc.none
          ; message =
              "The generated declaration is:\n"
              ^ declaration_source ?root_modules generated
          }
        ]
    | _ -> []
  in
  create
    ~notes:(notes error.kind)
    ~loc:error.loc
    (Typecheck_error.message error)
;;

let of_driver_error : Driver.Error.t -> t = function
  | Source error -> of_source_file_error error
  | Typecheck error -> of_typecheck_error error
;;

let of_runtime_error error =
  match Runtime_error.of_error error with
  | Some ({ loc; kind = _ } as error) ->
    create ~loc (Runtime_error.message error)
  | None -> create ~loc:Loc.none (Error.to_string_hum error)
;;

let indented output =
  if String.is_empty output
  then "  (no output)"
  else
    String.split_lines output
    |> List.map ~f:(fun line ->
      if String.is_empty line then line else "  " ^ line)
    |> String.concat ~sep:"\n"
;;

let of_test_result
      ({ name; error; output_before_error; different_outputs; _ } :
        Expect_test_runner.Test_result.t)
  =
  let in_test = sprintf "Expect test %S" name.txt in
  let error =
    Option.map error ~f:(fun error ->
      let diagnostic = of_runtime_error error in
      let output =
        if String.is_empty output_before_error
        then ""
        else
          "\nOutput since the last [%expect]:\n" ^ indented output_before_error
      in
      { diagnostic with
        message = sprintf "%s failed: %s%s" in_test diagnostic.message output
      })
  in
  List.map different_outputs ~f:(fun { loc; outputs } ->
    create
      ~loc
      (sprintf
         "%s: this [%%expect] was reached with different outputs:\n%s"
         in_test
         (List.mapi outputs ~f:(fun i output ->
            sprintf "Output %d:\n%s" (i + 1) (indented output))
          |> String.concat ~sep:"\n")))
  @ Option.to_list error
;;

let header (loc : Loc.t) ~whole_file =
  if whole_file
  then sprintf "File %S:" loc.file
  else if loc.start.line = loc.stop.line
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

(* Bytes continuing a UTF-8 character take no column. *)
let is_continuation_byte c = Char.to_int c land 0xC0 = 0x80

(* Blanks as wide as [prefix], keeping its tabs. *)
let padding prefix =
  String.filter_map prefix ~f:(fun c ->
    if Char.equal c '\t'
    then Some '\t'
    else if is_continuation_byte c
    then None
    else Some ' ')
;;

let width string = String.count string ~f:(Fn.non is_continuation_byte)

let quote (loc : Loc.t) contents =
  let lines = String.split contents ~on:'\n' |> Array.of_list in
  if loc.stop.line > Array.length lines
  then []
  else (
    let number_width = String.length (Int.to_string loc.stop.line) in
    let numbered n =
      sprintf "%*d | %s" number_width n (String.rstrip lines.(n - 1))
    in
    if loc.start.line = loc.stop.line
    then (
      let line = lines.(loc.start.line - 1) in
      let start = Int.min loc.start.col (String.length line) in
      let stop = Int.max start (Int.min loc.stop.col (String.length line)) in
      let carets =
        Int.max 1 (width (String.sub line ~pos:start ~len:(stop - start)))
      in
      [ numbered loc.start.line
      ; String.make (number_width + 3) ' '
        ^ padding (String.prefix line start)
        ^ String.make carets '^'
      ])
    else (
      let all = List.range loc.start.line loc.stop.line ~stop:`inclusive in
      if List.length all <= max_quoted_lines
      then List.map all ~f:numbered
      else
        List.map (List.take all 3) ~f:numbered
        @ [ String.make number_width ' ' ^ " | ..." ]
        @ List.map (List.drop all (List.length all - 2)) ~f:numbered))
;;

let located ~contents ~whole_file (loc : Loc.t) =
  if Loc.is_none loc
  then []
  else
    header loc ~whole_file
    ::
    (if whole_file
     then []
     else Option.value_map (contents loc.file) ~default:[] ~f:(quote loc))
;;

let to_string_hum
      ?(contents = fun _ -> None)
      ?(warning = false)
      { loc; whole_file; message; notes }
  =
  let contents file =
    match contents file with
    | Some contents -> Some contents
    | None -> Prelude.source file
  in
  (located ~contents ~whole_file loc
   @ [ (if warning then "Warning: " else "Error: ") ^ message ])
  @ List.concat_map notes ~f:(fun { loc; message } ->
    located ~contents ~whole_file:false loc @ [ "Note: " ^ message ])
  |> List.map ~f:(fun line -> line ^ "\n")
  |> String.concat
;;
