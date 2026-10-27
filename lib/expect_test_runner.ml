open! Core

module Mismatch = struct
  type t =
    { loc : Loc.t
    ; expected : string option
    ; actual : string
    ; parenthesize : Loc.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Different_outputs = struct
  type t =
    { loc : Loc.t
    ; outputs : string list
    }
  [@@deriving sexp_of]
end

module Test_result = struct
  type t =
    { name : string Located.t
    ; mismatches : Mismatch.t list
    ; different_outputs : Different_outputs.t list [@sexp.list]
    ; error : Error.t option
    ; output_before_error : string [@sexp_drop_if String.is_empty]
    }
  [@@deriving sexp_of]

  let passed t =
    List.is_empty t.mismatches
    && List.is_empty t.different_outputs
    && Option.is_none t.error
  ;;
end

(* An [[%expect]] reached, with the outputs it was reached with, latest
   first. *)
module Reached = struct
  type t =
    { loc : Loc.t
    ; expected : string
    ; mutable outputs : string list
    }
end

let leading_whitespace line =
  String.prefix
    line
    (String.lfindi line ~f:(fun _ c -> not (Char.is_whitespace c))
     |> Option.value ~default:(String.length line))
;;

let common_prefix a b =
  let rec go i =
    if i < String.length a && i < String.length b && Char.equal a.[i] b.[i]
    then go (i + 1)
    else i
  in
  String.prefix a (go 0)
;;

let normalize string =
  let lines =
    String.split_lines string
    |> List.map ~f:String.rstrip
    |> List.drop_while ~f:String.is_empty
    |> List.rev
    |> List.drop_while ~f:String.is_empty
    |> List.rev
  in
  let common =
    List.filter lines ~f:(Fn.non String.is_empty)
    |> List.map ~f:leading_whitespace
    |> List.reduce ~f:common_prefix
    |> Option.value ~default:""
  in
  List.map lines ~f:(fun line ->
    if String.is_empty line
    then line
    else String.drop_prefix line (String.length common))
  |> String.concat ~sep:"\n"
;;

let rec ends_in_match (expr : Typed.Expr.t) =
  match expr.desc with
  | Match _ -> true
  | Let { in_; _ } -> ends_in_match in_
  | _ -> false
;;

let run_test ~limits driver (test : Typed.Expect_test.t) =
  let interpreter = Driver.interpreter driver in
  let buffer = Buffer.create 64 in
  let take_output () =
    let output = normalize (Buffer.contents buffer) in
    Buffer.clear buffer;
    output
  in
  let in_order = Queue.create () in
  let by_loc = String.Table.create () in
  let on_expect ({ txt; loc } : string Located.t) =
    let output = take_output () in
    let reached =
      Hashtbl.find_or_add by_loc (Loc.to_string loc) ~default:(fun () ->
        let reached = { Reached.loc; expected = txt; outputs = [] } in
        Queue.enqueue in_order reached;
        reached)
    in
    reached.outputs <- output :: reached.outputs
  in
  let output = Treewalk.output interpreter in
  Treewalk.set_output interpreter (Buffer.add_string buffer);
  Treewalk.set_limits interpreter limits;
  let result =
    Exn.protect
      ~f:(fun () -> Treewalk.run_expect_test interpreter test ~on_expect)
      ~finally:(fun () -> Treewalk.set_output interpreter output)
  in
  let mismatches = Queue.create () in
  let different_outputs = Queue.create () in
  Queue.iter in_order ~f:(fun { loc; expected; outputs } ->
    match List.rev outputs |> List.stable_dedup ~compare:String.compare with
    | [ actual ] ->
      if not (String.equal actual (normalize expected))
      then
        Queue.enqueue
          mismatches
          { Mismatch.loc
          ; expected = Some expected
          ; actual
          ; parenthesize = None
          }
    | outputs ->
      Queue.enqueue different_outputs { Different_outputs.loc; outputs });
  let output_before_error =
    match result with
    | Ok () ->
      let actual = take_output () in
      if not (String.is_empty actual)
      then (
        let stop = test.body.loc.stop in
        Queue.enqueue
          mismatches
          { Mismatch.loc = { test.body.loc with start = stop }
          ; expected = None
          ; actual
          ; parenthesize =
              Option.some_if (ends_in_match test.body) test.body.loc
          });
      ""
    | Error _ -> take_output ()
  in
  { Test_result.name = test.name
  ; mismatches = Queue.to_list mismatches
  ; different_outputs = Queue.to_list different_outputs
  ; error = Result.error result
  ; output_before_error
  }
;;

let run ?(limits = Limits.unlimited) driver =
  (Driver.checked driver).program.expect_tests
  |> List.map ~f:(run_test ~limits driver)
;;

let line_start contents ~offset =
  match String.rindex_from contents (offset - 1) '\n' with
  | Some i -> i + 1
  | None | (exception _) -> 0
;;

(* The indentation of what starts at [offset]: that of its line, deeper if
   [offset] is not the first thing on it. *)
let indentation contents ~offset =
  let start = line_start contents ~offset in
  let before = String.sub contents ~pos:start ~len:(offset - start) in
  let indentation = leading_whitespace before in
  if String.length indentation = String.length before
  then indentation
  else indentation ^ "  "
;;

let deeper indentation =
  if String.is_suffix indentation ~suffix:"\t"
  then indentation ^ "\t"
  else indentation ^ "  "
;;

let quoted actual ~indentation =
  let id =
    Sequence.unfold ~init:0 ~f:(fun n ->
      Some ((if n = 0 then "" else String.make n 'x'), n + 1))
    |> Sequence.find_exn ~f:(fun id ->
      not (String.is_substring actual ~substring:("|" ^ id ^ "}")))
  in
  let body =
    match String.split_lines actual with
    | [] -> " "
    | [ line ] -> sprintf " %s " line
    | lines ->
      "\n"
      ^ String.concat
          (List.map lines ~f:(fun line ->
             (if String.is_empty line then "" else indentation ^ line) ^ "\n"))
      ^ indentation
  in
  sprintf "{%s|%s|%s}" id body id
;;

let edits contents ({ loc; expected; actual; parenthesize } : Mismatch.t) =
  let start = loc.start.offset in
  match expected, parenthesize with
  | Some _, _ ->
    let line = line_start contents ~offset:start in
    let indentation =
      leading_whitespace (String.sub contents ~pos:line ~len:(start - line))
    in
    let quoted = quoted actual ~indentation:(deeper indentation) in
    [ ( start
      , loc.stop.offset
      , if loc.start.offset = loc.stop.offset then " " ^ quoted else quoted )
    ]
  | None, None ->
    let line = line_start contents ~offset:start in
    let indentation =
      leading_whitespace (String.sub contents ~pos:line ~len:(start - line))
    in
    [ ( start
      , start
      , sprintf
          ";\n%s[%%expect %s]"
          indentation
          (quoted actual ~indentation:(deeper indentation)) )
    ]
  | None, Some body ->
    let indentation = indentation contents ~offset:body.start.offset in
    [ body.start.offset, body.start.offset, "("
    ; ( start
      , start
      , sprintf
          ");\n%s[%%expect %s]"
          indentation
          (quoted actual ~indentation:(deeper indentation)) )
    ]
;;

let corrected ~path ~contents results =
  let edits =
    List.concat_map results ~f:(fun (result : Test_result.t) ->
      result.mismatches)
    |> List.filter ~f:(fun (mismatch : Mismatch.t) ->
      String.equal mismatch.loc.file path)
    |> List.concat_map ~f:(edits contents)
    |> List.stable_sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
  in
  if List.is_empty edits
  then None
  else (
    let buffer = Buffer.create (String.length contents) in
    let last =
      List.fold edits ~init:0 ~f:(fun pos (start, stop, replacement) ->
        Buffer.add_substring buffer contents ~pos ~len:(start - pos);
        Buffer.add_string buffer replacement;
        stop)
    in
    Buffer.add_substring
      buffer
      contents
      ~pos:last
      ~len:(String.length contents - last);
    Some (Buffer.contents buffer))
;;
