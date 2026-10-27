open! Core

module Mismatch = struct
  type t =
    { loc : Loc.t
    ; expected : string option
    ; actual : string
    }
  [@@deriving sexp_of]
end

module Test_result = struct
  type t =
    { name : string Located.t
    ; mismatches : Mismatch.t list
    ; error : Error.t option
    }
  [@@deriving sexp_of]

  let passed t = List.is_empty t.mismatches && Option.is_none t.error
end

let normalize string =
  let lines =
    String.split_lines string
    |> List.map ~f:String.rstrip
    |> List.drop_while ~f:String.is_empty
    |> List.rev
    |> List.drop_while ~f:String.is_empty
    |> List.rev
  in
  let indentation line =
    String.length line - String.length (String.lstrip line)
  in
  let common =
    List.filter lines ~f:(Fn.non String.is_empty)
    |> List.map ~f:indentation
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  List.map lines ~f:(fun line ->
    if String.is_empty line then line else String.drop_prefix line common)
  |> String.concat ~sep:"\n"
;;

let run_test ~limits driver (test : Typed.Expect_test.t) =
  let interpreter = Driver.interpreter driver in
  let buffer = Buffer.create 64 in
  let take_output () =
    let output = normalize (Buffer.contents buffer) in
    Buffer.clear buffer;
    output
  in
  let mismatches = Queue.create () in
  let check ~loc ~expected =
    let actual = take_output () in
    let matches =
      String.equal actual (normalize (Option.value expected ~default:""))
    in
    if not matches
    then Queue.enqueue mismatches { Mismatch.loc; expected; actual }
  in
  let output = Treewalk.output interpreter in
  Treewalk.set_output interpreter (Buffer.add_string buffer);
  Treewalk.set_limits interpreter limits;
  let result =
    Exn.protect
      ~f:(fun () ->
        Treewalk.run_expect_test
          interpreter
          test
          ~on_expect:(fun { txt; loc } -> check ~loc ~expected:(Some txt)))
      ~finally:(fun () -> Treewalk.set_output interpreter output)
  in
  (match result with
   | Ok () ->
     let stop = test.body.loc.stop in
     check ~loc:{ test.body.loc with start = stop } ~expected:None
   | Error _ -> ());
  { Test_result.name = test.name
  ; mismatches = Queue.to_list mismatches
  ; error = Result.error result
  }
;;

let run ?(limits = Limits.unlimited) driver =
  (Driver.checked driver).program.expect_tests
  |> List.map ~f:(run_test ~limits driver)
;;

let line_indentation contents ~offset =
  let line_start =
    match String.rindex_from contents (offset - 1) '\n' with
    | Some i -> i + 1
    | None | (exception _) -> 0
  in
  let rec count i =
    if i < String.length contents && Char.equal contents.[i] ' '
    then count (i + 1)
    else i - line_start
  in
  count line_start
;;

let quoted actual ~indent =
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
      let pad = String.make indent ' ' in
      "\n"
      ^ String.concat
          (List.map lines ~f:(fun line ->
             (if String.is_empty line then "" else pad ^ line) ^ "\n"))
      ^ pad
  in
  sprintf "{%s|%s|%s}" id body id
;;

let corrected ~path ~contents results =
  let edits =
    List.concat_map results ~f:(fun (result : Test_result.t) ->
      result.mismatches)
    |> List.filter ~f:(fun (mismatch : Mismatch.t) ->
      String.equal mismatch.loc.file path)
    |> List.map ~f:(fun ({ loc; expected; actual } : Mismatch.t) ->
      let start = loc.start.offset in
      let indent = line_indentation contents ~offset:start in
      let replacement =
        match expected with
        | Some _ ->
          let quoted = quoted actual ~indent:(indent + 2) in
          if loc.start.offset = loc.stop.offset then " " ^ quoted else quoted
        | None ->
          sprintf
            ";\n%s[%%expect %s]"
            (String.make indent ' ')
            (quoted actual ~indent:(indent + 2))
      in
      start, loc.stop.offset, replacement)
    |> List.stable_sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
    |> List.remove_consecutive_duplicates
         ~which_to_keep:`First
         ~equal:(fun (a, _, _) (b, _, _) -> a = b)
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
