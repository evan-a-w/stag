open! Import

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
    ; generated : bool [@sexp.bool]
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
    |> List.map ~f:Source_edit.leading_whitespace
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
  ; generated = test.generated
  }
;;

let run ?(limits = Limits.unlimited) ?files driver =
  (Driver.checked driver).program.expect_tests
  |> List.filter ~f:(fun (test : Typed.Expect_test.t) ->
    Option.for_all files ~f:(fun files -> Set.mem files test.name.loc.file))
  |> List.map ~f:(run_test ~limits driver)
;;

(* The indentation of what starts at [offset]: that of its line, deeper if
   [offset] is not the first thing on it. *)
let indentation contents ~offset =
  let indentation = Source_edit.indentation contents ~offset in
  if Source_edit.first_on_line contents ~offset
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

let mismatch_edits
      contents
      ({ loc; expected; actual; parenthesize } : Mismatch.t)
  =
  let start = loc.start.offset in
  match expected, parenthesize with
  | Some _, _ ->
    let indentation = Source_edit.indentation contents ~offset:start in
    let quoted = quoted actual ~indentation:(deeper indentation) in
    [ { Source_edit.start
      ; stop = loc.stop.offset
      ; replacement =
          (if loc.start.offset = loc.stop.offset then " " ^ quoted else quoted)
      }
    ]
  | None, None ->
    let indentation = Source_edit.indentation contents ~offset:start in
    [ { start
      ; stop = start
      ; replacement =
          sprintf
            ";\n%s[%%expect %s]"
            indentation
            (quoted actual ~indentation:(deeper indentation))
      }
    ]
  | None, Some body ->
    let indentation = indentation contents ~offset:body.start.offset in
    [ { start = body.start.offset; stop = body.start.offset; replacement = "(" }
    ; { start
      ; stop = start
      ; replacement =
          sprintf
            ");\n%s[%%expect %s]"
            indentation
            (quoted actual ~indentation:(deeper indentation))
      }
    ]
;;

let edits ~path ~contents results =
  List.concat_map results ~f:(fun (result : Test_result.t) ->
    if result.generated then [] else result.mismatches)
  |> List.filter ~f:(fun (mismatch : Mismatch.t) ->
    String.equal mismatch.loc.file path)
  |> List.concat_map ~f:(mismatch_edits contents)
;;
