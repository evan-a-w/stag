open! Import
open Source_helpers

let signature source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    Ok
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    Error (List.map errors ~f:Typecheck_error.message)
;;

let round_trip source ~show =
  match signature source with
  | Error errors -> List.iter errors ~f:print_endline
  | Ok printed ->
    let annotated =
      String.split_lines printed
      |> List.filter_map ~f:(fun line ->
        String.chop_prefix line ~prefix:"val "
        |> Option.map ~f:(fun rest ->
          let name, ty = String.lsplit2_exn rest ~on:':' in
          String.strip name, ty))
    in
    let copies =
      List.map annotated ~f:(fun (name, ty) ->
        sprintf "let %s_copy :%s = %s" name ty name)
    in
    (* A function's annotation alone: [fun] adds nothing to its arrow's modes
       but what every function has. Anything else is unannotated by
       [round_trip_diverge ()]'s own. *)
    let is_arrow ty =
      let ty =
        match String.substr_index ty ~pattern:" with " with
        | Some i -> String.prefix ty i
        | None -> ty
      in
      match Stag_syntax.Parser.type_expr ~file:"t.stag" ty with
      | Ok { txt = Arrow _; _ } -> true
      | Ok _ -> false
      | Error error -> raise_s [%sexp (error : Stag_syntax.Parser.Error.t)]
    in
    let alone =
      List.map annotated ~f:(fun (name, ty) ->
        sprintf
          "let %s_alone :%s = %s"
          name
          ty
          (if is_arrow ty
           then "fun v -> round_trip_diverge ()"
           else "round_trip_diverge ()"))
    in
    (match show with
     | `Signature -> print_string printed
     | `Copies -> print_endline (String.concat ~sep:"\n" copies));
    let round_trip_diverge =
      "let round_trip_diverge (u : unit) = round_trip_diverge u"
    in
    (match
       signature
         (String.concat
            ~sep:"\n"
            ((source :: round_trip_diverge :: copies) @ alone))
     with
     | Error errors -> List.iter errors ~f:print_endline
     | Ok signature' ->
       let lines = String.split_lines signature' in
       let find name =
         List.find_map lines ~f:(fun line ->
           String.chop_prefix line ~prefix:("val " ^ name ^ " :"))
       in
       List.iter annotated ~f:(fun (original, _) ->
         List.iter [ "_copy"; "_alone" ] ~f:(fun suffix ->
           let name = original ^ suffix in
           if not ([%equal: string option] (find name) (find original))
           then
             print_s
               [%message
                 "differs"
                   (name : string)
                   (find original : string option)
                   (find name : string option)])))
;;
