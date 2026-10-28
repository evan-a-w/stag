open! Import

let file ({ path; contents } : Source_file.t) =
  Formatter.format ~file:path contents
  |> Result.map_error ~f:Diagnostic.of_parse_error
;;

module Mode = struct
  type t =
    | Check
    | Inplace
end

module Unformatted = struct
  type t =
    { file : Source_file.t
    ; formatted : string
    }

  let diff { file = { path; contents }; formatted } =
    Text_diff.unified
      ~old_name:path
      ~new_name:path
      ~old:contents
      ~new_:formatted
    |> Option.value ~default:""
  ;;
end

module Summary = struct
  type t =
    { unformatted : Unformatted.t list
    ; errors : Diagnostic.t list
    }
end

let write ?root ~path data =
  let full_path =
    Option.value_map root ~default:path ~f:(fun root -> root ^/ path)
  in
  match Out_channel.write_all full_path ~data with
  | () -> Ok ()
  | exception Sys_error message ->
    let reason =
      String.rsplit2 message ~on:':'
      |> Option.value_map ~default:message ~f:(fun (_, reason) ->
        String.strip reason)
    in
    Error (Diagnostic.create ~loc:Loc.none (sprintf "%s: %s" path reason))
;;

let run ?root ~(mode : Mode.t) files =
  let unformatted = Queue.create () in
  let errors = Queue.create () in
  List.iter files ~f:(fun (source : Source_file.t) ->
    match file source with
    | Error diagnostic -> Queue.enqueue errors diagnostic
    | Ok formatted when String.equal formatted source.contents -> ()
    | Ok formatted ->
      let unformatted_file = { Unformatted.file = source; formatted } in
      (match mode with
       | Check -> Queue.enqueue unformatted unformatted_file
       | Inplace ->
         (match write ?root ~path:source.path formatted with
          | Ok () -> Queue.enqueue unformatted unformatted_file
          | Error diagnostic -> Queue.enqueue errors diagnostic)));
  { Summary.unformatted = Queue.to_list unformatted
  ; errors = Queue.to_list errors
  }
;;
