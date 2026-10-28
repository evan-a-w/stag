open! Import

let stale driver =
  List.filter (Driver.checked driver).expansions ~f:(fun expansion ->
    match expansion.status with
    | Stale _ -> true
    | Up_to_date -> false)
;;

let edit ({ loc; expansion; status } : Expansion.t) ~contents =
  match status with
  | Up_to_date -> None
  | Stale { generated } ->
    let indentation =
      Source_edit.indentation contents ~offset:loc.start.offset
    in
    let lines =
      String.split_lines generated
      |> List.map ~f:(fun line ->
        (if String.is_empty line then "" else indentation ^ "  " ^ line) ^ "\n")
    in
    Some
      { Source_edit.start = expansion.loc.start.offset
      ; stop = expansion.loc.stop.offset
      ; replacement = String.concat ("\n" :: lines) ^ indentation
      }
;;

let diagnostic ({ loc; _ } : Expansion.t) =
  Diagnostic.create
    ~loc
    "this inline expansion is out of date: the declarations written differ \
     from those its splice generates"
;;
