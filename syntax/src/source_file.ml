open! Import

type t =
  { path : string
  ; contents : string
  }
[@@deriving sexp_of]

let extension = ".stag"

let module_name path =
  let basename = Filename.basename path in
  match String.chop_suffix basename ~suffix:extension with
  | None -> Error (sprintf "%s does not end in %s" basename extension)
  | Some name ->
    let is_ident_char c =
      Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
    in
    if
      String.is_empty name
      || (not (Char.is_alpha name.[0]))
      || not (String.for_all name ~f:is_ident_char)
    then
      Error
        (sprintf
           "%s is not a valid module file name: it should be a letter followed \
            by letters, digits, _ and ', then %s"
           basename
           extension)
    else Ok (String.capitalize name)
;;

module Error = struct
  type t =
    | Parse of Parser.Error.t
    | Invalid_file_name of
        { path : string
        ; message : string
        }
  [@@deriving sexp_of]

  let loc = function
    | Parse error -> Parser.Error.loc error
    | Invalid_file_name { path; message = _ } -> Loc.start_of_file path
  ;;
end

let parse { path; contents } =
  let%bind.Result name =
    module_name path
    |> Result.map_error ~f:(fun message ->
      Error.Invalid_file_name { path; message })
  in
  let%map.Result toplevels =
    Parser.program ~file:path contents
    |> Result.map_error ~f:(fun e -> Error.Parse e)
  in
  let loc = Loc.start_of_file path in
  Located.create
    ~loc
    (Syntax.Toplevel.Module (Located.create ~loc name, toplevels))
;;
