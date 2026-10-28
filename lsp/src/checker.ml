open! Import

module Result = struct
  type t =
    { diagnostics : Diagnostic.t list String.Map.t
    ; index : Index.t option
    }
end

let read ~contents path =
  match contents path with
  | Some contents -> contents
  | None ->
    (try In_channel.read_all path with
     | Sys_error _ -> "")
;;

(* [absolute] locates in absolute paths, and those without a location in
   [path]. *)
let diagnostics_by_file ~path ~absolute ~files diagnostics =
  let init =
    List.map files ~f:(fun file -> file, [])
    |> String.Map.of_alist_reduce ~f:Fn.const
  in
  List.fold diagnostics ~init ~f:(fun map (diagnostic : Diagnostic.t) ->
    let diagnostic =
      { diagnostic with
        loc = absolute diagnostic.loc
      ; notes =
          List.map diagnostic.notes ~f:(fun (note : Diagnostic.Note.t) ->
            { note with loc = absolute note.loc })
      }
    in
    let file =
      if Loc.is_none diagnostic.loc then path else diagnostic.loc.file
    in
    Map.add_multi map ~key:file ~data:diagnostic)
  |> Map.map ~f:List.rev
;;

let by_loc diagnostics =
  List.stable_sort diagnostics ~compare:(fun (a : Diagnostic.t) b ->
    Loc.compare a.loc b.loc)
;;

(* With errors, the index is of what checked. *)
let check_groups ~limits ~path ~absolute ~files (groups, parse_errors) =
  let diagnostics = diagnostics_by_file ~path ~absolute ~files in
  match groups with
  | None -> { Result.diagnostics = diagnostics parse_errors; index = None }
  | Some groups ->
    let (checked : Typecheck.Checked.t), errors =
      match Driver.check_modules ~limits ~output:ignore groups with
      | Ok driver -> Driver.checked driver, []
      | Error { errors; partial } ->
        partial, List.map errors ~f:Diagnostic.of_typecheck_error
    in
    { diagnostics = diagnostics (by_loc (parse_errors @ errors))
    ; index =
        Some
          (Index.create
             ~groups
             ~program:checked.program
             ~expansions:checked.expansions
           |> Index.relocate ~f:absolute)
    }
;;

let standalone ~limits ~contents path =
  let file = { Source_file.path; contents = read ~contents path } in
  let toplevel, errors = Source_file.parse_recovering file in
  check_groups
    ~limits
    ~path
    ~absolute:Fn.id
    ~files:[ path ]
    ( Option.map toplevel ~f:(fun toplevel ->
        [ Driver.Module_group.File toplevel ])
    , List.map errors ~f:Diagnostic.of_source_file_error )
;;

let absolute_loc ~root (loc : Loc.t) =
  if Loc.is_none loc || Filename.is_absolute loc.file
  then loc
  else { loc with file = root ^/ loc.file }
;;

let relative ~root path =
  match String.chop_prefix path ~prefix:(root ^ "/") with
  | Some relative -> relative
  | None -> path
;;

let in_project ~limits ~contents ~root path =
  let absolute = absolute_loc ~root in
  match
    Project.load
      ~contents:(fun relative -> contents (root ^/ relative))
      ~root
      ()
  with
  | Error errors ->
    Some
      { Result.diagnostics =
          diagnostics_by_file ~path ~absolute ~files:[ path ] errors
      ; index = None
      }
  | Ok project ->
    let file = relative ~root path in
    let%map.Option target =
      List.find (Project.targets project) ~f:(fun target ->
        List.exists target.files ~f:(fun source ->
          String.equal source.path file))
    in
    let files =
      List.concat_map
        (Project.closure project target @ [ target ])
        ~f:(fun target ->
          List.map target.files ~f:(fun source -> root ^/ source.path))
    in
    check_groups
      ~limits
      ~path
      ~absolute
      ~files
      (Program.parse_recovering project target)
;;

let check ~limits ~contents path =
  let project =
    match Project.find_root (Filename.dirname path) with
    | Error _ -> None
    | Ok root -> in_project ~limits ~contents ~root path
  in
  match project with
  | Some result -> result
  | None -> standalone ~limits ~contents path
;;
