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
    let diagnostic = Diagnostic.relocate diagnostic ~f:absolute in
    let file =
      if Loc.is_none diagnostic.loc then path else diagnostic.loc.file
    in
    Map.add_multi map ~key:file ~data:diagnostic)
  |> Map.map ~f:List.rev
;;

(* [checked] is the program's modules and the program checked, or its
   errors. *)
let of_checked ?prelude ~path ~absolute ~files checked =
  let diagnostics = diagnostics_by_file ~path ~absolute ~files in
  match checked with
  | Error errors -> { Result.diagnostics = diagnostics errors; index = None }
  | Ok (groups, driver) ->
    let checked = Driver.checked driver in
    let index =
      Index.create
        ?prelude
        ~groups
        ~program:checked.program
        ~expansions:checked.expansions
        ()
      |> Index.relocate ~f:absolute
    in
    { diagnostics = diagnostics []; index = Some index }
;;

(* The toplevels of [toplevels] located in files that [relocate] moves. *)
let rec relocated ~relocate (toplevels : Syntax.Toplevel.t list) =
  List.filter_map toplevels ~f:(fun toplevel ->
    let file = toplevel.loc.file in
    if not (String.equal (relocate toplevel.loc : Loc.t).file file)
    then Some toplevel
    else (
      match toplevel.txt with
      | Module (name, members) ->
        Some { toplevel with txt = Module (name, relocated ~relocate members) }
      | Let _
      | Type _
      | Exception _
      | Module_splice _
      | Include _
      | Expect_test _
      | Bench _ -> None))
;;

(* Definitions in the prelude are found in [stdlib]'s sources, those that are
   the embedded ones. *)
let check_groups ~limits ~stdlib ~path ~absolute ~files groups =
  let prelude, absolute =
    match stdlib with
    | None -> None, absolute
    | Some checkout ->
      let embedded = Stdlib_checkout.relocate_embedded checkout in
      ( Some (relocated ~relocate:embedded (force Prelude.toplevels))
      , Fn.compose absolute embedded )
  in
  of_checked
    ?prelude
    ~path
    ~absolute
    ~files
    (let%bind.Core.Result groups = groups in
     let%map.Core.Result driver =
       Program.check_groups ~limits ~output:ignore groups
     in
     groups, driver)
;;

let in_stdlib ~limits ~contents checkout path =
  let files =
    Stdlib_checkout.source_paths checkout
    |> Core.Result.ok
    |> Option.value ~default:[ path ]
  in
  of_checked
    ~path
    ~absolute:Fn.id
    ~files
    (let%bind.Core.Result sources =
       Stdlib_checkout.sources ~contents checkout
       |> Core.Result.map_error ~f:List.return
     in
     let%bind.Core.Result prelude =
       Prelude.parse sources
       |> Core.Result.map_error ~f:(fun error ->
         [ Diagnostic.of_source_file_error error ])
     in
     let%map.Core.Result driver =
       Driver.check_prelude ~limits ~output:ignore prelude
       |> Core.Result.map_error ~f:(fun error ->
         [ Diagnostic.of_typecheck_error error ])
     in
     ( [ Driver.Module_group.Group
           ( Located.create ~loc:Loc.none Prelude_path.root
           , List.map prelude ~f:(fun toplevel ->
               Driver.Module_group.File toplevel) )
       ]
     , driver ))
;;

let standalone ~limits ~stdlib ~contents path =
  let file = { Source_file.path; contents = read ~contents path } in
  check_groups
    ~limits
    ~stdlib
    ~path
    ~absolute:Fn.id
    ~files:[ path ]
    (Source_file.parse file
     |> Core.Result.map ~f:(fun toplevel ->
       [ Driver.Module_group.File toplevel ])
     |> Core.Result.map_error ~f:(fun error ->
       [ Diagnostic.of_source_file_error error ]))
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

let in_project ~limits ~stdlib ~contents ~root path =
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
      ~stdlib
      ~path
      ~absolute
      ~files
      (Program.parse project target)
;;

let check ?stdlib ~limits ~contents path =
  match Stdlib_checkout.of_source path with
  | Some checkout -> in_stdlib ~limits ~contents checkout path
  | None ->
    let stdlib =
      Option.first_some (Stdlib_checkout.find (Filename.dirname path)) stdlib
    in
    let project =
      match Project.find_root (Filename.dirname path) with
      | Error _ -> None
      | Ok root -> in_project ~limits ~stdlib ~contents ~root path
    in
    (match project with
     | Some result -> result
     | None -> standalone ~limits ~stdlib ~contents path)
;;
