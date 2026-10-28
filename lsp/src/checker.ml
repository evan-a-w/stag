open! Import

module Result = struct
  type t =
    { diagnostics : Diagnostic.t list String.Map.t
    ; index : Index.t option
    ; depends_on : string list
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
let of_checked ?prelude ?(of_prelude = false) ~path ~absolute ~files checked =
  let diagnostics = diagnostics_by_file ~path ~absolute ~files in
  match checked with
  | Error errors ->
    { Result.diagnostics = diagnostics errors; index = None; depends_on = [] }
  | Ok (groups, driver) ->
    let checked = Driver.checked driver in
    let index =
      Index.create
        ?prelude
        ~of_prelude
        ~groups
        ~program:checked.program
        ~expansions:checked.expansions
        ()
      |> Index.relocate ~f:absolute
    in
    { diagnostics = diagnostics []; index = Some index; depends_on = [] }
;;

(* The toplevels of [toplevels] located in files that [relocate] moves. *)
let rec relocated ~relocate (toplevels : Syntax.Toplevel.t list) =
  List.filter_map toplevels ~f:(fun toplevel ->
    match toplevel.txt with
    | Module (name, members) ->
      (match relocated ~relocate members with
       | [] -> None
       | members ->
         Some { toplevel with txt = Syntax.Toplevel.Module (name, members) })
    | Let _
    | Type _
    | Exception _
    | Module_splice _
    | Include _
    | Expect_test _
    | Bench _ ->
      Option.some_if
        (not
           (String.equal (relocate toplevel.loc : Loc.t).file toplevel.loc.file))
        toplevel)
;;

module Prelude_source = struct
  (* The prelude a program is checked with: the embedded one, whose
     definitions are found in [stdlib]'s sources that are the same, or the
     sources of the checkout the program is in. *)
  type t =
    | Embedded of { stdlib : Stdlib_checkout.t option }
    | Checkout of Stdlib_checkout.t

  let create ~stdlib ~path =
    match Stdlib_checkout.find (Filename.dirname path) with
    | Some checkout -> Checkout checkout
    | None -> Embedded { stdlib }
  ;;
end

let source_paths checkout =
  Stdlib_checkout.source_paths checkout
  |> Core.Result.ok
  |> Option.value ~default:[]
;;

(* Errors in the prelude's sources, which are reported in them, leave
   [path] unchecked, which is said in it. *)
let with_prelude_errors diagnostics ~path ~depends_on =
  if
    List.exists depends_on ~f:(fun source ->
      not (List.is_empty (Map.find_multi diagnostics source)))
  then
    Map.add_multi
      diagnostics
      ~key:path
      ~data:
        (Diagnostic.create
           ~loc:(Loc.start_of_file path)
           "This file is not checked: its checkout's prelude has errors")
  else diagnostics
;;

let check_groups
      ~limits
      ~(prelude : Prelude_source.t)
      ~contents
      ~path
      ~absolute
      ~files
      groups
  =
  let checked ?prelude groups =
    let%bind.Core.Result groups = groups in
    let%map.Core.Result driver =
      Program.check_groups ~limits ~output:ignore ?prelude groups
    in
    groups, driver
  in
  match prelude with
  | Embedded { stdlib = None } ->
    of_checked ~path ~absolute ~files (checked groups)
  | Embedded { stdlib = Some checkout } ->
    let embedded = Stdlib_checkout.relocate_embedded ~contents checkout in
    { (of_checked
         ~prelude:(relocated ~relocate:embedded (force Prelude.toplevels))
         ~path
         ~absolute:(Fn.compose absolute embedded)
         ~files
         (checked groups))
      with
      depends_on = source_paths checkout
    }
  | Checkout checkout ->
    let depends_on = source_paths checkout in
    let result =
      match Stdlib_checkout.prelude ~contents checkout with
      | Error diagnostic ->
        of_checked ~path ~absolute ~files (Error [ diagnostic ])
      | Ok prelude ->
        of_checked ~prelude ~path ~absolute ~files (checked ~prelude groups)
    in
    { result with
      diagnostics = with_prelude_errors result.diagnostics ~path ~depends_on
    ; depends_on
    }
;;

let in_stdlib ~limits ~contents checkout path =
  let files =
    match source_paths checkout with
    | [] -> [ path ]
    | files -> files
  in
  of_checked
    ~of_prelude:true
    ~path
    ~absolute:Fn.id
    ~files
    (let%map.Core.Result { prelude; driver } =
       Stdlib_checkout.check ~contents ~output:ignore ~limits checkout
       |> Core.Result.map_error ~f:List.return
     in
     ( [ Driver.Module_group.Group
           ( Located.create ~loc:Loc.none Prelude_path.root
           , List.map prelude ~f:(fun toplevel ->
               Driver.Module_group.File toplevel) )
       ]
     , driver ))
;;

let standalone ~limits ~prelude ~contents path =
  let file = { Source_file.path; contents = read ~contents path } in
  check_groups
    ~limits
    ~prelude
    ~contents
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

let in_project ~limits ~prelude ~contents ~root path =
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
      ; depends_on = []
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
      ~prelude
      ~contents
      ~path
      ~absolute
      ~files
      (Program.parse project target)
;;

let check ?stdlib ~limits ~contents path =
  match Stdlib_checkout.of_source path with
  | Some checkout -> in_stdlib ~limits ~contents checkout path
  | None ->
    let prelude = Prelude_source.create ~stdlib ~path in
    let project =
      match Project.find_root (Filename.dirname path) with
      | Error _ -> None
      | Ok root -> in_project ~limits ~prelude ~contents ~root path
    in
    (match project with
     | Some result -> result
     | None -> standalone ~limits ~prelude ~contents path)
;;
