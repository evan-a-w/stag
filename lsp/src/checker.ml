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

(* [checked] is the program's modules, if they parsed, and the program
   checked, which with errors is what checked. *)
let of_checked
      ?prelude
      ?(of_prelude = false)
      ~path
      ~absolute
      ~files
      ~errors
      checked
  =
  let diagnostics = diagnostics_by_file ~path ~absolute ~files in
  match checked with
  | None ->
    { Result.diagnostics = diagnostics errors; index = None; depends_on = [] }
  | Some (groups, (checked : Typecheck.Checked.t), type_errors) ->
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
    { diagnostics =
        diagnostics
          (Diagnostic.sort
             (errors @ List.map type_errors ~f:Diagnostic.of_typecheck_error))
    ; index = Some index
    ; depends_on = []
    }
;;

let of_driver = function
  | Ok driver -> Driver.checked driver, []
  | Error { Typecheck.Failed.errors; partial } -> partial, errors
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
    | Let_mut _
    | Type _
    | Exception _
    | Malformed_exception _
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

let check_groups
      ~limits
      ~(prelude : Prelude_source.t)
      ~contents
      ~path
      ~absolute
      ~files
      groups
  =
  let groups, errors = groups in
  let checked ?prelude groups =
    Option.map groups ~f:(fun groups ->
      let checked, type_errors =
        of_driver (Driver.check_modules ~limits ~output:ignore ?prelude groups)
      in
      groups, checked, type_errors)
  in
  match prelude with
  | Embedded { stdlib = None } ->
    of_checked ~path ~absolute ~files ~errors (checked groups)
  | Embedded { stdlib = Some checkout } ->
    let embedded = Stdlib_checkout.relocate_embedded ~contents checkout in
    { (of_checked
         ~prelude:(relocated ~relocate:embedded (force Prelude.toplevels))
         ~path
         ~absolute:(Fn.compose absolute embedded)
         ~files
         ~errors
         (checked groups))
      with
      depends_on = source_paths checkout
    }
  | Checkout checkout ->
    let result =
      match Stdlib_checkout.prelude ~contents checkout with
      | Error diagnostic ->
        (* The prelude's error is reported in its source, and the program
           left unchecked, which is said in it. *)
        of_checked
          ~path
          ~absolute
          ~files
          ~errors:
            [ diagnostic
            ; Diagnostic.create
                ~loc:(Loc.start_of_file path)
                "This file is not checked: its checkout's prelude has errors"
            ]
          None
      | Ok prelude ->
        of_checked
          ~prelude
          ~path
          ~absolute
          ~files
          ~errors
          (checked ~prelude groups)
    in
    { result with depends_on = source_paths checkout }
;;

let in_stdlib ~limits ~contents checkout path =
  let files =
    match source_paths checkout with
    | [] -> [ path ]
    | files -> files
  in
  match Stdlib_checkout.prelude ~contents checkout with
  | Error error ->
    of_checked
      ~of_prelude:true
      ~path
      ~absolute:Fn.id
      ~files
      ~errors:[ error ]
      None
  | Ok prelude ->
    let checked, type_errors =
      of_driver (Driver.check_prelude ~limits ~output:ignore prelude)
    in
    of_checked
      ~of_prelude:true
      ~path
      ~absolute:Fn.id
      ~files
      ~errors:[]
      (Some
         ( [ Driver.Module_group.Group
               ( Located.create ~loc:Loc.none Prelude_path.root
               , List.map prelude ~f:(fun toplevel ->
                   Driver.Module_group.File toplevel) )
           ]
         , checked
         , type_errors ))
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
    (let toplevel, errors = Source_file.parse_recovering file in
     ( Option.map toplevel ~f:(fun toplevel ->
         [ Driver.Module_group.File toplevel ])
     , List.map errors ~f:Diagnostic.of_source_file_error ))
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
      (Program.parse_recovering project target)
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
