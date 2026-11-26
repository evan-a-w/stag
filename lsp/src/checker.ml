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

(* [absolute] locates in absolute paths, and those without a location in [path]. *)
let diagnostics_by_file ~path ~absolute ~files diagnostics =
  let init =
    List.map files ~f:(fun file -> file, [])
    |> String.Map.of_alist_reduce ~f:Fn.const
  in
  List.fold diagnostics ~init ~f:(fun map (diagnostic : Diagnostic.t) ->
    let diagnostic = Diagnostic.relocate diagnostic ~f:absolute in
    let file =
      if Loc.is_none diagnostic.loc then path else Loc.file_name diagnostic.loc
    in
    Map.add_multi map ~key:file ~data:diagnostic)
  |> Map.map ~f:List.rev
;;

(* [program] is the program's modules, if they parsed, and what checked of them. *)
let of_checked
  ?prelude
  ?(of_prelude = false)
  ~path
  ~absolute
  ~files
  ~diagnostics
  program
  =
  { Result.diagnostics = diagnostics_by_file ~path ~absolute ~files diagnostics
  ; index =
      Option.map program ~f:(fun (groups, (checked : Typecheck.Checked.t)) ->
        Index.create ?prelude ~of_prelude ~groups ~checked ()
        |> Index.relocate ~f:absolute)
  ; depends_on = []
  }
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
    | External _
    | Malformed_exception _
    | Module_splice _
    | Include _
    | Expect_test _
    | Bench _
    | Attributed _
    | Attribute _ ->
      Option.some_if
        (not
           (Loc.File.equal
              (relocate toplevel.loc : Loc.t).file
              toplevel.loc.file))
        toplevel)
;;

let source_paths checkout =
  Stdlib_checkout.source_paths checkout
  |> Core.Result.ok
  |> Option.value ~default:[]
;;

(* A program sees [prelude_choice], loaded into [checked]; the embedded one's
   definitions are found in [stdlib]'s sources that are the same. *)
let of_program
  ?stdlib
  ~contents
  ~path
  ~absolute
  ~files
  ~depends_on
  ~(prelude_choice : Pipeline_config.Prelude_choice.t)
  checked
  =
  let result =
    match checked with
    | Error diagnostic ->
      (* The prelude's error is reported in its source, and the program left
         unchecked, which is said in it. *)
      of_checked
        ~path
        ~absolute
        ~files
        ~diagnostics:
          [ diagnostic
          ; Diagnostic.create
              ~loc:(Loc.start_of_file (Loc.File.of_string path))
              "This file is not checked: its checkout's prelude has errors"
          ]
        None
    | Ok
        ( (prelude : Pipeline_config.Loaded_prelude.t)
        , { Pipeline.Checked.groups
          ; checked
          ; diagnostics
          ; warnings
          ; driver = _
          } ) ->
      let diagnostics = Diagnostic.sort (diagnostics @ warnings) in
      let program = Option.both groups checked in
      (match prelude, stdlib with
       | Parsed prelude, _ ->
         of_checked ~prelude ~path ~absolute ~files ~diagnostics program
       | Embedded, None ->
         of_checked ~path ~absolute ~files ~diagnostics program
       | Embedded, Some checkout ->
         let embedded = Stdlib_checkout.relocate_embedded ~contents checkout in
         of_checked
           ~prelude:(relocated ~relocate:embedded (force Prelude.toplevels))
           ~path
           ~absolute:(Fn.compose absolute embedded)
           ~files
           ~diagnostics
           program)
  in
  let prelude_sources =
    match prelude_choice, stdlib with
    | Checkout checkout, _ | Embedded, Some checkout -> source_paths checkout
    | Embedded, None -> []
  in
  { result with depends_on = depends_on @ prelude_sources }
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
      ~diagnostics:[ error ]
      None
  | Ok prelude ->
    let { Pipeline.Checked.groups; checked; diagnostics; warnings; driver = _ } =
      Pipeline.check_prelude
        ~record_references:true
        ~limits
        ~output:ignore
        prelude
    in
    let diagnostics = Diagnostic.sort (diagnostics @ warnings) in
    of_checked
      ~of_prelude:true
      ~path
      ~absolute:Fn.id
      ~files
      ~diagnostics
      (Option.both groups checked)
;;

let standalone ?stdlib ~limits ~contents ~depends_on path =
  let config = Pipeline_config.create ~limits [ path ] in
  of_program
    ?stdlib
    ~contents
    ~path
    ~absolute:Fn.id
    ~files:[ path ]
    ~depends_on
    ~prelude_choice:config.prelude
    (Pipeline.check
       ~load_prelude:(Pipeline_config.Loaded_prelude.load ~contents)
       ~record_references:true
       ~output:ignore
       config
       (Pipeline.parse_files
          [ { Source_file.path; contents = read ~contents path } ]))
;;

let absolute_loc ~root (loc : Loc.t) =
  if Loc.is_none loc || Filename.is_absolute (Loc.file_name loc)
  then loc
  else { loc with file = Loc.File.of_string (root ^/ Loc.file_name loc) }
;;

let relative ~root path =
  match String.chop_prefix path ~prefix:(root ^ "/") with
  | Some relative -> relative
  | None -> path
;;

(* Checked within its target, or, if in none, the project's files that would
   change that. *)
let in_project ?stdlib ~limits ~contents ~root path =
  let absolute = absolute_loc ~root in
  match
    Project.load
      ~contents:(fun relative -> contents (root ^/ relative))
      ~root
      ()
  with
  | Error errors ->
    First
      { Result.diagnostics =
          diagnostics_by_file ~path ~absolute ~files:[ path ] errors
      ; index = None
      ; depends_on = [ root ^/ Project.project_file ]
      }
  | Ok project ->
    let depends_on =
      List.map (Project.input_files project) ~f:(fun file -> root ^/ file)
    in
    let file = relative ~root path in
    (match
       List.find (Project.targets project) ~f:(fun target ->
         List.exists target.files ~f:(fun source ->
           String.equal source.path file))
     with
     | None -> Second depends_on
     | Some target ->
       First
         (of_program
            ?stdlib
            ~contents
            ~path
            ~absolute
            ~files:(Program.paths project target)
            ~depends_on
            ~prelude_choice:(Program.prelude_choice project target)
            (Program.check
               ~load_prelude:(Pipeline_config.Loaded_prelude.load ~contents)
               ~record_references:true
               ~output:ignore
               ~limits
               project
               target)))
;;

let check ?stdlib ~limits ~contents path =
  match Stdlib_checkout.of_source path with
  | Some checkout -> in_stdlib ~limits ~contents checkout path
  | None ->
    (match Project.find_root (Filename.dirname path) with
     | Error _ -> standalone ?stdlib ~limits ~contents ~depends_on:[] path
     | Ok root ->
       (match in_project ?stdlib ~limits ~contents ~root path with
        | First result -> result
        | Second depends_on ->
          standalone ?stdlib ~limits ~contents ~depends_on path))
;;
