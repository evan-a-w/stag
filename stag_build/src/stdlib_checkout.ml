open! Import

type t = { root : string } [@@deriving sexp_of]

let marker_file = "stag-stdlib"
let prelude_file = "prelude/prelude.stag"
let core_dir = "core"
let root t = t.root
let create root = { root = Fs.canonical root }

let has_marker dir =
  match Fs.Kind.of_path (dir ^/ marker_file) with
  | File -> true
  | Directory | Other -> false
  | exception (Sys_error _ | Core_unix.Unix_error _) -> false
;;

(* The checkout whose [_build] directory [path] is in. *)
let rec building path =
  let parent = Filename.dirname path in
  if String.equal parent path
  then None
  else if String.equal (Filename.basename path) "_build" && has_marker parent
  then Some (create parent)
  else building parent
;;

let is_checkout dir = has_marker dir && Option.is_none (building dir)

let of_source path =
  let dir = Filename.dirname path in
  let parent = Filename.dirname dir in
  let%bind.Option root =
    if String.equal path (parent ^/ prelude_file)
    then Some parent
    else if String.equal dir (parent ^/ core_dir)
            && String.is_suffix path ~suffix:Source_file.extension
    then Some parent
    else None
  in
  Option.some_if (is_checkout root) (create root)
;;

let find dir =
  let rec find dir =
    if has_marker dir
    then Some (create dir)
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else find parent)
  in
  match building dir with
  | Some _ -> None
  | None -> find dir
;;

let of_executable path =
  let dir = Filename.dirname path in
  Option.first_some (building dir) (find dir)
;;

let core_paths t =
  let errors = Queue.create () in
  let core =
    Fs.read_dir ~error:(Queue.enqueue errors) ~root:t.root core_dir
    |> List.filter_map ~f:(fun (name, (kind : Fs.Kind.t)) ->
      match kind with
      | File when String.is_suffix name ~suffix:Source_file.extension ->
        Some (t.root ^/ core_dir ^/ name)
      | File | Directory | Other -> None)
  in
  match Queue.peek errors with
  | Some error -> Error error
  | None -> Ok core
;;

let prelude_path t = t.root ^/ prelude_file

let source_paths t =
  let%map.Result core = core_paths t in
  prelude_path t :: core
;;

let read ~contents path =
  match contents path with
  | Some contents -> Ok contents
  | None -> Fs.try_ ~path (fun () -> In_channel.read_all path)
;;

let sources ?(contents = Fn.const None) t =
  let read path =
    let%map.Result contents = read ~contents path in
    { Source_file.path; contents }
  in
  let%bind.Result prelude = read (prelude_path t) in
  let%bind.Result core_paths = core_paths t in
  let%map.Result core = List.map core_paths ~f:read |> Result.all in
  { Prelude.Sources.prelude; core }
;;

let prelude ?contents t =
  let%bind.Result sources = sources ?contents t in
  Prelude.parse sources |> Result.map_error ~f:Diagnostic.of_source_file_error
;;

let signature_of_source t (signature : Signature.t) path =
  if String.equal path (prelude_path t)
  then
    Ok
      (List.filter signature ~f:(function
        | Module { name = "Core"; _ } -> false
        | Module _ | Value _ | Type _ | Exception _ -> true))
  else (
    let%map.Result module_name =
      Source_file.module_name path
      |> Result.map_error ~f:(fun message ->
        Diagnostic.of_source_file_error (Invalid_file_name { path; message }))
    in
    List.filter_map signature ~f:(function
      | Module { name = "Core"; items } ->
        Some
          (Signature.Item.Module
             { name = "Core"
             ; items =
                 List.filter items ~f:(function
                   | Module { name; _ } -> String.equal name module_name
                   | Value _ | Type _ | Exception _ -> false)
             })
      | Module _ | Value _ | Type _ | Exception _ -> None))
;;

let checkout_path embedded =
  let { Prelude.Sources.prelude; core } = Prelude.Sources.embedded in
  if String.equal embedded prelude.path
  then Some prelude_file
  else
    List.find_map core ~f:(fun { path; contents = _ } ->
      Option.some_if
        (String.equal path embedded)
        (core_dir ^/ Filename.basename path))
;;

let relocate_embedded ?(contents = Fn.const None) t =
  let sources =
    List.filter_map
      (Prelude.Sources.embedded.prelude :: Prelude.Sources.embedded.core)
      ~f:(fun { path = embedded; contents = embedded_contents } ->
        let%bind.Option relative = checkout_path embedded in
        let path = t.root ^/ relative in
        let%bind.Option actual = read ~contents path |> Result.ok in
        Option.some_if (String.equal actual embedded_contents) (embedded, path))
    |> String.Map.of_alist_exn
  in
  fun (loc : Loc.t) ->
    match Map.find sources (Loc.file_name loc) with
    | Some file -> { loc with file = Loc.File.of_string file }
    | None -> loc
;;
