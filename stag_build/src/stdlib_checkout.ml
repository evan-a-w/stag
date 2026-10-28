open! Import

type t = { root : string } [@@deriving sexp_of]

let marker_file = "stag-stdlib"
let root t = t.root

let is_checkout dir =
  match Fs.Kind.of_path (dir ^/ marker_file) with
  | File -> true
  | Directory | Other -> false
  | exception (Sys_error _ | Core_unix.Unix_error _) -> false
;;

let of_source path =
  let dir = Filename.dirname path in
  let parent = Filename.dirname dir in
  let root =
    if String.equal path (parent ^/ Prelude.checkout_prelude)
    then Some parent
    else if
      String.equal dir (parent ^/ Prelude.checkout_core_dir)
      && String.is_suffix path ~suffix:Source_file.extension
    then Some parent
    else None
  in
  let%bind.Option root = root in
  Option.some_if (is_checkout root) { root }
;;

let rec find dir =
  if is_checkout dir
  then Some { root = dir }
  else (
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find parent)
;;

let of_executable path =
  let dir = Filename.dirname path in
  find
    (match String.substr_index dir ~pattern:"/_build/" with
     | Some index -> String.prefix dir index
     | None -> dir)
;;

let core_paths t =
  let errors = Queue.create () in
  let core =
    Fs.read_dir
      ~error:(Queue.enqueue errors)
      ~root:t.root
      Prelude.checkout_core_dir
    |> List.filter_map ~f:(fun (name, (kind : Fs.Kind.t)) ->
      match kind with
      | File when String.is_suffix name ~suffix:Source_file.extension ->
        Some (t.root ^/ Prelude.checkout_core_dir ^/ name)
      | File | Directory | Other -> None)
  in
  match Queue.peek errors with
  | Some error -> Error error
  | None -> Ok core
;;

let prelude_path t = t.root ^/ Prelude.checkout_prelude

let source_paths t =
  let%map.Result core = core_paths t in
  prelude_path t :: core
;;

let sources ?(contents = Fn.const None) t =
  let read path =
    match contents path with
    | Some contents -> Ok { Source_file.path; contents }
    | None ->
      let%map.Result contents =
        Fs.try_ ~path (fun () -> In_channel.read_all path)
      in
      { Source_file.path; contents }
  in
  let%bind.Result prelude = read (prelude_path t) in
  let%bind.Result core_paths = core_paths t in
  let%map.Result core = List.map core_paths ~f:read |> Result.all in
  { Prelude.Sources.prelude; core }
;;

let relocate_embedded t =
  let sources =
    List.filter_map
      (Prelude.Sources.embedded.prelude :: Prelude.Sources.embedded.core)
      ~f:(fun { path = embedded; contents } ->
        let%bind.Option relative = Prelude.checkout_path embedded in
        let path = t.root ^/ relative in
        match In_channel.read_all path with
        | on_disk ->
          Option.some_if (String.equal on_disk contents) (embedded, path)
        | exception Sys_error _ -> None)
    |> String.Map.of_alist_exn
  in
  fun (loc : Loc.t) ->
    match Map.find sources loc.file with
    | Some file -> { loc with file }
    | None -> loc
;;
