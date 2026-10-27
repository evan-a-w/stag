open! Import

let project_file = "stag-project"

let find_root dir =
  let rec loop dir =
    if Sys_unix.file_exists_exn (dir ^/ project_file)
    then Ok dir
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir
      then
        Or_error.errorf
          "No %s file in the current directory or any of its ancestors"
          project_file
      else loop parent)
  in
  loop dir
;;

module Target = struct
  type t =
    { stanza : Stanza.t
    ; dir : string
    ; files : Source_file.t list
    }
  [@@deriving sexp_of]

  let name t = t.stanza.name.txt
end

type t =
  { root : string
  ; targets : Target.t list
  ; by_name : Target.t String.Map.t
  }
[@@deriving fields ~getters]

let find t name = Map.find t.by_name name
let relative ~dir name = if String.equal dir "." then name else dir ^/ name

let is_skipped name =
  String.is_prefix name ~prefix:"." || String.is_prefix name ~prefix:"_"
;;

let read_targets ~root ~error =
  let rec walk dir =
    let absolute = root ^/ dir in
    let entries =
      Sys_unix.readdir absolute
      |> Array.to_list
      |> List.sort ~compare:String.compare
    in
    let is_file name = Sys_unix.is_file_exn (absolute ^/ name) in
    let target =
      if not (List.mem entries "stag" ~equal:String.equal && is_file "stag")
      then None
      else (
        let path = relative ~dir "stag" in
        match Stanza.parse ~path (In_channel.read_all (absolute ^/ "stag")) with
        | Error errors ->
          List.iter errors ~f:error;
          None
        | Ok None -> None
        | Ok (Some stanza) ->
          let files =
            List.filter entries ~f:(fun name ->
              String.is_suffix name ~suffix:Source_file.extension
              && is_file name)
            |> List.map ~f:(fun name ->
              { Source_file.path = relative ~dir name
              ; contents = In_channel.read_all (absolute ^/ name)
              })
          in
          Some { Target.stanza; dir; files })
    in
    let subdirs =
      List.filter entries ~f:(fun name ->
        (not (is_skipped name)) && Sys_unix.is_directory_exn (absolute ^/ name))
    in
    Option.to_list target
    @ List.concat_map subdirs ~f:(fun name -> walk (relative ~dir name))
  in
  walk "."
;;

let check_file_names (target : Target.t) ~error =
  List.iter target.files ~f:(fun { path; contents = _ } ->
    match Source_file.module_name path with
    | Ok (_ : string) -> ()
    | Error message ->
      error
        (Diagnostic.create
           ~whole_file:true
           ~loc:(Loc.start_of_file path)
           message))
;;

let index targets ~error =
  List.fold
    targets
    ~init:String.Map.empty
    ~f:(fun by_name (target : Target.t) ->
      let name = target.stanza.name in
      match Map.find by_name name.txt with
      | Some (other : Target.t) ->
        error
        @@ Diagnostic.create
             ~loc:name.loc
             (sprintf
                "The name %s is already used by the %s in %s"
                name.txt
                (Stanza.Kind.to_string other.stanza.kind)
                other.stanza.name.loc.file);
        by_name
      | None -> Map.set by_name ~key:name.txt ~data:target)
;;

let check_libraries targets ~by_name ~error =
  List.iter targets ~f:(fun (target : Target.t) ->
    List.iter target.stanza.libraries ~f:(fun library ->
      match Map.find by_name library.txt with
      | None ->
        error
        @@ Diagnostic.create
             ~loc:library.loc
             (sprintf "Unknown library %s" library.txt)
      | Some (other : Target.t) ->
        (match other.stanza.kind with
         | Library -> ()
         | Executable | Test ->
           error
           @@ Diagnostic.create
                ~loc:library.loc
                (sprintf
                   "%s is %s, not a library"
                   library.txt
                   (Stanza.Kind.to_string_with_article other.stanza.kind)))))
;;

let deps ~by_name (target : Target.t) =
  List.map target.stanza.libraries ~f:(fun library ->
    library, Map.find_exn by_name library.txt)
;;

let check_cycles targets ~by_name ~error =
  let finished = String.Hash_set.create () in
  let rec visit (target : Target.t) ~stack =
    let name = Target.name target in
    if not (Hash_set.mem finished name)
    then (
      let stack = name :: stack in
      List.iter
        (deps ~by_name target)
        ~f:(fun ((library : string Located.t), dep) ->
          if List.mem stack library.txt ~equal:String.equal
          then (
            let cycle =
              List.drop_while
                (List.rev stack)
                ~f:(Fn.non (String.equal library.txt))
            in
            error
            @@ Diagnostic.create
                 ~loc:library.loc
                 (sprintf
                    "Dependency cycle between libraries: %s"
                    (String.concat ~sep:" -> " (cycle @ [ library.txt ]))))
          else visit dep ~stack);
      Hash_set.add finished name)
  in
  List.iter targets ~f:(visit ~stack:[])
;;

let closure_of ~by_name target =
  let seen = String.Hash_set.create () in
  let order = Queue.create () in
  let rec visit target =
    List.iter (deps ~by_name target) ~f:(fun (_, (dep : Target.t)) ->
      if not (Hash_set.mem seen (Target.name dep))
      then (
        Hash_set.add seen (Target.name dep);
        visit dep;
        Queue.enqueue order dep))
  in
  visit target;
  Queue.to_list order
;;

let closure t target = closure_of ~by_name:t.by_name target

let check_module_clashes targets ~by_name ~error =
  List.iter targets ~f:(fun (target : Target.t) ->
    match target.stanza.kind with
    | Library -> ()
    | Executable | Test ->
      let libraries =
        closure_of ~by_name target
        |> List.map ~f:(fun (library : Target.t) ->
          Stanza.module_name library.stanza, library)
        |> String.Map.of_alist_exn
      in
      List.iter target.files ~f:(fun { path; contents = _ } ->
        match Source_file.module_name path with
        | Error _ -> ()
        | Ok module_name ->
          Option.iter (Map.find libraries module_name) ~f:(fun library ->
            error
            @@ Diagnostic.create
                 ~whole_file:true
                 ~loc:(Loc.start_of_file path)
                 (sprintf
                    "The module %s is also the library %s"
                    module_name
                    (Target.name library)))))
;;

let load ~root =
  let errors = Queue.create () in
  let error = Queue.enqueue errors in
  let ok_so_far () = Queue.is_empty errors in
  let targets = read_targets ~root ~error in
  List.iter targets ~f:(check_file_names ~error);
  let by_name = index targets ~error in
  check_libraries targets ~by_name ~error;
  if ok_so_far () then check_cycles targets ~by_name ~error;
  if ok_so_far () then check_module_clashes targets ~by_name ~error;
  if ok_so_far ()
  then Ok { root; targets; by_name }
  else Error (Queue.to_list errors)
;;
