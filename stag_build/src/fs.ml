open! Import

module Kind = struct
  type t =
    | File
    | Directory
    | Other

  let of_kind : Core_unix.file_kind -> t = function
    | S_REG -> File
    | S_DIR -> Directory
    | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> Other
  ;;

  let of_path path =
    match (Core_unix.lstat path).st_kind with
    | S_LNK ->
      (match of_kind (Core_unix.stat path).st_kind with
       | File -> File
       | Directory | Other -> Other)
    | kind -> of_kind kind
  ;;
end

let reason = function
  | Sys_error message ->
    Some
      (String.rsplit2 message ~on:':'
       |> Option.value_map ~default:message ~f:(fun (_, reason) ->
         String.strip reason))
  | Core_unix.Unix_error (error, _, _) -> Some (Core_unix.Error.message error)
  | _ -> None
;;

let try_ ~path f =
  match f () with
  | x -> Ok x
  | exception exn ->
    (match reason exn with
     | None -> raise exn
     | Some reason ->
       Error (Diagnostic.create ~loc:Loc.none (sprintf "%s: %s" path reason)))
;;

let canonical path =
  Option.value
    (Option.try_with (fun () -> Filename_unix.realpath path))
    ~default:path
;;

let relative_path ~from path =
  let parts path =
    String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty)
  in
  let rec common from path =
    match from, path with
    | a :: from, b :: path when String.equal a b -> common from path
    | _ -> from, path
  in
  let up, down = common (parts from) (parts path) in
  match List.map up ~f:(Fn.const "..") @ down with
  | [] -> "."
  | parts -> String.concat ~sep:"/" parts
;;

let relative ~dir name = if String.equal dir "." then name else dir ^/ name

let read_dir ?(must_read = Fn.const false) ~error ~root dir =
  match try_ ~path:dir (fun () -> Sys_unix.readdir (root ^/ dir)) with
  | Error diagnostic ->
    error diagnostic;
    []
  | Ok names ->
    Array.to_list names
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun name ->
      let path = relative ~dir name in
      match try_ ~path (fun () -> Kind.of_path (root ^/ path)) with
      | Ok kind -> Some (name, kind)
      | Error diagnostic ->
        if must_read name then error diagnostic;
        None)
;;
