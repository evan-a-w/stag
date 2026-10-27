open! Core

let rec remove path =
  if Sys_unix.is_directory_exn path
  then (
    Array.iter (Sys_unix.readdir path) ~f:(fun name -> remove (path ^/ name));
    Core_unix.rmdir path)
  else Sys_unix.remove path
;;

let with_dir files ~f =
  let root = Filename_unix.temp_dir "stag_build" "" in
  List.iter files ~f:(fun (path, contents) ->
    Core_unix.mkdir_p (Filename.dirname (root ^/ path));
    Out_channel.write_all (root ^/ path) ~data:contents);
  Exn.protect ~f:(fun () -> f root) ~finally:(fun () -> remove root)
;;

let print_diagnostics ~root diagnostics =
  let contents path =
    Option.try_with (fun () -> In_channel.read_all (root ^/ path))
  in
  List.iter diagnostics ~f:(fun diagnostic ->
    print_string (Stag.Diagnostic.to_string_hum ~contents diagnostic))
;;
