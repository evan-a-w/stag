open! Import

module Serialized = struct
  type t =
    { version : int
    ; benches : (string * float array) list
    }
  [@@deriving sexp]
end

type t = float array String.Map.t

let version = 1
let sexp_of_t t = Serialized.sexp_of_t { version; benches = Map.to_alist t }

let t_of_sexp sexp =
  let { Serialized.version = v; benches } = Serialized.t_of_sexp sexp in
  if v <> version
  then raise_s [%message "unsupported baseline version" ~version:(v : int)];
  String.Map.of_alist_exn benches
;;

let empty = String.Map.empty

let update t results =
  List.fold results ~init:t ~f:(fun t (result : Bench_result.t) ->
    Map.set t ~key:result.name ~data:(Bench_result.per_iteration_ns result))
;;

let find = Map.find

let validate_name name =
  let valid_char c =
    Char.is_alphanum c || List.mem [ '-'; '_'; '.' ] c ~equal:Char.equal
  in
  if String.is_empty name
  then Or_error.error_string "baseline name is empty"
  else if Char.equal name.[0] '.'
  then Or_error.error_s [%message "baseline name starts with '.'" name]
  else if not (String.for_all name ~f:valid_char)
  then
    Or_error.error_s
      [%message
        "baseline name may only contain letters, digits, '-', '_' and '.'" name]
  else Ok ()
;;

let path ~dir ~name = Filename.concat dir (name ^ ".sexp")

let load ~path =
  if not (Stdlib.Sys.file_exists path)
  then Or_error.error_s [%message "no baseline at" path]
  else (
    match Sexp.load_sexp_conv_exn path t_of_sexp with
    | t -> Ok t
    | exception exn ->
      Or_error.error_s [%message "malformed baseline" path (exn : exn)])
;;

let rec mkdir_p dir =
  if not (Stdlib.Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    try Stdlib.Sys.mkdir dir 0o755 with
    | Sys_error _ when Stdlib.Sys.is_directory dir -> ())
;;

let save t ~path =
  Or_error.try_with (fun () ->
    mkdir_p (Filename.dirname path);
    let tmp = path ^ ".tmp" in
    Out_channel.write_all tmp ~data:(Sexp.to_string_hum (sexp_of_t t) ^ "\n");
    Stdlib.Sys.rename tmp path)
;;
