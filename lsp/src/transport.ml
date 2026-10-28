open! Import

type t =
  { input : Core_unix.File_descr.t
  ; output : Core_unix.File_descr.t
  ; buffer : Buffer.t
  ; chunk : Bytes.t
  }

let create ~input ~output =
  { input; output; buffer = Buffer.create 65536; chunk = Bytes.create 65536 }
;;

(* Reads more into the buffer, returning false at the end of the input. *)
let fill t =
  match
    Core_unix.read t.input ~buf:t.chunk ~pos:0 ~len:(Bytes.length t.chunk)
  with
  | 0 -> false
  | n ->
    Buffer.add_subbytes t.buffer t.chunk ~pos:0 ~len:n;
    true
  | exception Core_unix.Unix_error (EINTR, _, _) -> true
;;

let header_end = "\r\n\r\n"

let content_length headers =
  String.split headers ~on:'\n'
  |> List.find_map ~f:(fun line ->
    match String.lsplit2 line ~on:':' with
    | Some (name, value)
      when String.Caseless.equal (String.strip name) "content-length" ->
      Int.of_string_opt (String.strip value)
    | Some _ | None -> None)
;;

let consume t len =
  let rest = Buffer.To_string.subo t.buffer ~pos:len in
  Buffer.clear t.buffer;
  Buffer.add_string t.buffer rest
;;

let parse body =
  match Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string body) with
  | packet -> Ok packet
  | exception exn -> Error (Exn.to_string exn)
;;

let rec read t =
  let contents = Buffer.contents t.buffer in
  match String.substr_index contents ~pattern:header_end with
  | None -> if fill t then read t else None
  | Some index ->
    let body_start = index + String.length header_end in
    (match content_length (String.prefix contents index) with
     | None ->
       consume t body_start;
       Some (Error "a message without a Content-Length header")
     | Some length ->
       if String.length contents < body_start + length
       then if fill t then read t else None
       else (
         consume t (body_start + length);
         Some (parse (String.sub contents ~pos:body_start ~len:length))))
;;

let has_pending_input t =
  Buffer.length t.buffer > 0
  ||
  match
    Core_unix.select
      ~read:[ t.input ]
      ~write:[]
      ~except:[]
      ~timeout:`Immediately
      ()
  with
  | { read = _ :: _; _ } -> true
  | { read = []; _ } -> false
  | exception Core_unix.Unix_error (EINTR, _, _) -> false
;;

let frame packet =
  let body = Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet) in
  sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body
;;

let write t packet =
  let data = frame packet in
  let rec loop pos =
    if pos < String.length data
    then (
      let n =
        Core_unix.single_write_substring
          t.output
          ~buf:data
          ~pos
          ~len:(String.length data - pos)
      in
      loop (pos + n))
  in
  loop 0
;;
