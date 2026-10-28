open! Import

(* The unread input is [buffer]'s bytes [start, stop). No header ends before
   [scanned]. *)
type t =
  { input : Core_unix.File_descr.t
  ; output : Core_unix.File_descr.t
  ; read_size : int
  ; mutable buffer : Bytes.t
  ; mutable start : int
  ; mutable stop : int
  ; mutable scanned : int
  }

let create ?(read_size = 65536) ~input ~output () =
  { input
  ; output
  ; read_size
  ; buffer = Bytes.create read_size
  ; start = 0
  ; stop = 0
  ; scanned = 0
  }
;;

let of_stdio () =
  let output = Core_unix.dup Core_unix.stdout in
  Core_unix.set_close_on_exec output;
  Core_unix.dup2 ~src:Core_unix.stderr ~dst:Core_unix.stdout ();
  create ~input:Core_unix.stdin ~output ()
;;

(* Makes room for [read_size] more bytes after [stop]. *)
let make_room t =
  if t.stop + t.read_size > Bytes.length t.buffer
  then (
    let length = t.stop - t.start in
    let buffer =
      if length + t.read_size > Bytes.length t.buffer
      then
        Bytes.create
          (Int.max (2 * Bytes.length t.buffer) (length + t.read_size))
      else t.buffer
    in
    Bytes.blit ~src:t.buffer ~src_pos:t.start ~dst:buffer ~dst_pos:0 ~len:length;
    t.buffer <- buffer;
    t.scanned <- t.scanned - t.start;
    t.start <- 0;
    t.stop <- length)
;;

(* Reads more, returning false at the end of the input. *)
let fill t =
  make_room t;
  match Core_unix.read t.input ~buf:t.buffer ~pos:t.stop ~len:t.read_size with
  | 0 -> false
  | n ->
    t.stop <- t.stop + n;
    true
  | exception Core_unix.Unix_error (EINTR, _, _) -> true
;;

let header_end = "\r\n\r\n"

let find_header_end t =
  let matches_at index =
    String.for_alli header_end ~f:(fun i c ->
      Char.equal (Bytes.get t.buffer (index + i)) c)
  in
  let rec loop index =
    if index + String.length header_end > t.stop
    then (
      t.scanned <- index;
      None)
    else if matches_at index
    then (
      t.scanned <- index;
      Some index)
    else loop (index + 1)
  in
  loop (Int.max t.start t.scanned)
;;

let content_length headers =
  String.split headers ~on:'\n'
  |> List.find_map ~f:(fun line ->
    match String.lsplit2 line ~on:':' with
    | Some (name, value)
      when String.Caseless.equal (String.strip name) "content-length" ->
      Int.of_string_opt (String.strip value)
    | Some _ | None -> None)
;;

let write_string t data =
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

let frame_json json =
  let body = Yojson.Safe.to_string json in
  sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body
;;

let frame packet = frame_json (Jsonrpc.Packet.yojson_of_t packet)
let write t packet = write_string t (frame packet)

let respond_error t ~id ~code message =
  write_string
    t
    (frame_json
       (`Assoc
           [ "jsonrpc", `String "2.0"
           ; "id", id
           ; ( "error"
             , Jsonrpc.Response.Error.yojson_of_t
                 (Jsonrpc.Response.Error.make ~code ~message ()) )
           ]))
;;

let without_null_params : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc fields ->
    `Assoc
      (List.filter fields ~f:(function
         | "params", `Null -> false
         | _ -> true))
  | json -> json
;;

let id_of_json : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc fields ->
    (match List.Assoc.find fields ~equal:String.equal "id" with
     | Some (`Int _ as id) | Some (`String _ as id) -> id
     | Some _ | None -> `Null)
  | _ -> `Null
;;

let parse t body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error message ->
    respond_error t ~id:`Null ~code:ParseError message;
    Error message
  | json ->
    (match Jsonrpc.Packet.t_of_yojson (without_null_params json) with
     | packet -> Ok packet
     | exception exn ->
       let message = Exn.to_string exn in
       respond_error t ~id:(id_of_json json) ~code:InvalidRequest message;
       Error message)
;;

let rec read t =
  match find_header_end t with
  | None -> if fill t then read t else None
  | Some index ->
    let body_start = index + String.length header_end in
    (match
       content_length
         (Bytes.To_string.sub t.buffer ~pos:t.start ~len:(index - t.start))
     with
     | None ->
       t.start <- body_start;
       Some (Error "a message without a Content-Length header")
     | Some length when t.stop - body_start >= length ->
       t.start <- body_start + length;
       Some (parse t (Bytes.To_string.sub t.buffer ~pos:body_start ~len:length))
     | Some _ -> if fill t then read t else None)
;;

let has_pending_input ?(within = Time_ns.Span.zero) t =
  t.stop > t.start
  ||
  match
    Core_unix.select
      ~read:[ t.input ]
      ~write:[]
      ~except:[]
      ~timeout:
        (if Time_ns.Span.( <= ) within Time_ns.Span.zero
         then `Immediately
         else `After within)
      ()
  with
  | { read = _ :: _; _ } -> true
  | { read = []; _ } -> false
  | exception Core_unix.Unix_error (EINTR, _, _) -> false
;;
