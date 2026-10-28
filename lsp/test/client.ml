open! Import

type t =
  { root : string
  ; server : Server.t
  ; sent : Jsonrpc.Packet.t Queue.t
  ; mutable next_id : int
  ; contents : string String.Table.t
  }

let rec remove path =
  if Sys_unix.is_directory_exn path
  then (
    Array.iter (Sys_unix.readdir path) ~f:(fun name -> remove (path ^/ name));
    Core_unix.rmdir path)
  else Sys_unix.remove path
;;

let server t = t.server
let path t file = t.root ^/ file
let uri t file = Lsp.Uri.to_string (Lsp.Uri.of_path (path t file))

let relativize t string =
  String.substr_replace_all
    (String.substr_replace_all
       string
       ~pattern:(Lsp.Uri.to_string (Lsp.Uri.of_path t.root) ^ "/")
       ~with_:"")
    ~pattern:(t.root ^ "/")
    ~with_:""
;;

let print_json t json =
  print_endline (relativize t (Yojson.Safe.pretty_to_string json))
;;

let print_diagnostics t (params : Lsp_types.PublishDiagnosticsParams.t) =
  let range (range : Lsp_types.Range.t) =
    sprintf
      "%d:%d-%d:%d"
      range.start.line
      range.start.character
      range.end_.line
      range.end_.character
  in
  printf
    "diagnostics %s%s:%s\n"
    (relativize t (Lsp.Uri.to_string params.uri))
    (match params.version with
     | Some version -> sprintf " (version %d)" version
     | None -> "")
    (if List.is_empty params.diagnostics then " none" else "");
  List.iter params.diagnostics ~f:(fun diagnostic ->
    let message =
      match diagnostic.message with
      | `String message -> message
      | `MarkupContent { value; _ } -> value
    in
    printf
      "  %s %s: %s\n"
      (range diagnostic.range)
      (match diagnostic.severity with
       | Some Error -> "error"
       | Some Warning -> "warning"
       | Some Information | Some Hint | None -> "info")
      (String.substr_replace_all message ~pattern:"\n" ~with_:"\n    ");
    List.iter
      (Option.value diagnostic.relatedInformation ~default:[])
      ~f:(fun related ->
        printf
          "    related %s %s: %s\n"
          (relativize t (Lsp.Uri.to_string related.location.uri))
          (range related.location.range)
          related.message))
;;

(* What the server sent, but responses, which it returns. *)
let flush t =
  let responses = Queue.create () in
  Queue.iter t.sent ~f:(fun (packet : Jsonrpc.Packet.t) ->
    match packet with
    | Notification notification ->
      (match Lsp.Server_notification.of_jsonrpc notification with
       | Ok (PublishDiagnostics params) -> print_diagnostics t params
       | _ -> print_json t (Jsonrpc.Notification.yojson_of_t notification))
    | Request request -> printf "request %s\n" request.method_
    | Response response -> Queue.enqueue responses response
    | Batch_response _ | Batch_call _ -> print_endline "batch");
  Queue.clear t.sent;
  Queue.to_list responses
;;

let check_pending t =
  Server.check_pending t.server;
  match flush t with
  | [] -> ()
  | _ -> print_endline "unexpected response"
;;

let notify ?(check = true) t method_ params =
  Server.handle
    t.server
    (Notification
       (Jsonrpc.Notification.create
          ~method_
          ?params:
            (match params with
             | `Null -> None
             | params -> Some (Jsonrpc.Structured.t_of_yojson params))
          ()));
  if check then Server.check_pending t.server;
  match flush t with
  | [] -> ()
  | _ -> print_endline "unexpected response"
;;

let request t method_ params =
  let id = `Int t.next_id in
  t.next_id <- t.next_id + 1;
  Server.handle
    t.server
    (Request
       (Jsonrpc.Request.create
          ~id
          ~method_
          ?params:
            (match params with
             | `Null -> None
             | params -> Some (Jsonrpc.Structured.t_of_yojson params))
          ()));
  match flush t with
  | [ { id = _; result = Ok result } ] -> Some result
  | [ { id = _; result = Error error } ] ->
    print_json t (Jsonrpc.Response.Error.yojson_of_t error);
    None
  | responses ->
    printf "%d responses\n" (List.length responses);
    None
;;

let text_document ?(extra = []) t file =
  `Assoc ([ "textDocument", `Assoc [ "uri", `String (uri t file) ] ] @ extra)
;;

let position ~line ~character =
  `Assoc [ "line", `Int line; "character", `Int character ]
;;

let did_open t file =
  let text =
    match Hashtbl.find t.contents file with
    | Some text -> text
    | None -> In_channel.read_all (path t file)
  in
  Hashtbl.set t.contents ~key:file ~data:text;
  notify
    t
    "textDocument/didOpen"
    (`Assoc
        [ ( "textDocument"
          , `Assoc
              [ "uri", `String (uri t file)
              ; "languageId", `String "stag"
              ; "version", `Int 1
              ; "text", `String text
              ] )
        ])
;;

let did_change ?check t file ~version text =
  Hashtbl.set t.contents ~key:file ~data:text;
  notify
    ?check
    t
    "textDocument/didChange"
    (`Assoc
        [ ( "textDocument"
          , `Assoc [ "uri", `String (uri t file); "version", `Int version ] )
        ; "contentChanges", `List [ `Assoc [ "text", `String text ] ]
        ])
;;

let print_semantic_tokens t file =
  match request t "textDocument/semanticTokens/full" (text_document t file) with
  | None -> ()
  | Some result ->
    let tokens = Lsp_types.SemanticTokens.t_of_yojson result in
    let text = Text.create (Hashtbl.find_exn t.contents file) in
    List.iter (Semantic_tokens.decode tokens.data) ~f:(fun token ->
      let start =
        Text.offset_of_position
          text
          (Lsp_types.Position.create
             ~line:token.line
             ~character:token.character)
      in
      let stop =
        Text.offset_of_position
          text
          (Lsp_types.Position.create
             ~line:token.line
             ~character:(token.character + token.length))
      in
      printf
        "%d:%d+%d %s%s %S\n"
        token.line
        token.character
        token.length
        (Sexp.to_string (Semantic_tokens.Token_type.sexp_of_t token.token_type))
        (if token.declaration then " (declaration)" else "")
        (String.sub (Text.contents text) ~pos:start ~len:(stop - start)))
;;

let with_server ?(limits = Server.default_limits) ?(files = []) f =
  let root = Filename_unix.realpath (Filename_unix.temp_dir "stag_lsp" "") in
  List.iter files ~f:(fun (file, contents) ->
    Core_unix.mkdir_p (Filename.dirname (root ^/ file));
    Out_channel.write_all (root ^/ file) ~data:contents);
  let sent = Queue.create () in
  let t =
    { root
    ; server = Server.create ~limits ~send:(Queue.enqueue sent)
    ; sent
    ; next_id = 0
    ; contents = String.Table.create ()
    }
  in
  Exn.protect
    ~f:(fun () ->
      ignore
        (request t "initialize" (`Assoc [ "capabilities", `Assoc [] ])
         : _ option);
      notify t "initialized" (`Assoc []);
      f t)
    ~finally:(fun () -> remove root)
;;

let position_request t method_ file ~line ~character =
  request
    t
    method_
    (text_document t file ~extra:[ "position", position ~line ~character ])
;;

let print_range (range : Lsp_types.Range.t) =
  sprintf
    "%d:%d-%d:%d"
    range.start.line
    range.start.character
    range.end_.line
    range.end_.character
;;

let print_hover t file ~line ~character =
  printf "%d:%d " line character;
  match position_request t "textDocument/hover" file ~line ~character with
  | None | Some `Null -> print_endline "no hover"
  | Some json ->
    let hover = Lsp_types.Hover.t_of_yojson json in
    (match hover.contents with
     | `MarkupContent { value; _ } ->
       printf
         "%s%s\n"
         (Option.value_map hover.range ~default:"" ~f:(fun range ->
            print_range range ^ " "))
         (String.substr_replace_all value ~pattern:"\n" ~with_:" ")
     | _ -> print_endline "unexpected hover")
;;

let print_definition t file ~line ~character =
  printf "%d:%d " line character;
  match position_request t "textDocument/definition" file ~line ~character with
  | None | Some `Null -> print_endline "no definition"
  | Some (`List locations) ->
    List.iter locations ~f:(fun json ->
      let { Lsp_types.Location.uri; range } =
        Lsp_types.Location.t_of_yojson json
      in
      printf
        "-> %s %s\n"
        (relativize t (Lsp.Uri.to_string uri))
        (print_range range))
  | Some json -> print_json t json
;;
