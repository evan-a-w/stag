open! Import
module Types = Lsp_types

module Document = struct
  module Checked = struct
    type t =
      { version : int
      ; index : Index.t option
      }
  end

  type t =
    { text : Text.t
    ; version : int
    ; checked : Checked.t option
    }

  let current_index t =
    match t.checked with
    | Some { version; index } when version = t.version -> index
    | Some _ | None -> None
  ;;
end

module State = struct
  type t =
    | Uninitialized
    | Running
    | Shut_down
    | Exited of int
end

type t =
  { limits : Limits.t
  ; send : Jsonrpc.Packet.t -> unit
  ; documents : Document.t String.Table.t
  ; pending : String.Hash_set.t
  ; published : Yojson.Safe.t String.Table.t
  ; virtual_uris : Lsp.Uri.t String.Table.t
  ; mutable state : State.t
  ; mutable refresh_semantic_tokens : bool
  ; mutable next_request_id : int
  }

let default_limits =
  { Limits.fuel = None
  ; max_depth = Some 100_000
  ; time_limit = Some (Time_ns.Span.of_int_sec 5)
  }
;;

let create ~limits ~send =
  { limits
  ; send
  ; documents = String.Table.create ()
  ; pending = String.Hash_set.create ()
  ; published = String.Table.create ()
  ; virtual_uris = String.Table.create ()
  ; state = Uninitialized
  ; refresh_semantic_tokens = false
  ; next_request_id = 0
  }
;;

let exit_code t =
  match t.state with
  | Exited code -> Some code
  | Uninitialized | Running | Shut_down -> None
;;

let notify t notification =
  t.send (Notification (Lsp.Server_notification.to_jsonrpc notification))
;;

let request t request =
  let id = `String (sprintf "stag-%d" t.next_request_id) in
  t.next_request_id <- t.next_request_id + 1;
  t.send (Request (Lsp.Server_request.to_jsonrpc_request request ~id))
;;

(* Documents without a file, eg. [untitled:Untitled-1], are checked
   standalone as [/untitled_1.stag]. *)
let path_of_uri t uri =
  let string = Lsp.Uri.to_string uri in
  if String.is_prefix string ~prefix:"file:"
  then Lsp.Uri.to_path uri
  else (
    let name =
      String.map
        (Lsp.Uri.to_path uri |> Filename.basename)
        ~f:(fun c -> if Char.is_alphanum c then Char.lowercase c else '_')
    in
    let path =
      sprintf
        "/%s%s.stag"
        (if String.is_empty name || not (Char.is_alpha name.[0])
         then "untitled_"
         else "")
        name
    in
    Hashtbl.set t.virtual_uris ~key:path ~data:uri;
    path)
;;

let uri_of_path t path =
  match Hashtbl.find t.virtual_uris path with
  | Some uri -> uri
  | None -> Lsp.Uri.of_path path
;;

let text_of_file t path =
  match Hashtbl.find t.documents path with
  | Some document -> document.text
  | None ->
    Text.create
      (try In_channel.read_all path with
       | Sys_error _ -> "")
;;

let contents t path =
  Option.map (Hashtbl.find t.documents path) ~f:(fun document ->
    Text.contents document.text)
;;

let lsp_diagnostic t (diagnostic : Diagnostic.t) =
  let range_in (loc : Loc.t) =
    let text = text_of_file t loc.file in
    if Loc.is_none loc
    then Text.range_of_offsets text ~start:0 ~stop:0
    else Text.range_of_loc text loc
  in
  let located, unlocated =
    List.partition_tf diagnostic.notes ~f:(fun note ->
      not (Loc.is_none note.loc))
  in
  let message =
    String.concat
      ~sep:"\n"
      (diagnostic.message
       :: List.map unlocated ~f:(fun note -> "Note: " ^ note.message))
  in
  let range =
    if diagnostic.whole_file
    then range_in (Loc.start_of_file diagnostic.loc.file)
    else range_in diagnostic.loc
  in
  Types.Diagnostic.create
    ~message:(`String message)
    ~range
    ~severity:Error
    ~source:"stag"
    ~relatedInformation:
      (List.map located ~f:(fun note ->
         Types.DiagnosticRelatedInformation.create
           ~location:
             (Types.Location.create
                ~uri:(uri_of_path t note.loc.file)
                ~range:(range_in note.loc))
           ~message:note.message))
    ()
;;

let publish t ~path diagnostics =
  let params =
    Types.PublishDiagnosticsParams.create
      ~uri:(uri_of_path t path)
      ?version:
        (Option.map (Hashtbl.find t.documents path) ~f:(fun document ->
           document.version))
      ~diagnostics:(List.map diagnostics ~f:(lsp_diagnostic t))
      ()
  in
  let json = Types.PublishDiagnosticsParams.yojson_of_t params in
  let unchanged =
    match Hashtbl.find t.published path with
    | Some previous -> Yojson.Safe.equal previous json
    | None -> List.is_empty diagnostics
  in
  if not unchanged
  then (
    if List.is_empty diagnostics
    then Hashtbl.remove t.published path
    else Hashtbl.set t.published ~key:path ~data:json;
    notify t (PublishDiagnostics params))
;;

let check t path =
  match Checker.check ~limits:t.limits ~contents:(contents t) path with
  | exception exn ->
    eprintf "stag lsp: checking %s: %s\n%!" path (Exn.to_string exn)
  | result ->
    Map.iteri result.diagnostics ~f:(fun ~key:file ~data:_ ->
      Hash_set.remove t.pending file;
      Hashtbl.change t.documents file ~f:(fun document ->
        Option.map document ~f:(fun (document : Document.t) ->
          { document with
            checked = Some { version = document.version; index = result.index }
          })));
    Map.iteri result.diagnostics ~f:(fun ~key:path ~data ->
      publish t ~path data)
;;

let check_pending t =
  let checked_any = not (Hash_set.is_empty t.pending) in
  let rec loop () =
    match Hash_set.find t.pending ~f:(Fn.const true) with
    | None -> ()
    | Some path ->
      Hash_set.remove t.pending path;
      check t path;
      loop ()
  in
  loop ();
  if checked_any && t.refresh_semantic_tokens
  then request t SemanticTokensRefresh
;;

let document_path t (text_document : Types.TextDocumentIdentifier.t) =
  path_of_uri t text_document.uri
;;

let offset_at t ~path position =
  let%map.Option document = Hashtbl.find t.documents path in
  document, Text.offset_of_position document.text position
;;

let parse ~path (document : Document.t) =
  Source_file.parse { path; contents = Text.contents document.text }
  |> Result.ok
;;

let semantic_tokens t ~path =
  let%map.Option document = Hashtbl.find t.documents path in
  let index = Document.current_index document in
  let occurrences =
    match parse ~path document with
    | Some toplevel -> Syntax_names.of_toplevels ~module_path:[] [ toplevel ]
    | None -> []
  in
  let tokens =
    Semantic_tokens.compute
      document.text
      ~file:path
      ~occurrences
      ~is_function:(fun offset ->
        match index with
        | Some index -> Index.is_function index ~file:path ~offset
        | None -> false)
  in
  Types.SemanticTokens.create ~data:(Semantic_tokens.encode tokens) ()
;;

let formatting t ~path =
  let%map.Option document = Hashtbl.find t.documents path in
  match Format.file { path; contents = Text.contents document.text } with
  | Error _ -> []
  | Ok formatted when String.equal formatted (Text.contents document.text) -> []
  | Ok formatted ->
    [ Types.TextEdit.create
        ~newText:formatted
        ~range:(Text.full_range document.text)
    ]
;;

let hover t ~path position =
  check_pending t;
  let%bind.Option document, offset = offset_at t ~path position in
  let%bind.Option index = Document.current_index document in
  let%map.Option { loc; text } = Index.hover index ~file:path ~offset in
  Types.Hover.create
    ~contents:
      (`MarkupContent (Types.MarkupContent.create ~kind:Markdown ~value:text))
    ~range:(Text.range_of_loc document.text loc)
    ()
;;

let definition t ~path position =
  check_pending t;
  let%bind.Option document, offset = offset_at t ~path position in
  let%bind.Option index = Document.current_index document in
  let%map.Option loc = Index.definition index ~file:path ~offset in
  `Location
    [ Types.Location.create
        ~uri:(uri_of_path t loc.file)
        ~range:(Text.range_of_loc (text_of_file t loc.file) loc)
    ]
;;

let document_symbols t ~path =
  let%bind.Option document = Hashtbl.find t.documents path in
  let%map.Option toplevel = parse ~path document in
  let toplevels =
    match toplevel.txt with
    | Module (_, toplevels) -> toplevels
    | _ -> [ toplevel ]
  in
  `DocumentSymbol (Document_symbols.of_toplevels document.text toplevels)
;;

let capabilities =
  Types.ServerCapabilities.create
    ~textDocumentSync:
      (`TextDocumentSyncOptions
          (Types.TextDocumentSyncOptions.create
             ~openClose:true
             ~change:Incremental
             ~save:
               (`SaveOptions (Types.SaveOptions.create ~includeText:false ()))
             ()))
    ~hoverProvider:(`Bool true)
    ~definitionProvider:(`Bool true)
    ~documentFormattingProvider:(`Bool true)
    ~documentSymbolProvider:(`Bool true)
    ~semanticTokensProvider:
      (`SemanticTokensOptions
          (Types.SemanticTokensOptions.create
             ~legend:Semantic_tokens.legend
             ~full:(`Bool true)
             ()))
    ()
;;

let initialize t (params : Types.InitializeParams.t) =
  t.state <- Running;
  t.refresh_semantic_tokens
  <- (let%bind.Option workspace = params.capabilities.workspace in
      let%bind.Option semantic_tokens = workspace.semanticTokens in
      semantic_tokens.refreshSupport)
     |> Option.value ~default:false;
  Types.InitializeResult.create
    ~capabilities
    ~serverInfo:(Types.InitializeResult.create_serverInfo ~name:"stag" ())
    ()
;;

let respond_error t id ~code message =
  t.send
    (Response
       (Jsonrpc.Response.error
          id
          (Jsonrpc.Response.Error.make ~code ~message ())))
;;

let handle_request t (jsonrpc : Jsonrpc.Request.t) =
  match Lsp.Client_request.of_jsonrpc jsonrpc with
  | Error message -> respond_error t jsonrpc.id ~code:InvalidParams message
  | Ok (E request) ->
    let result
      : type a.
        a Lsp.Client_request.t
        -> (a, Jsonrpc.Response.Error.Code.t * string) Result.t
      =
      fun request ->
      match t.state, request with
      | Uninitialized, Initialize params -> Ok (initialize t params)
      | (Running | Shut_down | Exited _), Initialize _ ->
        Error (InvalidRequest, "Already initialized")
      | Uninitialized, _ -> Error (ServerNotInitialized, "Not initialized")
      | (Shut_down | Exited _), _ -> Error (InvalidRequest, "Shut down")
      | Running, Shutdown ->
        t.state <- Shut_down;
        Ok ()
      | Running, TextDocumentFormatting params ->
        Ok (formatting t ~path:(document_path t params.textDocument))
      | Running, SemanticTokensFull params ->
        Ok (semantic_tokens t ~path:(document_path t params.textDocument))
      | Running, TextDocumentHover params ->
        Ok (hover t ~path:(document_path t params.textDocument) params.position)
      | Running, TextDocumentDefinition params ->
        Ok
          (definition
             t
             ~path:(document_path t params.textDocument)
             params.position)
      | Running, DocumentSymbol params ->
        Ok (document_symbols t ~path:(document_path t params.textDocument))
      | Running, _ -> Error (MethodNotFound, jsonrpc.method_)
    in
    (match result request with
     | exception exn ->
       respond_error t jsonrpc.id ~code:InternalError (Exn.to_string exn)
     | Error (code, message) -> respond_error t jsonrpc.id ~code message
     | Ok result ->
       t.send
         (Response
            (Jsonrpc.Response.ok
               jsonrpc.id
               (Lsp.Client_request.yojson_of_result request result))))
;;

let open_document t ~path ~version text =
  Hashtbl.set t.documents ~key:path ~data:{ text; version; checked = None };
  Hash_set.add t.pending path
;;

let handle_notification t (jsonrpc : Jsonrpc.Notification.t) =
  match Lsp.Client_notification.of_jsonrpc jsonrpc, t.state with
  | Ok Exit, _ ->
    t.state
    <- Exited
         (match t.state with
          | Shut_down -> 0
          | Uninitialized | Running | Exited _ -> 1)
  | _, (Uninitialized | Shut_down | Exited _) | Error _, Running -> ()
  | Ok notification, Running ->
    (match notification with
     | TextDocumentDidOpen { textDocument = { uri; version; text; _ } } ->
       open_document t ~path:(path_of_uri t uri) ~version (Text.create text)
     | TextDocumentDidChange { textDocument = { uri; version }; contentChanges }
       ->
       let path = path_of_uri t uri in
       Option.iter (Hashtbl.find t.documents path) ~f:(fun document ->
         open_document
           t
           ~path
           ~version
           (List.fold contentChanges ~init:document.text ~f:Text.apply_change))
     | DidSaveTextDocument { textDocument; _ } ->
       Hash_set.add t.pending (document_path t textDocument)
     | TextDocumentDidClose { textDocument } ->
       let path = document_path t textDocument in
       Hashtbl.remove t.documents path;
       Hash_set.remove t.pending path;
       publish t ~path []
     | _ -> ())
;;

let handle t (packet : Jsonrpc.Packet.t) =
  match packet with
  | Request request -> handle_request t request
  | Notification notification -> handle_notification t notification
  | Batch_call calls ->
    List.iter calls ~f:(function
      | `Request request -> handle_request t request
      | `Notification notification -> handle_notification t notification)
  | Response _ | Batch_response _ -> ()
;;

let run ?(debounce = Time_ns.Span.of_int_ms 150) ~limits transport =
  let t = create ~limits ~send:(Transport.write transport) in
  let rec loop () =
    match exit_code t with
    | Some code -> code
    | None ->
      if not (Transport.has_pending_input transport ~within:debounce)
      then check_pending t;
      (match Transport.read transport with
       | None -> 1
       | Some (Error message) ->
         eprintf "stag lsp: %s\n%!" message;
         loop ()
       | Some (Ok packet) ->
         (try handle t packet with
          | exn -> eprintf "stag lsp: %s\n%!" (Exn.to_string exn));
         loop ())
  in
  loop ()
;;
