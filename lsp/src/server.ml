open! Import
module Types = Lsp_types

module Document = struct
  module Checked = struct
    type t =
      { index : Index.t option
      ; generations : int String.Map.t
        (** Of the files checked, as they were. *)
      }
  end

  type t =
    { path : string
    ; checkable : bool
    ; text : Text.t
    ; version : int
    ; checked : Checked.t option
    }
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
  ; stdlib : Stdlib_checkout.t option
  ; send : Jsonrpc.Packet.t -> unit
  ; documents : Document.t String.Table.t
  ; paths : string String.Table.t
  ; pending : String.Hash_set.t
  ; published : Yojson.Safe.t String.Table.t
  ; generations : int String.Table.t
  ; covered : String.Set.t String.Table.t
  ; owners : string String.Table.t
  ; mutable client_capabilities : Types.ClientCapabilities.t option
  ; mutable state : State.t
  ; mutable encoding : Text.Encoding.t
  ; mutable refresh_semantic_tokens : bool
  ; mutable next_request_id : int
  }

let default_limits =
  { Limits.fuel = None
  ; max_depth = Some 100_000
  ; time_limit = Some (Time_ns.Span.of_int_sec 5)
  }
;;

let create ?stdlib ~limits ~send () =
  { limits
  ; stdlib
  ; send
  ; documents = String.Table.create ()
  ; paths = String.Table.create ()
  ; pending = String.Hash_set.create ()
  ; published = String.Table.create ()
  ; generations = String.Table.create ()
  ; covered = String.Table.create ()
  ; owners = String.Table.create ()
  ; client_capabilities = None
  ; state = Uninitialized
  ; encoding = Utf_16
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

let document_of_path t path =
  Option.bind (Hashtbl.find t.paths path) ~f:(Hashtbl.find t.documents)
;;

(* Symlinks resolved, as [Stdlib_checkout]'s are, so that a file has one
   path. A file that does not exist is in its directory's canonical path. *)
let canonical path =
  let realpath path = Option.try_with (fun () -> Filename_unix.realpath path) in
  match realpath path with
  | Some path -> path
  | None ->
    Option.value_map
      (realpath (Filename.dirname path))
      ~default:path
      ~f:(fun dir -> dir ^/ Filename.basename path)
;;

(* Documents without a file, eg. [untitled:Untitled-1], are named by a path
   of their own, eg. [/untitled_1.stag]. Only those of [file:] and
   [untitled:] are checked: others, eg. [git:], are views of files. *)
let path_of_uri t uri =
  let string = Lsp.Uri.to_string uri in
  if String.is_prefix string ~prefix:"file:"
  then canonical (Lsp.Uri.to_path uri), true
  else (
    let name =
      String.map
        (Lsp.Uri.to_path uri |> Filename.basename)
        ~f:(fun c -> if Char.is_alphanum c then Char.lowercase c else '_')
    in
    let name =
      if String.is_empty name || not (Char.is_alpha name.[0])
      then "untitled_" ^ name
      else name
    in
    let rec unique n =
      let path =
        sprintf "/%s%s.stag" name (if n = 1 then "" else sprintf "_%d" n)
      in
      if Hashtbl.mem t.paths path then unique (n + 1) else path
    in
    unique 1, String.is_prefix string ~prefix:"untitled:")
;;

let uri_of_path t path =
  match Hashtbl.find t.paths path with
  | Some uri -> Lsp.Uri.of_string uri
  | None -> Lsp.Uri.of_path path
;;

let read_file path =
  try In_channel.read_all path with
  | Sys_error _ -> ""
;;

let text_of_file t path =
  match document_of_path t path with
  | Some document -> document.text
  | None -> Text.create ~encoding:t.encoding (read_file path)
;;

let contents t path =
  Option.map (document_of_path t path) ~f:(fun document ->
    Text.contents document.text)
;;

let generation t path =
  Hashtbl.find t.generations path |> Option.value ~default:0
;;

let current_index t (document : Document.t) =
  let%bind.Option checked = document.checked in
  if
    Map.for_alli checked.generations ~f:(fun ~key ~data ->
      generation t key = data)
  then checked.index
  else None
;;

(* [path]'s contents changed: the checks of it are out of date. *)
let touch t path =
  Hashtbl.incr t.generations path;
  Hashtbl.iteri t.documents ~f:(fun ~key ~data:(document : Document.t) ->
    match document.checked with
    | Some { generations; _ } when Map.mem generations path ->
      Hash_set.add t.pending key
    | Some _ | None -> ())
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
        (Option.map (document_of_path t path) ~f:(fun document ->
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

(* The files that [uri]'s check reported on, and so whose diagnostics it
   published last, unless a later check also reported on them. *)
let cover t ~uri files =
  let previous =
    Hashtbl.find t.covered uri |> Option.value ~default:String.Set.empty
  in
  Set.iter (Set.diff previous files) ~f:(fun path ->
    if Option.equal String.equal (Hashtbl.find t.owners path) (Some uri)
    then (
      Hashtbl.remove t.owners path;
      publish t ~path []));
  Set.iter files ~f:(fun path -> Hashtbl.set t.owners ~key:path ~data:uri);
  Hashtbl.set t.covered ~key:uri ~data:files
;;

let check t ~uri (document : Document.t) =
  match
    Checker.check
      ?stdlib:t.stdlib
      ~limits:t.limits
      ~contents:(contents t)
      document.path
  with
  | exception exn ->
    eprintf "stag lsp: checking %s: %s\n%!" document.path (Exn.to_string exn)
  | result ->
    let generations =
      Set.union
        (Map.key_set result.diagnostics)
        (String.Set.of_list result.depends_on)
      |> Set.to_map ~f:(generation t)
    in
    Map.iter_keys result.diagnostics ~f:(fun path ->
      Option.iter (Hashtbl.find t.paths path) ~f:(fun uri ->
        Hash_set.remove t.pending uri;
        Hashtbl.change t.documents uri ~f:(fun document ->
          Option.map document ~f:(fun (document : Document.t) ->
            { document with
              checked = Some { index = result.index; generations }
            }))));
    Map.iteri result.diagnostics ~f:(fun ~key:path ~data ->
      publish t ~path data);
    cover t ~uri (Map.key_set result.diagnostics)
;;

let check_pending t =
  let checked_any = not (Hash_set.is_empty t.pending) in
  let rec loop () =
    match Hash_set.find t.pending ~f:(Fn.const true) with
    | None -> ()
    | Some uri ->
      Hash_set.remove t.pending uri;
      Option.iter (Hashtbl.find t.documents uri) ~f:(check t ~uri);
      loop ()
  in
  loop ();
  if checked_any && t.refresh_semantic_tokens
  then request t SemanticTokensRefresh
;;

let find_document t (text_document : Types.TextDocumentIdentifier.t) =
  Hashtbl.find t.documents (Lsp.Uri.to_string text_document.uri)
;;

let parse (document : Document.t) =
  Source_file.parse
    { path = document.path; contents = Text.contents document.text }
  |> Result.ok
;;

let semantic_tokens t (document : Document.t) =
  let index = current_index t document in
  let occurrences =
    match parse document with
    | Some toplevel -> Syntax_names.of_toplevels ~module_path:[] [ toplevel ]
    | None -> []
  in
  let tokens =
    Semantic_tokens.compute
      document.text
      ~file:document.path
      ~occurrences
      ~is_function:(fun offset ->
        match index with
        | Some index -> Index.is_function index ~file:document.path ~offset
        | None -> false)
  in
  Types.SemanticTokens.create ~data:(Semantic_tokens.encode tokens) ()
;;

let formatting (document : Document.t) =
  let contents = Text.contents document.text in
  let crlf =
    match String.index contents '\n' with
    | Some i -> i > 0 && Char.equal contents.[i - 1] '\r'
    | None -> false
  in
  match Format.file { path = document.path; contents } with
  | Error _ -> []
  | Ok formatted ->
    let formatted =
      if crlf
      then
        String.substr_replace_all formatted ~pattern:"\r\n" ~with_:"\n"
        |> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"
      else formatted
    in
    if String.equal formatted contents
    then []
    else
      [ Types.TextEdit.create
          ~newText:formatted
          ~range:(Text.full_range document.text)
      ]
;;

(* Checks first, so that the index is of the current contents. *)
let at_position t text_document position ~f =
  check_pending t;
  let%bind.Option document = find_document t text_document in
  let%bind.Option index = current_index t document in
  f
    document
    index
    ~file:document.path
    ~offset:(Text.offset_of_position document.text position)
;;

let hover t text_document position =
  at_position t text_document position ~f:(fun document index ~file ~offset ->
    let%map.Option { loc; text } = Index.hover index ~file ~offset in
    Types.Hover.create
      ~contents:
        (`MarkupContent (Types.MarkupContent.create ~kind:Markdown ~value:text))
      ~range:(Text.range_of_loc document.text loc)
      ())
;;

let definition t text_document position =
  at_position t text_document position ~f:(fun _ index ~file ~offset ->
    let%map.Option loc = Index.definition index ~file ~offset in
    `Location
      [ Types.Location.create
          ~uri:(uri_of_path t loc.file)
          ~range:(Text.range_of_loc (text_of_file t loc.file) loc)
      ])
;;

let document_symbols (document : Document.t) =
  let%map.Option toplevel = parse document in
  let toplevels =
    match toplevel.txt with
    | Module (_, toplevels) -> toplevels
    | _ -> [ toplevel ]
  in
  `DocumentSymbol (Document_symbols.of_toplevels document.text toplevels)
;;

let capabilities ~encoding =
  Types.ServerCapabilities.create
    ~positionEncoding:(Text.Encoding.to_lsp encoding)
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
  t.encoding
  <- Text.Encoding.negotiate
       ((let%bind.Option general = params.capabilities.general in
         general.positionEncodings)
        |> Option.value ~default:[]);
  Types.InitializeResult.create
    ~capabilities:(capabilities ~encoding:t.encoding)
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
      | Uninitialized, Initialize params ->
        t.client_capabilities <- Some params.capabilities;
        Ok (initialize t params)
      | (Running | Shut_down | Exited _), Initialize _ ->
        Error (InvalidRequest, "Already initialized")
      | Uninitialized, _ -> Error (ServerNotInitialized, "Not initialized")
      | (Shut_down | Exited _), _ -> Error (InvalidRequest, "Shut down")
      | Running, Shutdown ->
        t.state <- Shut_down;
        Ok ()
      | Running, TextDocumentFormatting params ->
        Ok (Option.map (find_document t params.textDocument) ~f:formatting)
      | Running, SemanticTokensFull params ->
        Ok
          (Option.map
             (find_document t params.textDocument)
             ~f:(semantic_tokens t))
      | Running, TextDocumentHover params ->
        Ok (hover t params.textDocument params.position)
      | Running, TextDocumentDefinition params ->
        Ok (definition t params.textDocument params.position)
      | Running, DocumentSymbol params ->
        Ok
          (Option.bind
             (find_document t params.textDocument)
             ~f:document_symbols)
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

let watch_files t =
  let dynamic =
    let%bind.Option capabilities = t.client_capabilities in
    let%bind.Option workspace = capabilities.workspace in
    let%bind.Option watched = workspace.didChangeWatchedFiles in
    watched.dynamicRegistration
  in
  if Option.value dynamic ~default:false
  then
    request
      t
      (ClientRegisterCapability
         (Types.RegistrationParams.create
            ~registrations:
              [ Types.Registration.create
                  ~id:"stag-files"
                  ~method_:"workspace/didChangeWatchedFiles"
                  ~registerOptions:
                    (Types.DidChangeWatchedFilesRegistrationOptions.yojson_of_t
                       (Types.DidChangeWatchedFilesRegistrationOptions.create
                          ~watchers:
                            (List.map
                               [ "**/*.stag"; "**/stag"; "**/stag-project" ]
                               ~f:(fun pattern ->
                                 Types.FileSystemWatcher.create
                                   ~globPattern:(`Pattern pattern)
                                   ()))))
                  ()
              ]))
;;

let open_document t ~uri ~version text =
  let key = Lsp.Uri.to_string uri in
  let previous = Hashtbl.find t.documents key in
  let path, checkable, checked, changed =
    match previous with
    | Some document ->
      ( document.path
      , document.checkable
      , document.checked
      , not (String.equal (Text.contents document.text) (Text.contents text)) )
    | None ->
      let path, checkable = path_of_uri t uri in
      Hashtbl.set t.paths ~key:path ~data:key;
      ( path
      , checkable
      , None
      , not (String.equal (read_file path) (Text.contents text)) )
  in
  Hashtbl.set t.documents ~key ~data:{ path; checkable; text; version; checked };
  if changed then touch t path;
  if checkable then Hash_set.add t.pending key
;;

let close_document t (text_document : Types.TextDocumentIdentifier.t) =
  let key = Lsp.Uri.to_string text_document.uri in
  Option.iter (Hashtbl.find t.documents key) ~f:(fun document ->
    Hashtbl.remove t.documents key;
    publish t ~path:document.path [];
    Hashtbl.remove t.paths document.path;
    Hash_set.remove t.pending key;
    if
      not (String.equal (read_file document.path) (Text.contents document.text))
    then touch t document.path;
    let covered =
      Hashtbl.find_and_remove t.covered key
      |> Option.value ~default:String.Set.empty
    in
    Set.iter covered ~f:(fun path ->
      let still_covered =
        Option.is_some (document_of_path t path)
        || Hashtbl.exists t.documents ~f:(fun (other : Document.t) ->
          match other.checked with
          | Some { generations; _ } -> Map.mem generations path
          | None -> false)
      in
      if not still_covered
      then (
        Hashtbl.remove t.owners path;
        publish t ~path [])))
;;

(* Files changed on disk, eg. by [git checkout], or project files: any
   check may be out of date. *)
let files_changed t (changes : Types.FileEvent.t list) =
  List.iter changes ~f:(fun change ->
    let path = canonical (Lsp.Uri.to_path change.uri) in
    if Option.is_none (document_of_path t path)
    then Hashtbl.incr t.generations path);
  Hashtbl.iteri t.documents ~f:(fun ~key ~data:(document : Document.t) ->
    if document.checkable then Hash_set.add t.pending key)
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
     | Initialized -> watch_files t
     | TextDocumentDidOpen { textDocument = { uri; version; text; _ } } ->
       open_document t ~uri ~version (Text.create ~encoding:t.encoding text)
     | TextDocumentDidChange { textDocument = { uri; version }; contentChanges }
       ->
       Option.iter
         (Hashtbl.find t.documents (Lsp.Uri.to_string uri))
         ~f:(fun document ->
           open_document
             t
             ~uri
             ~version
             (List.fold contentChanges ~init:document.text ~f:Text.apply_change))
     | DidSaveTextDocument { textDocument; _ } ->
       Option.iter (find_document t textDocument) ~f:(fun document ->
         if document.checkable
         then Hash_set.add t.pending (Lsp.Uri.to_string textDocument.uri))
     | TextDocumentDidClose { textDocument } -> close_document t textDocument
     | DidChangeWatchedFiles { changes } -> files_changed t changes
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
  let t =
    create
      ?stdlib:
        (Option.try_with (fun () ->
           Filename_unix.realpath Sys_unix.executable_name)
         |> Option.bind ~f:Stdlib_checkout.of_executable)
      ~limits
      ~send:(Transport.write transport)
      ()
  in
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
