open! Import

let temp_file contents =
  let path = Filename_unix.temp_file "stag_lsp" "" in
  Out_channel.write_all path ~data:contents;
  path
;;

(* Runs [f] over a transport reading [input] from a file, returning what it
   wrote. *)
let with_input ?read_size input ~f =
  let input_path = temp_file input in
  let output_path = temp_file "" in
  let input = Core_unix.openfile input_path ~mode:[ O_RDONLY ] in
  let output = Core_unix.openfile output_path ~mode:[ O_WRONLY ] in
  let result =
    Exn.protect
      ~f:(fun () -> f (Transport.create ?read_size ~input ~output ()))
      ~finally:(fun () ->
        Core_unix.close input;
        Core_unix.close output)
  in
  let written = In_channel.read_all output_path in
  Sys_unix.remove input_path;
  Sys_unix.remove output_path;
  result, written
;;

(* The bodies of framed messages, one per line, with initialize's result
   elided. *)
let print_frames output =
  let rec loop output =
    if not (String.is_empty output)
    then (
      match String.substr_index output ~pattern:"\r\n\r\n" with
      | None -> printf "unframed: %S\n" output
      | Some index ->
        let length =
          String.chop_prefix_exn
            (String.prefix output index)
            ~prefix:"Content-Length: "
          |> Int.of_string
        in
        let body = String.sub output ~pos:(index + 4) ~len:length in
        let json =
          match Yojson.Safe.from_string body with
          | `Assoc fields
            when List.Assoc.mem fields ~equal:String.equal "result"
                 &&
                 match
                   List.Assoc.find_exn fields ~equal:String.equal "result"
                 with
                 | `Assoc result ->
                   List.Assoc.mem result ~equal:String.equal "capabilities"
                 | _ -> false ->
            `Assoc
              (List.Assoc.add
                 fields
                 ~equal:String.equal
                 "result"
                 (`String "<initialize result>"))
          | json -> json
        in
        print_endline (Yojson.Safe.to_string json);
        loop (String.drop_prefix output (index + 4 + length)))
  in
  loop output
;;

let frame body =
  sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body
;;

let request id method_ =
  Transport.frame (Request (Jsonrpc.Request.create ~id:(`Int id) ~method_ ()))
;;

let read_all transport =
  let output = Buffer.create 0 in
  let rec loop () =
    let pending = Transport.has_pending_input transport in
    match Transport.read transport with
    | None -> bprintf output "%b end\n" pending
    | Some (Ok packet) ->
      bprintf
        output
        "%b %s\n"
        pending
        (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet));
      loop ()
    | Some (Error message) ->
      bprintf output "%b error: %s\n" pending message;
      loop ()
  in
  loop ();
  Buffer.contents output
;;

(* [f] over [input], and what it wrote, the same for each read size. *)
let print_for_read_sizes input ~f =
  let outputs =
    List.map [ 1; 7; 65536 ] ~f:(fun read_size ->
      with_input ~read_size input ~f)
  in
  match List.dedup_and_sort outputs ~compare:[%compare: string * string] with
  | [ (output, written) ] ->
    print_string output;
    print_endline "written:";
    print_frames written
  | _ -> print_endline "differs by read size"
;;

let%expect_test "framing and invalid messages" =
  print_for_read_sizes
    (String.concat
       [ request 1 "a"
       ; "Content-Type: x\r\ncontent-length:  2 \r\n\r\n{}"
       ; frame "nope!"
       ; frame {|{"jsonrpc":"2.0","id":3,"params":[1]}|}
       ; frame {|{"jsonrpc":"2.0","id":4,"method":"m","params":1}|}
       ; frame {|{"jsonrpc":"2.0","id":5,"method":"m","params":null}|}
       ; frame {|{"jsonrpc":"2.0","method":"n","params":null}|}
       ; "Content-Type: x\r\n\r\n"
       ; request 6 "é"
       ; "Content-Length: 10\r\n\r\n{\"trunc"
       ])
    ~f:read_all;
  [%expect
    {|
    true {"id":1,"method":"a","jsonrpc":"2.0"}
    true error: ("Jsonrpc: json conversion failed: missing field jsonrpc")
    true error: Line 1, bytes 0-5:
    Invalid token 'nope!'
    true error: ("Jsonrpc: json conversion failed: missing field error")
    true error: ("Jsonrpc: json conversion failed: invalid structured value")
    true {"id":5,"method":"m","jsonrpc":"2.0"}
    true {"method":"n","jsonrpc":"2.0"}
    true error: a message without a Content-Length header
    true {"id":6,"method":"é","jsonrpc":"2.0"}
    true end
    written:
    {"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"(\"Jsonrpc: json conversion failed: missing field jsonrpc\")"}}
    {"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Line 1, bytes 0-5:\nInvalid token 'nope!'"}}
    {"jsonrpc":"2.0","id":3,"error":{"code":-32600,"message":"(\"Jsonrpc: json conversion failed: missing field error\")"}}
    {"jsonrpc":"2.0","id":4,"error":{"code":-32600,"message":"(\"Jsonrpc: json conversion failed: invalid structured value\")"}}
    |}]
;;

let%expect_test "a large message, and many small ones" =
  let large = String.make 3_000_000 'x' in
  let input =
    String.concat
      (Transport.frame
         (Notification
            (Jsonrpc.Notification.create
               ~method_:"large"
               ~params:(`List [ `String large ])
               ()))
       :: List.init 20_000 ~f:(fun id -> request id "small"))
  in
  let (), _ =
    with_input input ~f:(fun transport ->
      let rec loop ~large ~small =
        match Transport.read transport with
        | None -> print_s [%message (large : int) (small : int)]
        | Some (Ok (Notification { params = Some (`List [ `String s ]); _ })) ->
          loop ~large:(String.length s) ~small
        | Some (Ok (Request _)) -> loop ~large ~small:(small + 1)
        | Some _ -> print_endline "unexpected"
      in
      loop ~large:0 ~small:0)
  in
  [%expect {| ((large 3000000) (small 20000)) |}]
;;

let initialize =
  Transport.frame
    (Request
       (Jsonrpc.Request.create
          ~id:(`Int 1)
          ~method_:"initialize"
          ~params:(`Assoc [ "capabilities", `Assoc [] ])
          ()))
;;

let did_open uri text =
  Transport.frame
    (Notification
       (Jsonrpc.Notification.create
          ~method_:"textDocument/didOpen"
          ~params:
            (`Assoc
                [ ( "textDocument"
                  , `Assoc
                      [ "uri", `String uri
                      ; "languageId", `String "stag"
                      ; "version", `Int 1
                      ; "text", `String text
                      ] )
                ])
          ()))
;;

let hover id uri =
  Transport.frame
    (Request
       (Jsonrpc.Request.create
          ~id:(`Int id)
          ~method_:"textDocument/hover"
          ~params:
            (`Assoc
                [ "textDocument", `Assoc [ "uri", `String uri ]
                ; "position", `Assoc [ "line", `Int 0; "character", `Int 4 ]
                ])
          ()))
;;

let notification method_ =
  Transport.frame (Notification (Jsonrpc.Notification.create ~method_ ()))
;;

let%expect_test "serving until exit" =
  print_for_read_sizes
    (String.concat
       [ initialize
       ; notification "initialized"
       ; frame "{"
       ; frame {|{"jsonrpc":"2.0","id":2}|}
       ; did_open "untitled:a" "let x = Int.add 1 \"s\"\n"
       ; hover 3 "untitled:a"
       ; frame {|{"jsonrpc":"2.0","id":4,"method":"shutdown","params":null}|}
       ; frame {|{"jsonrpc":"2.0","method":"exit","params":null}|}
       ; request 5 "after exit"
       ])
    ~f:(fun transport ->
      sprintf
        "code %d\n"
        (Server.run
           ~debounce:Time_ns.Span.zero
           ~limits:Server.default_limits
           transport));
  [%expect
    {|
    stag lsp: Line 1, bytes 0-1:
    Unexpected end of input
    stag lsp: ("Jsonrpc: json conversion failed: missing field error")
    stag lsp: Line 1, bytes 0-1:
    Unexpected end of input
    stag lsp: ("Jsonrpc: json conversion failed: missing field error")
    stag lsp: Line 1, bytes 0-1:
    Unexpected end of input
    stag lsp: ("Jsonrpc: json conversion failed: missing field error")
    code 0
    written:
    {"result":"<initialize result>","id":1,"jsonrpc":"2.0"}
    {"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Line 1, bytes 0-1:\nUnexpected end of input"}}
    {"jsonrpc":"2.0","id":2,"error":{"code":-32600,"message":"(\"Jsonrpc: json conversion failed: missing field error\")"}}
    {"params":{"diagnostics":[{"message":"This expression has type string but an expression was expected of type int","range":{"end":{"character":21,"line":0},"start":{"character":18,"line":0}},"relatedInformation":[],"severity":1,"source":"stag"}],"uri":"untitled:a","version":1},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"id":3,"jsonrpc":"2.0","result":{"contents":{"kind":"markdown","value":"```stag\nA.x : int\n```"},"range":{"end":{"character":5,"line":0},"start":{"character":4,"line":0}}}}
    {"id":4,"jsonrpc":"2.0","result":null}
    |}]
;;

(* Runs [f] in a child process with stdin, stdout and stderr pipes, returning
   what it wrote to stdout and stderr. *)
let in_child ~f =
  let stdout_read, stdout_write = Core_unix.pipe () in
  let stderr_read, stderr_write = Core_unix.pipe () in
  let stdin_read, stdin_write = Core_unix.pipe () in
  Out_channel.flush stdout;
  match Core_unix.fork () with
  | `In_the_child ->
    Core_unix.dup2 ~src:stdin_read ~dst:Core_unix.stdin ();
    Core_unix.dup2 ~src:stdout_write ~dst:Core_unix.stdout ();
    Core_unix.dup2 ~src:stderr_write ~dst:Core_unix.stderr ();
    f ();
    Out_channel.flush stdout;
    Core_unix.exit_immediately 0
  | `In_the_parent pid ->
    List.iter
      [ stdout_write; stderr_write; stdin_read; stdin_write ]
      ~f:Core_unix.close;
    let read fd = In_channel.input_all (Core_unix.in_channel_of_descr fd) in
    let stdout = read stdout_read in
    let stderr = read stderr_read in
    Core_unix.close stdout_read;
    Core_unix.close stderr_read;
    Core_unix.waitpid_exn pid;
    stdout, stderr
;;

let%expect_test "what else is written to stdout goes to stderr" =
  let stdout, stderr =
    in_child ~f:(fun () ->
      let transport = Transport.of_stdio () in
      print_string "stray print\n";
      Out_channel.flush stdout;
      ignore
        (Core_unix.write_substring Core_unix.stdout ~buf:"stray write\n" : int);
      Transport.write transport (Response (Jsonrpc.Response.ok (`Int 1) `Null)))
  in
  print_frames stdout;
  print_endline "stderr:";
  print_string stderr;
  [%expect
    {|
    {"id":1,"jsonrpc":"2.0","result":null}
    stderr:
    stray print
    stray write
    |}]
;;

(* Writes [pieces] to a pipe from a child process, pausing between them. *)
let with_writer pieces ~f =
  let read_end, write_end = Core_unix.pipe () in
  let output_path = temp_file "" in
  let output = Core_unix.openfile output_path ~mode:[ O_WRONLY ] in
  Out_channel.flush stdout;
  match Core_unix.fork () with
  | `In_the_child ->
    Core_unix.close read_end;
    List.iter pieces ~f:(function
      | `Write data ->
        ignore (Core_unix.write_substring write_end ~buf:data : int)
      | `Pause span ->
        ignore (Core_unix.nanosleep (Time_ns.Span.to_sec span) : float));
    Core_unix.exit_immediately 0
  | `In_the_parent pid ->
    Core_unix.close write_end;
    let result = f (Transport.create ~input:read_end ~output ()) in
    Core_unix.waitpid_exn pid;
    Core_unix.close read_end;
    Core_unix.close output;
    let written = In_channel.read_all output_path in
    Sys_unix.remove output_path;
    result, written
;;

let%expect_test "checks wait until the client is idle for the debounce" =
  let uri = "untitled:a" in
  let tokens id =
    Transport.frame
      (Request
         (Jsonrpc.Request.create
            ~id:(`Int id)
            ~method_:"textDocument/documentSymbol"
            ~params:(`Assoc [ "textDocument", `Assoc [ "uri", `String uri ] ])
            ()))
  in
  let (), written =
    with_writer
      [ `Write (initialize ^ notification "initialized")
      ; `Write (did_open uri "let x = Int.add 1 \"s\"\n")
      ; `Pause (Time_ns.Span.of_int_ms 50)
      ; `Write (tokens 2)
      ; `Pause (Time_ns.Span.of_int_sec 2)
      ; `Write (request 3 "shutdown" ^ notification "exit")
      ]
      ~f:(fun transport ->
        let code =
          Server.run
            ~debounce:(Time_ns.Span.of_int_ms 500)
            ~limits:Server.default_limits
            transport
        in
        print_s [%message (code : int)])
  in
  print_frames written;
  [%expect
    {|
    (code 0)
    {"result":"<initialize result>","id":1,"jsonrpc":"2.0"}
    {"id":2,"jsonrpc":"2.0","result":[{"children":[],"kind":13,"name":"x","range":{"end":{"character":21,"line":0},"start":{"character":0,"line":0}},"selectionRange":{"end":{"character":5,"line":0},"start":{"character":4,"line":0}}}]}
    {"params":{"diagnostics":[{"message":"This expression has type string but an expression was expected of type int","range":{"end":{"character":21,"line":0},"start":{"character":18,"line":0}},"relatedInformation":[],"severity":1,"source":"stag"}],"uri":"untitled:a","version":1},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"id":3,"jsonrpc":"2.0","result":null}
    |}]
;;
