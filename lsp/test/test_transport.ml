open! Import

let with_pipe input ~f =
  let read_end, write_end = Core_unix.pipe () in
  let output_read, output_write = Core_unix.pipe () in
  let written = Core_unix.write_substring write_end ~buf:input in
  assert (written = String.length input);
  Core_unix.close write_end;
  let transport = Transport.create ~input:read_end ~output:output_write in
  let result = f transport in
  Core_unix.close output_write;
  let output =
    In_channel.input_all (Core_unix.in_channel_of_descr output_read)
  in
  Core_unix.close read_end;
  Core_unix.close output_read;
  result, output
;;

let request id method_ =
  Transport.frame (Request (Jsonrpc.Request.create ~id:(`Int id) ~method_ ()))
;;

let%expect_test "framing" =
  let input =
    String.concat
      [ request 1 "a"
      ; "Content-Type: x\r\nContent-Length: 2\r\n\r\n{}"
      ; "Content-Length: 5\r\n\r\nnope!"
      ; request 2 "é"
      ; "Content-Length: 10\r\n\r\n{\"trunc"
      ]
  in
  let (), _ =
    with_pipe input ~f:(fun transport ->
      let rec loop () =
        print_s [%sexp (Transport.has_pending_input transport : bool)];
        match Transport.read transport with
        | None -> print_endline "end"
        | Some (Ok packet) ->
          print_endline
            (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet));
          loop ()
        | Some (Error message) ->
          print_endline ("error: " ^ message);
          loop ()
      in
      loop ())
  in
  [%expect
    {|
    true
    {"id":1,"method":"a","jsonrpc":"2.0"}
    true
    error: ("Jsonrpc: json conversion failed: missing field jsonrpc")
    true
    error: ("Yojson__Common.Json_error(\"Line 1, bytes 0-5:\\nInvalid token 'nope!'\")")
    true
    {"id":2,"method":"é","jsonrpc":"2.0"}
    true
    end
    |}]
;;

let%expect_test "serving over pipes, until exit" =
  let notification method_ =
    Transport.frame (Notification (Jsonrpc.Notification.create ~method_ ()))
  in
  let input =
    String.concat
      [ Transport.frame
          (Request
             (Jsonrpc.Request.create
                ~id:(`Int 1)
                ~method_:"initialize"
                ~params:(`Assoc [ "capabilities", `Assoc [] ])
                ()))
      ; request 2 "shutdown"
      ; notification "exit"
      ; request 3 "after exit"
      ]
  in
  let code, output =
    with_pipe input ~f:(Server.run ~limits:Server.default_limits)
  in
  print_s [%sexp (code : int)];
  String.split output ~on:'\n'
  |> List.iter ~f:(fun line ->
    print_endline (String.prefix (String.rstrip line) 100));
  [%expect
    {|
    0
    Content-Length: 546

    {"id":1,"jsonrpc":"2.0","result":{"capabilities":{"definitionProvider":true,"documentFormattingProvi

    {"id":2,"jsonrpc":"2.0","result":null}
    |}]
;;
