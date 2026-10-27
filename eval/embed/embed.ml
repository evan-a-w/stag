(* Prints an OCaml module binding [source] to the contents of the file given,
   for embedding it in a library. *)

let () =
  let contents = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  Printf.printf "let source = %S\n" contents
;;
