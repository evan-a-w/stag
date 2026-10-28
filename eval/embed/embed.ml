(* Prints an OCaml module binding [files] to the basenames and contents of the
   files given, sorted by basename, for embedding them in a library. *)

let () =
  let files =
    Array.to_list Sys.argv
    |> List.tl
    |> List.map (fun path ->
      Filename.basename path, In_channel.with_open_bin path In_channel.input_all)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  print_string "let files = [\n";
  List.iter
    (fun (name, contents) -> Printf.printf "  %S, %S;\n" name contents)
    files;
  print_string "]\n"
;;
