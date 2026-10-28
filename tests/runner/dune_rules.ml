let () =
  Array.to_list Sys.argv
  |> List.tl
  |> List.map Filename.basename
  |> List.map (fun file ->
    Printf.sprintf
      {|(rule
 (deps ../%s)
 (action
  (with-stdout-to
   %s.corrected
   (chdir
    ..
    (run %%{bin:stag} test -print-corrected -fuel 100000000 -time-limit 120s -max-depth 100000 %s)))))

(rule
 (alias runtest)
 (action
  (diff ../%s %s.corrected)))
|}
      file
      file
      file
      file
      file)
  |> String.concat "\n"
  |> print_string
;;
