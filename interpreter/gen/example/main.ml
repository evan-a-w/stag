let () =
  print_string (Rust_bindgen.generate (Ppx_rust_runtime.all ()) |> Core.ok_exn)
;;
