open! Core

let () =
  print_string
    (Rust_bindgen.generate
       ~rebuilt:(Ppx_rust_runtime.rebuilt ())
       (Ppx_rust_runtime.all ())
     |> ok_exn)
;;
