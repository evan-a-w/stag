open! Core

let () = print_string (Rust_bindgen.generate (Ppx_rust_runtime.all ()) |> ok_exn)
