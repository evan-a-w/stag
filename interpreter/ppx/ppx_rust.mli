(** [[@@deriving rust]] on type declarations registers them with
    [Ppx_rust_runtime], for [Rust_bindgen] to generate Rust types and their
    conversions from OCaml values.

    Next to each declared [u] it defines [rust_u : u Ppx_rust_runtime.Witness.t]
    (and declares it, in an interface), by which the declarations deriving
    [rust] refer to [u]: OCaml resolves the names, and checks that [rust_u]
    is [u]'s. So a type referred to must derive [rust] too, and be exported
    with its [rust_u] where it is used from another module. Builtins ([int],
    [string], ['a list], ['a K.Map.t], [K.Set.t], [Nothing.t], ...) are
    checked to be the standard ones.

    [[@@rust.shared]] on a declaration converts each OCaml value of it once,
    into the arena of the conversion, and [[@rust.skip]] on a field leaves it
    out (eg. a function). *)
