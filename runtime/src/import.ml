include Core
module Native_pointer = Ocaml_intrinsics.Native_pointer
module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
module Float_u = Stdlib_upstream_compatible.Float_u
module Float32_u = Stdlib_stable.Float32_u

let round_up n ~align = (n + align - 1) / align * align
