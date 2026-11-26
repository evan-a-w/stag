open! Import

type t =
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Int
  | Uint
  | Long
  | Ulong
  | Size_t
[@@deriving sexp_of, equal, compare, hash, enumerate]

let name = function
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Uint8 -> "uint8"
  | Uint16 -> "uint16"
  | Uint32 -> "uint32"
  | Uint64 -> "uint64"
  | Int -> "int"
  | Uint -> "uint"
  | Long -> "long"
  | Ulong -> "ulong"
  | Size_t -> "size_t"
;;

let by_type_name =
  String.Map.of_alist_exn
    (List.map all ~f:(fun t -> Prelude_path.canonical ("C." ^ name t), t))
;;

let of_type_name = Map.find by_type_name
