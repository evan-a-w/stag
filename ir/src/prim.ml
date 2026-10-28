open! Import

module Value = struct
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | String_list of string list
  [@@deriving sexp_of]
end

module Exception = struct
  type t =
    | Division_by_zero
    | Invalid_argument of string
    | Failure of string
  [@@deriving sexp_of]
end

module Base_type = struct
  type t =
    | Int
    | Bool
    | String
    | Char
    | Unit
    | String_list

  let ty : t -> Type.ty = function
    | Int -> Type.int
    | Bool -> Type.bool
    | String -> Type.string
    | Char -> Type.char
    | Unit -> Type.unit
    | String_list -> TCon (Prelude_path.canonical "list", [ ATy Type.string ])
  ;;
end

type t =
  { name : string
  ; args : Base_type.t list
  ; result : Base_type.t
  ; impl :
      output:(string -> unit) -> Value.t list -> (Value.t, Exception.t) Result.t
  }

let equal a b = String.equal a.name b.name
let sexp_of_t t = Sexp.Atom t.name

let ty t =
  List.fold_right t.args ~init:(Base_type.ty t.result) ~f:(fun arg res ->
    Type.TArrow (Base_type.ty arg, res))
;;

let arity t = List.length t.args

let bad_args t args =
  raise_s
    [%message
      "Prim applied to ill-typed arguments" (t : t) (args : Value.t list)]
;;

let apply t ~output args =
  if List.length args <> arity t then bad_args t args;
  t.impl ~output args
;;

let prim name args result impl =
  let rec t =
    { name
    ; args
    ; result
    ; impl =
        (fun ~output args ->
          match impl ~output args with
          | Some result -> result
          | None -> bad_args t args)
    }
  in
  t
;;

let int_compare a b = Value.Int (Int.sign (Int.compare a b) |> Sign.to_int)

let int_binop name f =
  prim name [ Int; Int ] Int (fun ~output:_ -> function
    | [ Int a; Int b ] -> Some (Ok (Value.Int (f a b)))
    | _ -> None)
;;

let int_division name f =
  prim name [ Int; Int ] Int (fun ~output:_ -> function
    | [ Int _; Int 0 ] -> Some (Error Exception.Division_by_zero)
    | [ Int a; Int b ] -> Some (Ok (Value.Int (f a b)))
    | _ -> None)
;;

let int_relation name f =
  prim name [ Int; Int ] Bool (fun ~output:_ -> function
    | [ Int a; Int b ] -> Some (Ok (Value.Bool (f a b)))
    | _ -> None)
;;

let print name (arg : Base_type.t) to_string =
  prim name [ arg ] Unit (fun ~output -> function
    | [ value ] ->
      Option.map (to_string value) ~f:(fun string ->
        output string;
        Ok Value.Unit)
    | _ -> None)
;;

let unary name arg result f =
  prim name [ arg ] result (fun ~output:_ -> function
    | [ value ] -> f value
    | _ -> None)
;;

let binary name args result f =
  prim name args result (fun ~output:_ -> function
    | [ a; b ] -> f a b
    | _ -> None)
;;

let ok value = Some (Ok value)

(* Only an optional minus and decimal digits, unlike OCaml's [int_of_string]. *)
let int_of_string s =
  let digits = String.chop_prefix s ~prefix:"-" |> Option.value ~default:s in
  if (not (String.is_empty digits)) && String.for_all digits ~f:Char.is_digit
  then Int.of_string_opt s
  else None
;;

let all =
  [ print "print_string" String (function
      | String s -> Some s
      | _ -> None)
  ; print "print_endline" String (function
      | String s -> Some (s ^ "\n")
      | _ -> None)
  ; print "print_int" Int (function
      | Int n -> Some (Int.to_string n)
      | _ -> None)
  ; print "print_char" Char (function
      | Char c -> Some (String.of_char c)
      | _ -> None)
  ; int_binop "Int.add" ( + )
  ; int_binop "Int.sub" ( - )
  ; int_binop "Int.mul" ( * )
  ; int_division "Int.div" ( / )
  ; int_division "Int.rem" Int.rem
  ; unary "Int.neg" Int Int (function
      | Int n -> ok (Value.Int (-n))
      | _ -> None)
  ; int_relation "Int.equal" ( = )
  ; binary "Int.compare" [ Int; Int ] Int (fun a b ->
      match a, b with
      | Int a, Int b -> ok (int_compare a b)
      | _ -> None)
  ; int_relation "Int.lt" ( < )
  ; int_relation "Int.le" ( <= )
  ; int_relation "Int.gt" ( > )
  ; int_relation "Int.ge" ( >= )
  ; unary "Int.to_string" Int String (function
      | Int n -> ok (Value.String (Int.to_string n))
      | _ -> None)
  ; unary "Int.of_string_exn" String Int (function
      | String s ->
        Some
          (match int_of_string s with
           | Some n -> Ok (Value.Int n)
           | None ->
             Error (Failure (sprintf "Int.of_string_exn: invalid integer %S" s)))
      | _ -> None)
  ; binary "String.concat" [ String; String_list ] String (fun a b ->
      match a, b with
      | String sep, String_list parts ->
        ok (Value.String (String.concat ~sep parts))
      | _ -> None)
  ; binary "String.append" [ String; String ] String (fun a b ->
      match a, b with
      | String a, String b -> ok (Value.String (a ^ b))
      | _ -> None)
  ; unary "String.length" String Int (function
      | String s -> ok (Value.Int (String.length s))
      | _ -> None)
  ; binary "String.get" [ String; Int ] Char (fun a b ->
      match a, b with
      | String s, Int i ->
        Some
          (if i >= 0 && i < String.length s
           then Ok (Value.Char s.[i])
           else
             Error
               (Invalid_argument
                  (sprintf
                     "String.get: index %d out of bounds of a string of length \
                      %d"
                     i
                     (String.length s))))
      | _ -> None)
  ; prim "String.sub" [ String; Int; Int ] String (fun ~output:_ -> function
      | [ String s; Int pos; Int len ] ->
        Some
          (if pos >= 0 && len >= 0 && pos + len <= String.length s
           then Ok (Value.String (String.sub s ~pos ~len))
           else
             Error
               (Invalid_argument
                  (sprintf
                     "String.sub: invalid substring (position %d, length %d) \
                      of a string of length %d"
                     pos
                     len
                     (String.length s))))
      | _ -> None)
  ; binary "String.equal" [ String; String ] Bool (fun a b ->
      match a, b with
      | String a, String b -> ok (Value.Bool (String.equal a b))
      | _ -> None)
  ; binary "String.compare" [ String; String ] Int (fun a b ->
      match a, b with
      | String a, String b ->
        ok (Value.Int (Int.sign (String.compare a b) |> Sign.to_int))
      | _ -> None)
  ; binary "Char.equal" [ Char; Char ] Bool (fun a b ->
      match a, b with
      | Char a, Char b -> ok (Value.Bool (Char.equal a b))
      | _ -> None)
  ; unary "Char.code" Char Int (function
      | Char c -> ok (Value.Int (Char.to_int c))
      | _ -> None)
  ; unary "Char.of_code" Int Char (function
      | Int n ->
        Some
          (match Char.of_int n with
           | Some c -> Ok (Value.Char c)
           | None ->
             Error
               (Invalid_argument
                  (sprintf "Char.of_code: invalid character code %d" n)))
      | _ -> None)
  ; unary "Char.to_string" Char String (function
      | Char c -> ok (Value.String (String.of_char c))
      | _ -> None)
  ; unary "Bool.not" Bool Bool (function
      | Bool b -> ok (Value.Bool (not b))
      | _ -> None)
  ; binary "Bool.equal" [ Bool; Bool ] Bool (fun a b ->
      match a, b with
      | Bool a, Bool b -> ok (Value.Bool (Bool.equal a b))
      | _ -> None)
  ]
;;

let by_name = String.Map.of_alist_exn (List.map all ~f:(fun t -> t.name, t))
let find path = Map.find by_name (Syntax.Path.to_string path)
let canonical_name t = Prelude_path.canonical (Prelude_path.prim_prefix ^ t.name)

let of_canonical_name name =
  let%bind.Option relative = Prelude_path.relative name in
  let%bind.Option name =
    String.chop_prefix relative ~prefix:Prelude_path.prim_prefix
  in
  Map.find by_name name
;;

let modules =
  List.filter_map all ~f:(fun t ->
    match String.lsplit2 t.name ~on:'.' with
    | Some (module_, _) -> Some module_
    | None -> None)
  |> String.Set.of_list
;;

let is_module name = Set.mem modules name
