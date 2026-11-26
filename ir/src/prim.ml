open! Import

module Value = struct
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | String_list of string list
    | String_option of string option
  [@@deriving sexp_of]
end

module Exception = struct
  type t =
    | Division_by_zero
    | Invalid_argument of string
    | Failure of string
    | Sys_error of string
  [@@deriving sexp_of]
end

module Io = struct
  type t =
    { output : string -> unit
    ; argv : string list
    }
end

module Base_type = struct
  type t =
    | Int
    | Bool
    | String
    | Char
    | Unit
    | String_list
    | String_option

  let ty : t -> Type.ty = function
    | Int -> Type.int
    | Bool -> Type.bool
    | String -> Type.string
    | Char -> Type.char
    | Unit -> Type.unit
    | String_list -> TCon (Prelude_path.canonical "list", [ ATy Type.string ])
    | String_option ->
      TCon (Prelude_path.canonical "option", [ ATy Type.string ])
  ;;
end

type t =
  { name : string
  ; args : Base_type.t list
  ; result : Base_type.t
  ; run_time_only : bool
  ; impl : io:Io.t -> Value.t list -> (Value.t, Exception.t) Result.t
  }

let equal a b = String.equal a.name b.name
let sexp_of_t t = Sexp.Atom t.name
let args t = List.map t.args ~f:Base_type.ty
let result t = Base_type.ty t.result
let arity t = List.length t.args
let name t = t.name
let run_time_only t = t.run_time_only

let bad_args t args =
  raise_s
    [%message
      "Prim applied to ill-typed arguments" (t : t) (args : Value.t list)]
;;

let apply t ~io args =
  if List.length args <> arity t then bad_args t args;
  t.impl ~io args
;;

let prim ?(run_time_only = false) name args result impl =
  let rec t =
    { name
    ; args
    ; result
    ; run_time_only
    ; impl =
        (fun ~io args ->
          match impl ~io args with
          | Some result -> result
          | None -> bad_args t args)
    }
  in
  t
;;

let int_compare a b = Value.Int (Int.sign (Int.compare a b) |> Sign.to_int)

let int_binop name f =
  prim name [ Int; Int ] Int (fun ~io:_ -> function
    | [ Int a; Int b ] -> Some (Ok (Value.Int (f a b)))
    | _ -> None)
;;

let int_division name f =
  prim name [ Int; Int ] Int (fun ~io:_ -> function
    | [ Int _; Int 0 ] -> Some (Error Exception.Division_by_zero)
    | [ Int a; Int b ] -> Some (Ok (Value.Int (f a b)))
    | _ -> None)
;;

let int_relation name f =
  prim name [ Int; Int ] Bool (fun ~io:_ -> function
    | [ Int a; Int b ] -> Some (Ok (Value.Bool (f a b)))
    | _ -> None)
;;

let print name (arg : Base_type.t) to_string =
  prim name [ arg ] Unit (fun ~io -> function
    | [ value ] ->
      Option.map (to_string value) ~f:(fun string ->
        io.output string;
        Ok Value.Unit)
    | _ -> None)
;;

let unary name arg result f =
  prim name [ arg ] result (fun ~io:_ -> function
    | [ value ] -> f value
    | _ -> None)
;;

let binary name args result f =
  prim name args result (fun ~io:_ -> function
    | [ a; b ] -> f a b
    | _ -> None)
;;

let ok value = Some (Ok value)

let sys_error f =
  Some
    (match f () with
     | value -> Ok value
     | exception Sys_error message -> Error (Exception.Sys_error message))
;;

(* Flushes stdout first, so a terminal shows the program's output in order. *)
let prerr name to_string =
  prim name [ String ] Unit (fun ~io:_ -> function
    | [ String s ] ->
      Out_channel.flush stdout;
      Out_channel.output_string stderr (to_string s);
      Out_channel.flush stderr;
      ok Value.Unit
    | _ -> None)
;;

(* Reading stdin flushes stdout, so a prompt shows before the program waits. *)
let read_stdin name result read =
  prim ~run_time_only:true name [ Unit ] result (fun ~io:_ -> function
    | [ Unit ] ->
      sys_error (fun () ->
        Out_channel.flush stdout;
        read In_channel.stdin)
    | _ -> None)
;;

(* Base may return an argument itself (eg. [String.sub] of the whole string,
   [String.concat] of one part), but a prim's result must be new. *)
let fresh s = Bytes.to_string (Bytes.of_string s)

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
  ; prerr "prerr_string" Fn.id
  ; prerr "prerr_endline" (fun s -> s ^ "\n")
  ; prim "flush" [ Unit ] Unit (fun ~io:_ -> function
      | [ Unit ] ->
        Out_channel.flush stdout;
        Out_channel.flush stderr;
        ok Value.Unit
      | _ -> None)
  ; read_stdin "read_line" String_option (fun stdin ->
      Value.String_option (In_channel.input_line stdin))
  ; read_stdin "read_all_stdin" String (fun stdin ->
      Value.String (In_channel.input_all stdin))
  ; unary "read_file" String String (function
      | String path ->
        sys_error (fun () -> Value.String (In_channel.read_all path))
      | _ -> None)
  ; prim
      ~run_time_only:true
      "write_file"
      [ String; String ]
      Unit
      (fun ~io:_ -> function
      | [ String path; String data ] ->
        sys_error (fun () ->
          Out_channel.write_all path ~data;
          Value.Unit)
      | _ -> None)
  ; prim ~run_time_only:true "argv" [ Unit ] String_list (fun ~io -> function
      | [ Unit ] -> ok (Value.String_list (List.map io.argv ~f:fresh))
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
        ok (Value.String (fresh (String.concat ~sep parts)))
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
  ; prim "String.sub" [ String; Int; Int ] String (fun ~io:_ -> function
      | [ String s; Int pos; Int len ] ->
        Some
          (if pos >= 0 && len >= 0 && pos + len <= String.length s
           then Ok (Value.String (fresh (String.sub s ~pos ~len)))
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
