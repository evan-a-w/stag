open! Import

(* A C function type, and how to call a function of it with stag values. *)
type fn =
  | Fn : ('a -> 'b) Ctypes.fn * (('a -> 'b) -> Value.t list -> Value.t) -> fn

type result_ = Result_ : 'r Ctypes.typ * ('r -> Value.t) -> result_
type arg = Arg : 'a Ctypes.typ * (Value.t -> 'a) -> arg

let bad_value value =
  raise_s [%message "C_call: an argument of the wrong type" (value : Value.t)]
;;

let result_ : C_signature.Result.t -> result_ = function
  | Int -> Result_ (Ctypes.int64_t, fun n -> Value.Int (Int64.to_int_trunc n))
  | Bool -> Result_ (Ctypes.bool, fun b -> Value.Bool b)
  | Char -> Result_ (Ctypes.char, fun c -> Value.Char c)
  | Unit -> Result_ (Ctypes.void, fun () -> Value.Unit)
;;

let arg : C_signature.Arg.t -> arg = function
  | Int ->
    Arg
      ( Ctypes.int64_t
      , function
        | Int n -> Int64.of_int n
        | value -> bad_value value )
  | Bool ->
    Arg
      ( Ctypes.bool
      , function
        | Bool b -> b
        | value -> bad_value value )
  | Char ->
    Arg
      ( Ctypes.char
      , function
        | Char c -> c
        | value -> bad_value value )
  | String ->
    Arg
      ( Ctypes.string
      , function
        | String s -> s
        | value -> bad_value value )
;;

let wrong_count () = raise_s [%message "C_call: the wrong number of arguments"]

let rec fn (args : C_signature.Arg.t list) result : fn =
  match args with
  | [] ->
    let (Result_ (typ, of_c)) = result_ result in
    Fn
      ( Ctypes.(void @-> returning typ)
      , fun f -> function
          | [ Value.Unit ] -> of_c (f ())
          | _ -> wrong_count () )
  | [ only ] ->
    let (Arg (arg_typ, to_c)) = arg only in
    let (Result_ (typ, of_c)) = result_ result in
    Fn
      ( Ctypes.(arg_typ @-> returning typ)
      , fun f -> function
          | [ value ] -> of_c (f (to_c value))
          | _ -> wrong_count () )
  | first :: rest ->
    let (Arg (arg_typ, to_c)) = arg first in
    let (Fn (rest_typ, call)) = fn rest result in
    Fn
      ( Ctypes.(arg_typ @-> rest_typ)
      , fun f -> function
          | value :: values -> call (f (to_c value)) values
          | [] -> wrong_count () )
;;

let functions = Hashtbl.Poly.create ()

let call ~symbol (signature : C_signature.t) args =
  let resolved =
    Hashtbl.find_or_add functions (symbol, signature) ~default:(fun () ->
      let (Fn (typ, call)) = fn signature.args signature.result in
      match Foreign.foreign symbol typ with
      | f -> Ok (call f)
      | exception Dl.DL_error _ -> Error "there is no such C symbol")
  in
  Result.map resolved ~f:(fun call -> call args)
;;
