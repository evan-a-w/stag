open! Import

(* A C function type, and how to call a function of it with stag values. *)
type fn =
  | Fn : ('a -> 'b) Ctypes.fn * (('a -> 'b) -> Value.t list -> Value.t) -> fn

type result_ = Result_ : 'r Ctypes.typ * ('r -> Value.t) -> result_
type arg = Arg : 'a Ctypes.typ * (Value.t -> 'a) -> arg

let bad_value value =
  raise_s [%message "C_call: an argument of the wrong type" (value : Value.t)]
;;

exception Out_of_range of Int64.t

let result_ : C_signature.Result.t -> result_ = function
  | Int ->
    Result_
      ( Ctypes.int64_t
      , fun n ->
          match Int64.to_int n with
          | Some n -> Value.Int n
          | None -> raise (Out_of_range n) )
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

module Key = struct
  module T = struct
    type t = string * C_signature.t [@@deriving compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

type t = (Value.t list -> Value.t, string) Result.t Key.Table.t

let create () = Key.Table.create ()

let call t ~symbol (signature : C_signature.t) args =
  let resolved =
    Hashtbl.find_or_add t (symbol, signature) ~default:(fun () ->
      let (Fn (typ, call)) = fn signature.args signature.result in
      match Foreign.foreign symbol typ with
      | f -> Ok (call f)
      | exception Dl.DL_error _ -> Error "there is no such C symbol")
  in
  Result.bind resolved ~f:(fun call ->
    match call args with
    | value -> Ok value
    | exception Out_of_range n ->
      Error
        (sprintf
           !"it returned %{Int64}, which is out of the range of int (63 bits)"
           n))
;;
