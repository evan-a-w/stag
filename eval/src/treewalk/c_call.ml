open! Import

(* A C function type, and how to call a function of it with stag values. *)
type fn =
  | Fn : ('a -> 'b) Ctypes.fn * (('a -> 'b) -> Value.t list -> Value.t) -> fn

type result_ = Result_ : 'r Ctypes.typ * ('r -> Value.t) -> result_
type arg = Arg : 'a Ctypes.typ * (Value.t -> 'a) -> arg

let bad_value value =
  raise_s [%message "C_call: an argument of the wrong type" (value : Value.t)]
;;

(* Of a result: the C value, printed. *)
exception Out_of_range of string

exception
  Argument_out_of_range of
    { value : int
    ; c_int : C_int.t
    }

exception Nul_in_string of string

(* A C integer type as stag ints: [to_int] is [None] out of the range of [int]. *)
type c_int =
  | C_int :
      { typ : 'a Ctypes.typ
      ; signed : bool
      ; of_int : int -> 'a
      ; to_int : 'a -> int option
      ; to_string : 'a -> string
      }
      -> c_int

let unsigned (type a) typ (module U : Unsigned.S with type t = a) =
  C_int
    { typ
    ; signed = false
    ; of_int = (fun n -> U.of_int64 (Int64.of_int n))
    ; to_int =
        (fun n ->
          let n = U.to_int64 n in
          if Int64.(n < zero) then None else Int64.to_int n)
    ; to_string = U.to_string
    }
;;

let small_signed typ =
  C_int
    { typ
    ; signed = true
    ; of_int = Fn.id
    ; to_int = Option.some
    ; to_string = Int.to_string
    }
;;

let c_int : C_int.t -> c_int = function
  | Int8 -> small_signed Ctypes.int8_t
  | Int16 -> small_signed Ctypes.int16_t
  | Int -> small_signed Ctypes.int
  | Int32 ->
    C_int
      { typ = Ctypes.int32_t
      ; signed = true
      ; of_int = Int32.of_int_trunc
      ; to_int = (fun n -> Some (Int32.to_int_exn n))
      ; to_string = Int32.to_string
      }
  | Int64 ->
    C_int
      { typ = Ctypes.int64_t
      ; signed = true
      ; of_int = Int64.of_int
      ; to_int = Int64.to_int
      ; to_string = Int64.to_string
      }
  | Long ->
    C_int
      { typ = Ctypes.long
      ; signed = true
      ; of_int = Signed.Long.of_int
      ; to_int = (fun n -> Int64.to_int (Signed.Long.to_int64 n))
      ; to_string = Signed.Long.to_string
      }
  | Uint8 -> unsigned Ctypes.uint8_t (module Unsigned.UInt8)
  | Uint16 -> unsigned Ctypes.uint16_t (module Unsigned.UInt16)
  | Uint32 -> unsigned Ctypes.uint32_t (module Unsigned.UInt32)
  | Uint64 -> unsigned Ctypes.uint64_t (module Unsigned.UInt64)
  | Uint -> unsigned Ctypes.uint (module Unsigned.UInt)
  | Ulong -> unsigned Ctypes.ulong (module Unsigned.ULong)
  | Size_t -> unsigned Ctypes.size_t (module Unsigned.Size_t)
;;

(* Whether [n] is in the range of a C integer type of [bits] bits. *)
let in_range n ~bits ~signed =
  match signed with
  | true ->
    bits >= Int.num_bits || (n >= -(1 lsl (bits - 1)) && n < 1 lsl (bits - 1))
  | false -> n >= 0 && (bits >= Int.num_bits || n < 1 lsl bits)
;;

let result_ : C_signature.Result.t -> result_ = function
  | Int ->
    Result_
      ( Ctypes.int64_t
      , fun n ->
          match Int64.to_int n with
          | Some n -> Value.Int n
          | None -> raise (Out_of_range (Int64.to_string n)) )
  | Bool -> Result_ (Ctypes.bool, fun b -> Value.Bool b)
  | Char -> Result_ (Ctypes.char, fun c -> Value.Char c)
  | Unit -> Result_ (Ctypes.void, fun () -> Value.Unit)
  | C_int kind ->
    let (C_int { typ; to_int; to_string; _ }) = c_int kind in
    Result_
      ( typ
      , fun n ->
          match to_int n with
          | Some n -> Value.Int n
          | None -> raise (Out_of_range (to_string n)) )
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
        | String s when String.mem s '\000' -> raise (Nul_in_string s)
        | String s -> s
        | value -> bad_value value )
  | C_int kind ->
    let (C_int { typ; signed; of_int; _ }) = c_int kind in
    let bits = 8 * Ctypes.sizeof typ in
    Arg
      ( typ
      , function
        | Int n when in_range n ~bits ~signed -> of_int n
        | Int value -> raise (Argument_out_of_range { value; c_int = kind })
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
    (* C may write to the same file descriptors, or exit. *)
    Out_channel.flush stdout;
    Out_channel.flush stderr;
    match call args with
    | value -> Ok value
    | exception Out_of_range n ->
      Error
        (sprintf "it returned %s, which is out of the range of int (63 bits)" n)
    | exception Argument_out_of_range { value; c_int } ->
      Error
        (sprintf
           "the argument %d is out of the range of C.%s"
           value
           (C_int.name c_int))
    | exception Nul_in_string s ->
      Error
        (sprintf
           "the string argument %S has a NUL character, so can't be passed to C"
           s))
;;
