open! Import

(** Calls C functions of the current process through libffi. *)

(** The C functions resolved so far, and the symbols found not to exist. *)
type t

val create : unit -> t

(** Calls [symbol], found by [dlsym] in the process, with [args], of the stag
    types of [signature]'s arguments, after flushing stdout and stderr. Errs,
    saying why, if there is no such symbol, if an argument is out of the range
    of its C integer type or is a string with a NUL character, or if it returns
    a value out of the range of [int]. *)
val call
  :  t
  -> symbol:string
  -> C_signature.t
  -> Value.t list
  -> (Value.t, string) Result.t
