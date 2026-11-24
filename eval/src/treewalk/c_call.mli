open! Import

(** Calls C functions of the current process through libffi. *)

(** Calls [symbol], found by [dlsym] in the process, with [args], of the stag
    types of [signature]'s arguments. Errs, saying why, if there is no such
    symbol, or if it returns an [int64_t] out of the range of [int]. *)
val call
  :  symbol:string
  -> C_signature.t
  -> Value.t list
  -> (Value.t, string) Result.t
