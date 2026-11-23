open! Import

(** Calls C functions of the current process through libffi. *)

(** Calls [symbol], found by [dlsym] in the process, with [args], of the stag
    types of [signature]'s arguments. Errs if there is no such symbol. *)
val call
  :  symbol:string
  -> C_signature.t
  -> Value.t list
  -> (Value.t, string) Result.t
