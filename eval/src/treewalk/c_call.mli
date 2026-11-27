open! Import

(** Calls C functions of the current process through libffi. *)

(** The C functions resolved so far, and the symbols found not to exist. *)
type t

val create : unit -> t

module Call_error : sig
  type t =
    | Sys_error of string (** Flushing the output before the call failed. *)
    | Failed of string (** Why the call couldn't be made, or its result used. *)
end

(** Calls [symbol], found by [dlsym] in the process, with [args], of the stag
    types of [signature]'s arguments, after flushing stdout and stderr. Errs
    with [Failed], saying why, if there is no such symbol, if an argument is out
    of the range of its C integer type or is a string with a NUL character, or
    if it returns a value out of the range of [int]. *)
val call
  :  t
  -> symbol:string
  -> C_signature.t
  -> Value.t list
  -> (Value.t, Call_error.t) Result.t
