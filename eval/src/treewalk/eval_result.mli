open! Import

(** The evaluator's results: returned on the stack, with [exclave_], their
    contents global, to be stored. *)
type 'a t = ('a Global.t, Error.t Global.t) Local_result.t

val return : 'a -> 'a t @ local
val error : Error.t -> 'a t @ local
val error_s : Sexp.t -> 'a t @ local
val of_or_error : 'a Or_error.t -> 'a t @ local
val to_or_error : 'a t @ local -> 'a Or_error.t

module List : sig
  (** Stops at the first error. *)
  val map : 'a list -> f:('a -> 'b t @ local) @ local -> 'b list t @ local

  (** Stops at the first error. *)
  val iter : 'a list -> f:('a -> unit t @ local) @ local -> unit t @ local
end
