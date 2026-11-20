open! Import

(** [Stag_eval.Runtime_error], specialised to raising [Value.t]s. *)

include module type of struct
  include Stag_eval.Runtime_error
end

val raise_exn : loc:Loc.t -> Value.t -> t

(** The exception an error raises, if it is one. *)
val raised : Error.t -> Value.t option
