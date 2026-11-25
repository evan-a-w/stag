open! Import

(** Checking a program's sources, as [stag], [stag-build] and the language
    server do. *)

module Checked : sig
  type t =
    { groups : Driver.Module_group.t list option
    (** The program's modules, if it parsed (as far as it did). *)
    ; checked : Typecheck.Checked.t option
    (** What checked of [groups], which with errors is
        [Typecheck.Failed.partial]. *)
    ; driver : Driver.t option (** If there are no [diagnostics]. *)
    ; diagnostics : Diagnostic.t list (** Parse and type errors, by location. *)
    }

  val to_result : t -> (Driver.t, Diagnostic.t list) Result.t
end

(** The files as the modules of a program, as far as they parse
    ([Source_file.parse_recovering]), and their errors. *)
val parse_files
  :  Source_file.t list
  -> Driver.Module_group.t list option * Diagnostic.t list

(** Checks the program [groups], if it parsed, with [errors] (its parse errors),
    seeing [prelude] ([Prelude] if none) within [limits]. [output] is as for
    [Driver.check]. *)
val check_sources
  :  ?compile_time_c_calls:Compile_time_c_calls.t
  -> ?output:(string -> unit)
  -> ?prelude:Syntax.Toplevel.t list
  -> limits:Limits.t
  -> Driver.Module_group.t list option * Diagnostic.t list
  -> Checked.t

(** [check_sources] with the prelude of [config], erring if it doesn't parse. *)
val check
  :  ?compile_time_c_calls:Compile_time_c_calls.t
  -> ?output:(string -> unit)
  -> ?contents:(string -> string option)
  -> Pipeline_config.t
  -> Driver.Module_group.t list option * Diagnostic.t list
  -> (Checked.t, Diagnostic.t) Result.t
