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
    ; warnings : Diagnostic.t list (** By location. *)
    }

  val to_result : t -> (Driver.t, Diagnostic.t list) Result.t
end

(** The files as the modules of a program, as far as they parse
    ([Source_file.parse_recovering]), and their errors. *)
val parse_files
  :  Source_file.t list
  -> Driver.Module_group.t list option * Diagnostic.t list

(** Checks the program [groups], if it parsed, with [errors] (its parse errors),
    seeing [prelude] within [limits]. [output] is as for [Driver.check],
    [record_references] and [warnings] as for [Typecheck.create]. *)
val check_sources
  :  ?compile_time_c_calls:Compile_time_c_calls.t
  -> ?record_references:bool
  -> ?warnings:Warning.Settings.t
  -> ?output:(string -> unit)
  -> prelude:Pipeline_config.Loaded_prelude.t
  -> limits:Limits.t
  -> Driver.Module_group.t list option * Diagnostic.t list
  -> Checked.t

(** [check_sources] with the prelude of [config], loaded by [load_prelude]
    ([Pipeline_config.Loaded_prelude.load] by default), erring if it fails, and
    that prelude. *)
val check
  :  ?compile_time_c_calls:Compile_time_c_calls.t
  -> ?record_references:bool
  -> ?output:(string -> unit)
  -> ?load_prelude:
       (Pipeline_config.Prelude_choice.t
        -> (Pipeline_config.Loaded_prelude.t, Diagnostic.t) Result.t)
  -> Pipeline_config.t
  -> Driver.Module_group.t list option * Diagnostic.t list
  -> (Pipeline_config.Loaded_prelude.t * Checked.t, Diagnostic.t) Result.t

(** Checks [prelude] itself ([Driver.check_prelude]), as the module
    [Prelude_path.root] of its toplevels. *)
val check_prelude
  :  ?record_references:bool
  -> ?output:(string -> unit)
  -> limits:Limits.t
  -> Syntax.Toplevel.t list
  -> Checked.t
