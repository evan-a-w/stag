open! Import

(** Benchmarks of the interpreter itself on a program: [parse] (as given),
    [typecheck] (of the program parsed once, running its compile-time splices
    and the toplevels they use) and [eval] (of the program checked afresh before
    each iteration, untimed), named [PREFIXparse], etc. The program sees
    [prelude] (the embedded one if [None]) and [compile_time_c_calls], as for
    [Driver.check_modules]. Output of the program is discarded. *)
val benches
  :  ?prefix:string
  -> ?compile_time_c_calls:Compile_time_c_calls.t
  -> prelude:Syntax.Toplevel.t list option
  -> (unit -> (Driver.Module_group.t list, Diagnostic.t list) Result.t)
  -> Measure.Suite.Bench.t list

(** The parse of [files] for [benches], as [Driver.check_files] parses them. *)
val parse_files
  :  Source_file.t list
  -> unit
  -> (Driver.Module_group.t list, Diagnostic.t list) Result.t
