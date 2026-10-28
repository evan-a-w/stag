open! Import

(** A program's [let%bench]es, as routines to measure, as ppx_bench collects
    OCaml's. *)

(** The benches of [driver]'s program in order, after [Driver.eval]: a
    parameterised [let%bench "name" x in [a; b] = e] is one bench per
    argument, named [name/A] after the argument's value. Arguments are
    evaluated now, and an iteration evaluates the body once. With [files], only
    the benches in those files (by [Loc.file]). Errors on the first argument
    that fails, or on a name given twice. Output of the program is discarded
    from now on. *)
val benches
  :  ?files:String.Set.t
  -> Driver.t
  -> (Measure.Suite.Bench.t list, Diagnostic.t) Result.t
