open! Import

(** A program's [let%bench]es, as routines to measure, as ppx_bench collects
    OCaml's. *)

(** The benches of [driver]'s program in order, after [Driver.eval]: a
    parameterised [let%bench "name" x in [a; b] = e] is one bench per argument,
    named [name/A] after the argument's value (truncated with [...] past 40
    characters). Each argument is evaluated only when its bench is about to run
    (see [Suite.run]), so an argument that fails, or whose name another bench
    has, fails only that bench. An iteration evaluates the body once. With
    [files], only the benches in those files (by [Loc.file]). Errors on two
    unparameterised benches of the same name. Output of the program is
    discarded, and evaluation unlimited, from now on. *)
val benches
  :  ?files:String.Set.t
  -> Driver.t
  -> (Measure.Suite.Bench.t list, Diagnostic.t) Result.t
