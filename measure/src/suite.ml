open! Import

module Bench = struct
  type t =
    { name : string
    ; elements : int option
    ; routine : Routine.t
    }
end

module Outcome = struct
  type t =
    { name : string
    ; entry : Report.Entry.t Or_error.t
    }
  [@@deriving sexp_of]
end

let selected ~filters name =
  List.is_empty filters
  || List.exists filters ~f:(fun filter ->
    String.is_substring name ~substring:filter)
;;

let run
      ?alloc
      ?baseline
      ?(on_outcome = ignore)
      ?(filters = [])
      config
      clock
      benches
  =
  List.filter benches ~f:(fun (bench : Bench.t) -> selected ~filters bench.name)
  |> List.map ~f:(fun { Bench.name; elements; routine } ->
    let entry =
      let%map.Or_error measurement =
        Measurement.run ?alloc config clock routine
      in
      let result = Bench_result.create ?elements config ~name measurement in
      let comparison =
        let%map.Option baseline =
          Option.bind baseline ~f:(Fn.flip Baseline.find name)
        in
        Comparison.compare
          config
          ~baseline
          ~current:(Bench_result.per_iteration_ns result)
      in
      { Report.Entry.result; comparison }
    in
    let outcome = { Outcome.name; entry } in
    on_outcome outcome;
    outcome)
;;
