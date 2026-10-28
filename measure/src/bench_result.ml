open! Import

type t =
  { name : string
  ; measurement : Measurement.t
  ; stats : Stats.t
  ; alloc : Alloc.t option
  }
[@@deriving sexp_of]

let per_iteration_ns_of_measurement (measurement : Measurement.t) =
  Array.of_list_map measurement.samples ~f:Sample.per_iteration_ns
;;

let per_iteration_ns t = per_iteration_ns_of_measurement t.measurement

let create config ~name (measurement : Measurement.t) =
  let stats =
    Stats.summarize config (per_iteration_ns_of_measurement measurement)
  in
  let alloc =
    let%map.Option allocs =
      Option.all
        (List.map measurement.samples ~f:(fun (sample : Sample.t) ->
           sample.alloc))
    in
    let iterations =
      List.sum (module Int) measurement.samples ~f:(fun s -> s.iterations)
    in
    Alloc.div
      (List.fold allocs ~init:Alloc.zero ~f:Alloc.add)
      ~by:(Float.of_int iterations)
  in
  { name; measurement; stats; alloc }
;;
