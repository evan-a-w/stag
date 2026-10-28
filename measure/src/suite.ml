open! Import

module Bench = struct
  type t =
    | Single of
        { name : string
        ; routine : Routine.t
        }
    | Parameterised of
        { name : string
        ; args : (unit -> (string * Routine.t) Or_error.t) list
        }

  let add_prefix t prefix =
    match t with
    | Single { name; routine } -> Single { name = prefix ^ name; routine }
    | Parameterised { name; args } ->
      Parameterised { name = prefix ^ name; args }
  ;;
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

let may_select_arguments ~filters name =
  List.is_empty filters
  || List.exists filters ~f:(fun filter ->
    String.is_substring name ~substring:filter
    ||
    match String.lsplit2 filter ~on:'/' with
    | Some (before, _) -> String.is_suffix name ~suffix:before
    | None -> false)
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
  let measure name routine =
    Gc.compact ();
    let entry =
      let%map.Or_error measurement =
        Measurement.run ?alloc config clock routine
      in
      let result = Bench_result.create config ~name measurement in
      let change : Report.Change.t =
        match baseline with
        | None -> Not_compared
        | Some baseline ->
          (match Map.find baseline name with
           | None -> Not_in_baseline
           | Some baseline ->
             Compared
               (Comparison.compare
                  config
                  ~baseline
                  ~current:(Bench_result.per_iteration_ns result)))
      in
      { Report.Entry.result; change }
    in
    { Outcome.name; entry }
  in
  let report outcome =
    on_outcome outcome;
    outcome
  in
  List.concat_map benches ~f:(fun (bench : Bench.t) ->
    match bench with
    | Single { name; routine } ->
      if selected ~filters name then [ report (measure name routine) ] else []
    | Parameterised { name; args } ->
      if not (may_select_arguments ~filters name)
      then []
      else
        List.filter_map args ~f:(fun arg ->
          match arg () with
          | Error error -> Some (report { Outcome.name; entry = Error error })
          | Ok (label, routine) ->
            let name = name ^ "/" ^ label in
            Option.some_if (selected ~filters name) ()
            |> Option.map ~f:(fun () -> report (measure name routine))))
;;
