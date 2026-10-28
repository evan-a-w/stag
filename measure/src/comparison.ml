open! Import

module Verdict = struct
  type t =
    | Improved
    | Regressed
    | No_change
    | Within_noise
  [@@deriving sexp]

  let to_string = function
    | Improved -> "Performance has improved."
    | Regressed -> "Performance has regressed."
    | No_change -> "No change in performance detected."
    | Within_noise -> "Change within noise threshold."
  ;;
end

type t =
  { change : Stats.Estimate.t
  ; verdict : Verdict.t
  }
[@@deriving sexp]

let resample random data =
  let n = Array.length data in
  Array.init n ~f:(fun _ -> data.(Random.State.int random n))
;;

let compare (config : Config.t) ~baseline ~current =
  let random = Random.State.make [| config.seed |] in
  let point = (Stats.mean current /. Stats.mean baseline) -. 1. in
  let changes =
    Array.init config.resamples ~f:(fun _ ->
      let current = resample random current in
      let baseline = resample random baseline in
      (Stats.mean current /. Stats.mean baseline) -. 1.)
  in
  let change : Stats.Estimate.t =
    if Array.is_empty changes
    then { point; lower = point; upper = point }
    else (
      let tail = (1. -. config.confidence) /. 2. in
      { point
      ; lower = Stats.percentile changes tail
      ; upper = Stats.percentile changes (1. -. tail)
      })
  in
  let verdict : Verdict.t =
    if Float.( <= ) change.lower 0. && Float.( <= ) 0. change.upper
    then No_change
    else if Float.( <= ) (Float.abs point) config.noise_threshold
    then Within_noise
    else if Float.( > ) point 0.
    then Regressed
    else Improved
  in
  { change; verdict }
;;
