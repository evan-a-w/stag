open! Import

module Verdict = struct
  type t =
    | Improved
    | Regressed
    | No_change
    | Within_noise
    | Cannot_compare
  [@@deriving sexp]

  let to_string = function
    | Improved -> "Performance has improved."
    | Regressed -> "Performance has regressed."
    | No_change -> "No change in performance detected."
    | Within_noise -> "Change within noise threshold."
    | Cannot_compare -> "Cannot compare with a baseline mean of 0."
  ;;
end

type t =
  { change : Stats.Estimate.t
  ; verdict : Verdict.t
  }
[@@deriving sexp]

let relative_change current baseline =
  (Stats.mean current /. Stats.mean baseline) -. 1.
;;

let compare (config : Config.t) ~baseline ~current =
  let point = relative_change current baseline in
  if Float.( <= ) (Stats.mean baseline) 0. || not (Float.is_finite point)
  then
    { change = { point; lower = point; upper = point }
    ; verdict = Cannot_compare
    }
  else (
    let change =
      Stats.bootstrap2
        ~random:(Random.State.make [| config.seed |])
        ~resamples:config.resamples
        ~confidence:config.confidence
        current
        baseline
        ~f:relative_change
    in
    let verdict : Verdict.t =
      if Float.( <= ) change.lower 0. && Float.( <= ) 0. change.upper
      then No_change
      else if Float.( > ) change.lower config.noise_threshold
      then Regressed
      else if Float.( < ) change.upper (-.config.noise_threshold)
      then Improved
      else Within_noise
    in
    { change; verdict })
;;
