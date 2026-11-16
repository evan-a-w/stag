open! Import

module Estimate = struct
  type t =
    { point : float
    ; lower : float
    ; upper : float
    }
  [@@deriving sexp]
end

let check_nonempty data =
  if Array.is_empty data then raise_s [%message "Stats: empty data"]
;;

let sorted data =
  let data = Array.copy data in
  Array.sort data ~compare:Float.compare;
  data
;;

let percentile_of_sorted sorted p =
  let n = Array.length sorted in
  let rank = Float.of_int (n - 1) *. Float.clamp_exn p ~min:0. ~max:1. in
  let lo = Float.iround_down_exn rank in
  let hi = Int.min (lo + 1) (n - 1) in
  let frac = rank -. Float.of_int lo in
  sorted.(lo) +. (frac *. (sorted.(hi) -. sorted.(lo)))
;;

let percentile data p =
  check_nonempty data;
  percentile_of_sorted (sorted data) p
;;

let median data = percentile data 0.5

let mean data =
  check_nonempty data;
  Array.sum (module Float) data ~f:Fn.id /. Float.of_int (Array.length data)
;;

let stddev data =
  let m = mean data in
  let n = Array.length data in
  if n = 1
  then 0.
  else
    Float.sqrt
      (Array.sum (module Float) data ~f:(fun x -> Float.square (x -. m))
       /. Float.of_int (n - 1))
;;

let mad data =
  let m = median data in
  1.4826 *. median (Array.map data ~f:(fun x -> Float.abs (x -. m)))
;;

module Outliers = struct
  type t =
    { low_severe : int
    ; low_mild : int
    ; high_mild : int
    ; high_severe : int
    }
  [@@deriving sexp]

  let classify data =
    let sorted = sorted data in
    check_nonempty sorted;
    let q1 = percentile_of_sorted sorted 0.25 in
    let q3 = percentile_of_sorted sorted 0.75 in
    let iqr = q3 -. q1 in
    let fence k = q1 -. (k *. iqr), q3 +. (k *. iqr) in
    let low_mild_fence, high_mild_fence = fence 1.5 in
    let low_severe_fence, high_severe_fence = fence 3. in
    let count f = Array.count sorted ~f in
    { low_severe = count (fun x -> Float.( < ) x low_severe_fence)
    ; low_mild =
        count (fun x ->
          Float.( >= ) x low_severe_fence && Float.( < ) x low_mild_fence)
    ; high_mild =
        count (fun x ->
          Float.( > ) x high_mild_fence && Float.( <= ) x high_severe_fence)
    ; high_severe = count (fun x -> Float.( > ) x high_severe_fence)
    }
  ;;

  let total t = t.low_severe + t.low_mild + t.high_mild + t.high_severe
end

type t =
  { count : int
  ; mean : Estimate.t
  ; median : float
  ; min : float
  ; max : float
  ; stddev : float
  ; mad : float
  ; p90 : float
  ; p99 : float
  ; outliers : Outliers.t
  }
[@@deriving sexp]

let interval ~confidence ~point statistics : Estimate.t =
  if Array.is_empty statistics
  then { point; lower = point; upper = point }
  else (
    Array.sort statistics ~compare:Float.compare;
    let tail = (1. -. confidence) /. 2. in
    { point
    ; lower = percentile_of_sorted statistics tail
    ; upper = percentile_of_sorted statistics (1. -. tail)
    })
;;

let resampler random data =
  check_nonempty data;
  let n = Array.length data in
  let resample = Array.create ~len:n 0. in
  fun () ->
    for i = 0 to n - 1 do
      resample.(i) <- data.(Random.State.int random n)
    done;
    resample
;;

let bootstrap ~random ~resamples ~confidence data ~(local_ f) =
  let resample = resampler random data in
  interval
    ~confidence
    ~point:(f data)
    (Array.init resamples ~f:(fun _ -> f (resample ())))
;;

let bootstrap2 ~random ~resamples ~confidence a b ~(local_ f) =
  let resample_a = resampler random a in
  let resample_b = resampler random b in
  interval
    ~confidence
    ~point:(f a b)
    (Array.init resamples ~f:(fun _ ->
       let a = resample_a () in
       f a (resample_b ())))
;;

let summarize (config : Config.t) data =
  let sorted = sorted data in
  check_nonempty sorted;
  let random = Random.State.make [| config.seed |] in
  let median = percentile_of_sorted sorted 0.5 in
  { count = Array.length data
  ; mean =
      bootstrap
        ~random
        ~resamples:config.resamples
        ~confidence:config.confidence
        data
        ~f:mean
  ; median
  ; min = sorted.(0)
  ; max = sorted.(Array.length sorted - 1)
  ; stddev = stddev data
  ; mad = mad data
  ; p90 = percentile_of_sorted sorted 0.9
  ; p99 = percentile_of_sorted sorted 0.99
  ; outliers = Outliers.classify data
  }
;;
