open! Import

module Format = struct
  type t =
    | Human
    | Sexp
    | Json
  [@@deriving sexp, enumerate]

  let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))
end

module Change = struct
  type t =
    | Not_compared
    | Not_in_baseline
    | Compared of Comparison.t
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { result : Bench_result.t
    ; change : Change.t
    }
  [@@deriving sexp_of]
end

(* [value] with 4 significant digits, scaled by a power of 1000 to the
   largest of [units] that leaves at least 1 before the point. *)
let scaled value ~units =
  if not (Float.is_finite value)
  then Float.to_string value, List.hd_exn units
  else (
    let sign, magnitude =
      if Float.is_negative value then "-", Float.abs value else "", value
    in
    let mantissa, exponent =
      match String.lsplit2 (sprintf "%.3e" magnitude) ~on:'e' with
      | Some (mantissa, exponent) ->
        String.filter mantissa ~f:Char.is_digit, Int.of_string exponent
      | None -> assert false
    in
    let exponent = if Float.equal magnitude 0. then 0 else exponent in
    let group =
      Int.clamp_exn (exponent / 3) ~min:0 ~max:(List.length units - 1)
    in
    let point = exponent - (3 * group) + 1 in
    let digits =
      if point <= 0
      then "0." ^ String.make (-point) '0' ^ mantissa
      else if point >= String.length mantissa
      then mantissa ^ String.make (point - String.length mantissa) '0'
      else
        String.prefix mantissa point ^ "." ^ String.drop_prefix mantissa point
    in
    sign ^ digits, List.nth_exn units group)
;;

let time ns =
  let ns = if Float.( < ) (Float.abs ns) 1e-3 then 0. else ns in
  let number, unit = scaled ns ~units:[ "ns"; "us"; "ms"; "s" ] in
  number ^ " " ^ unit
;;

let count value =
  if Float.equal value 0.
  then "0"
  else (
    let number, unit = scaled value ~units:[ ""; "k"; "M"; "G"; "T" ] in
    number ^ unit)
;;

let percent fraction = sprintf "%+.3f%%" (100. *. fraction)

let plural n word =
  if n = 1 then sprintf "1 %s" word else sprintf "%d %ss" n word
;;

let outliers (outliers : Stats.Outliers.t) ~count =
  match Stats.Outliers.total outliers with
  | 0 -> "none"
  | total ->
    let categories =
      List.filter_map
        [ outliers.low_severe, "low severe"
        ; outliers.low_mild, "low mild"
        ; outliers.high_mild, "high mild"
        ; outliers.high_severe, "high severe"
        ]
        ~f:(fun (n, name) -> Option.some_if (n > 0) (sprintf "%d %s" n name))
    in
    sprintf
      "%d of %d (%.2f%%): %s"
      total
      count
      (100. *. Float.of_int total /. Float.of_int count)
      (String.concat categories ~sep:", ")
;;

let human (config : Config.t) ({ result; change } : Entry.t) =
  let { Bench_result.name; measurement; stats; alloc } = result in
  let confidence = sprintf "%g%% CI" (100. *. config.confidence) in
  let line label value = sprintf "  %-10s %s" (label ^ ":") value in
  let lines =
    [ Some name
    ; Some
        (line
           "time"
           (sprintf
              "[%s %s %s] (mean, %s)"
              (time stats.mean.lower)
              (time stats.mean.point)
              (time stats.mean.upper)
              confidence))
    ; Some
        (line
           "median"
           (sprintf
              "%s   stddev: %s   MADN: %s"
              (time stats.median)
              (time stats.stddev)
              (time stats.mad)))
    ; Some
        (line
           "range"
           (sprintf
              "[%s %s]   p90: %s   p99: %s"
              (time stats.min)
              (time stats.max)
              (time stats.p90)
              (time stats.p99)))
    ; Option.map alloc ~f:(fun (alloc : Alloc.t) ->
        line
          "alloc"
          (sprintf
             "%s minor words, %s major words, %s minor GCs, %s major GCs per \
              iteration"
             (count alloc.minor_words)
             (count alloc.major_words)
             (count alloc.minor_collections)
             (count alloc.major_collections)))
    ; Some
        (line
           "samples"
           (sprintf
              "%d x %s (warm-up: %s in %s)"
              (List.length measurement.samples)
              (plural measurement.plan.iterations "iteration")
              (plural measurement.warm_up_iterations "iteration")
              (time (Time_ns.Span.to_ns measurement.warm_up_time))))
    ; Some (line "outliers" (outliers stats.outliers ~count:stats.count))
    ; (match change with
       | Not_compared -> None
       | Not_in_baseline -> Some (line "change" "not in the baseline")
       | Compared { change = _; verdict = Cannot_compare as verdict } ->
         Some (line "change" (Comparison.Verdict.to_string verdict))
       | Compared { change; verdict } ->
         Some
           (line
              "change"
              (sprintf
                 "[%s %s %s] (%s) %s"
                 (percent change.lower)
                 (percent change.point)
                 (percent change.upper)
                 confidence
                 (Comparison.Verdict.to_string verdict))))
    ]
  in
  String.concat (List.filter_opt lines) ~sep:"\n" ^ "\n"
;;

module Json = struct
  type t =
    | Null
    | Int of int
    | Number of float
    | String of string
    | List of t list
    | Object of (string * t) list

  let escape string =
    let buffer = Buffer.create (String.length string + 2) in
    Buffer.add_char buffer '"';
    String.iter string ~f:(fun c ->
      match c with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\012' -> Buffer.add_string buffer "\\f"
      | c when Char.to_int c < 0x20 || Char.to_int c = 0x7f ->
        Buffer.add_string buffer (sprintf "\\u%04x" (Char.to_int c))
      | c -> Buffer.add_char buffer c);
    Buffer.add_char buffer '"';
    Buffer.contents buffer
  ;;

  (* The shortest of [%.15g], [%.16g] and [%.17g] that reads back exactly. *)
  let number x =
    if not (Float.is_finite x)
    then "null"
    else
      List.find_map_exn [ 15; 16; 17 ] ~f:(fun precision ->
        let s = sprintf "%.*g" precision x in
        Option.some_if (precision = 17 || Float.equal (Float.of_string s) x) s)
  ;;

  let rec to_buffer buffer ~indent t =
    let newline indent =
      Buffer.add_char buffer '\n';
      Buffer.add_string buffer (String.make indent ' ')
    in
    let sequence open_ close items ~f =
      match items with
      | [] -> Buffer.add_string buffer (open_ ^ close)
      | items ->
        Buffer.add_string buffer open_;
        List.iteri items ~f:(fun i item ->
          if i > 0 then Buffer.add_char buffer ',';
          newline (indent + 2);
          f item);
        newline indent;
        Buffer.add_string buffer close
    in
    match t with
    | Null -> Buffer.add_string buffer "null"
    | Int i -> Buffer.add_string buffer (Int.to_string i)
    | Number x -> Buffer.add_string buffer (number x)
    | String s -> Buffer.add_string buffer (escape s)
    | List items ->
      sequence "[" "]" items ~f:(to_buffer buffer ~indent:(indent + 2))
    | Object fields ->
      sequence "{" "}" fields ~f:(fun (key, value) ->
        Buffer.add_string buffer (escape key);
        Buffer.add_string buffer ": ";
        to_buffer buffer ~indent:(indent + 2) value)
  ;;

  let to_string t =
    let buffer = Buffer.create 1024 in
    to_buffer buffer ~indent:0 t;
    Buffer.contents buffer
  ;;
end

let json_of_entry ({ result; change } : Entry.t) : Json.t =
  let { Bench_result.name; measurement; stats; alloc } = result in
  let estimate ({ point; lower; upper } : Stats.Estimate.t) =
    [ "point", Json.Number point; "lower", Number lower; "upper", Number upper ]
  in
  Object
    [ "name", String name
    ; "samples", Int (List.length measurement.samples)
    ; "iterations", Int measurement.plan.iterations
    ; "mean", Object (estimate stats.mean)
    ; "median", Number stats.median
    ; "min", Number stats.min
    ; "max", Number stats.max
    ; "stddev", Number stats.stddev
    ; "mad", Number stats.mad
    ; "p90", Number stats.p90
    ; "p99", Number stats.p99
    ; ( "outliers"
      , Object
          [ "low_severe", Int stats.outliers.low_severe
          ; "low_mild", Int stats.outliers.low_mild
          ; "high_mild", Int stats.outliers.high_mild
          ; "high_severe", Int stats.outliers.high_severe
          ] )
    ; ( "alloc"
      , Option.value_map alloc ~default:Json.Null ~f:(fun (alloc : Alloc.t) ->
          Object
            [ "minor_words", Number alloc.minor_words
            ; "promoted_words", Number alloc.promoted_words
            ; "major_words", Number alloc.major_words
            ; "minor_collections", Number alloc.minor_collections
            ; "major_collections", Number alloc.major_collections
            ]) )
    ; ( "change"
      , match change with
        | Not_compared -> Json.Null
        | Not_in_baseline -> Object [ "verdict", String "not_in_baseline" ]
        | Compared { change; verdict } ->
          Object
            (estimate change
             @ [ ( "verdict"
                 , String
                     (String.lowercase
                        (Sexp.to_string (Comparison.Verdict.sexp_of_t verdict)))
                 )
               ]) )
    ]
;;

let machine (format : Format.t) entries =
  match format with
  | Human -> raise_s [%message "Report.machine: human is not a machine format"]
  | Sexp -> Sexp.to_string_hum [%sexp (entries : Entry.t list)] ^ "\n"
  | Json -> Json.to_string (List (List.map entries ~f:json_of_entry)) ^ "\n"
;;
