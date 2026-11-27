open! Import

(* Random syntax ([Gen]), printed at several widths and checked to round trip. *)

module Line_counts = struct
  type t =
    { lines : int
    ; over_width : int
    }
  [@@deriving sexp_of]

  let zero = { lines = 0; over_width = 0 }

  let ( + ) a b =
    { lines = a.lines + b.lines; over_width = a.over_width + b.over_width }
  ;;

  let of_string s ~width =
    let lines = String.split_lines s in
    { lines = List.length lines
    ; over_width = List.count lines ~f:(fun line -> String.length line > width)
    }
  ;;
end

(* Each width's line counts, summed. *)
module Counts = struct
  type t = Line_counts.t Int.Map.t [@@deriving sexp_of]

  let add t ~width counts =
    Map.update t width ~f:(fun existing ->
      Line_counts.(Option.value existing ~default:zero + counts))
  ;;
end

(* Round trips [t] at each width, counting the printed lines. *)
let check
  counts
  ~widths
  ~(round_trip : ?widths:int list -> 'a -> unit)
  ~(print : ?width:int -> 'a -> string)
  t
  =
  round_trip ~widths t;
  List.fold widths ~init:counts ~f:(fun counts width ->
    if width < 10
    then counts
    else
      Counts.add counts ~width (Line_counts.of_string (print ~width t) ~width))
;;

let check_all counts ~widths (gen : unit -> Gen.t) =
  let type_expr = Gen.type_expr (gen ()) in
  let pattern = Gen.pattern (gen ()) in
  let expr = Gen.expr (gen ()) in
  let program =
    let gen = gen () in
    List.init 3 ~f:(fun _ -> Gen.toplevel gen)
  in
  let counts =
    check
      counts
      ~widths
      ~round_trip:Round_trip.type_expr
      ~print:(fun ?width t -> Printer.type_expr ?width t)
      type_expr
  in
  let counts =
    check
      counts
      ~widths
      ~round_trip:Round_trip.pattern
      ~print:(fun ?width t -> Printer.pattern ?width t)
      pattern
  in
  let counts =
    check
      counts
      ~widths
      ~round_trip:Round_trip.expr
      ~print:(fun ?width t -> Printer.expr ?width t)
      expr
  in
  check
    counts
    ~widths
    ~round_trip:Round_trip.program
    ~print:Printer.program
    program
;;

(* Lines over the width are mostly from nesting deeper than the width, and
   chains of long constructor names. *)
let%expect_test "random syntax round trips" =
  let random = Random.State.make [| 0 |] in
  let counts = ref Int.Map.empty in
  for size = 0 to 5000 do
    counts
    := check_all !counts ~widths:[ 80; 30; 1 ] (fun () ->
         { Gen.random; size = size % 60; deep = false })
  done;
  print_s [%sexp (!counts : Counts.t)];
  [%expect
    {|
    ((30 ((lines 540483) (over_width 314642)))
     (80 ((lines 435054) (over_width 82343))))
    |}]
;;

let%expect_test "deep random syntax round trips" =
  let random = Random.State.make [| 1 |] in
  let counts = ref Int.Map.empty in
  for count = 0 to 199 do
    counts
    := check_all !counts ~widths:[ 80; 30 ] (fun () ->
         { Gen.random; size = 20 + count; deep = true })
  done;
  print_s [%sexp (!counts : Counts.t)];
  [%expect
    {|
    ((30 ((lines 108690) (over_width 94372)))
     (80 ((lines 104059) (over_width 71642))))
    |}]
;;
