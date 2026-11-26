open! Import
module Implied_constraints = Private.Implied_constraints

let identity = Morphism.identity (T Stage)
let wider = Morphism.cross_portable

(* Prints each constraint [src <= m(dst)] of nodes named by letter, with its
   verdict. *)
let verdicts edges =
  let names =
    List.dedup_and_sort
      ~compare:Char.compare
      (List.concat_map edges ~f:(fun (a, _, b) -> [ a; b ]))
  in
  let index name =
    Option.value_exn (List.findi names ~f:(fun _ c -> Char.equal c name)) |> fst
  in
  let succ =
    Array.of_list_map names ~f:(fun name ->
      List.filter_map edges ~f:(fun (src, m, dst) ->
        Option.some_if (Char.equal src name) (index dst, m))
      |> Array.of_list)
  in
  let t = Implied_constraints.create ~succ in
  List.iteri names ~f:(fun src name ->
    Array.iteri succ.(src) ~f:(fun i (dst, m) ->
      print_s
        [%message
          ""
            ~_:(sprintf "%c <= %c" name (List.nth_exn names dst))
            (m : Morphism.t)
            ~_:
              (Implied_constraints.verdict t ~src i
               : Implied_constraints.Verdict.t)]))
;;

let%expect_test "without cycles, a constraint is implied by a path around it" =
  verdicts [ 'a', identity, 'b'; 'b', identity, 'c'; 'a', identity, 'c' ];
  [%expect
    {|
    ("a <= b" (m id) Not_implied)
    ("a <= c" (m id) Implied)
    ("b <= c" (m id) Not_implied)
    |}]
;;

let%expect_test "a parallel constraint implies one at a wider morphism" =
  verdicts
    [ 'a', wider, 'b'; 'a', identity, 'b'; 'b', wider, 'c'; 'a', identity, 'c' ];
  [%expect
    {|
    ("a <= b" (m cross_portable) Implied)
    ("a <= b" (m id) Not_implied)
    ("a <= c" (m id) Not_implied)
    ("b <= c" (m cross_portable) Not_implied)
    |}]
;;

let%expect_test "from nodes reaching cycles, which are implied depends on the \
                 order"
  =
  (* [s <= a] and [s <= b] each imply the other, through the cycle: only one can
     be dropped. *)
  verdicts
    [ 's', identity, 'a'
    ; 's', identity, 'b'
    ; 'a', identity, 'b'
    ; 'b', identity, 'a'
    ; 'a', identity, 'c'
    ];
  [%expect
    {|
    ("a <= b" (m id) Not_implied)
    ("a <= c" (m id) Unknown)
    ("b <= a" (m id) Not_implied)
    ("s <= a" (m id) Unknown)
    ("s <= b" (m id) Unknown)
    |}]
;;

let%expect_test "a node is reached through each of its constraints" =
  (* Both of [s]'s constraints reach [d], through different nodes: together they
     imply [s <= d], but [s] reaches its own cycle, so it is left undecided. *)
  verdicts
    [ 's', identity, 'a'
    ; 's', identity, 'b'
    ; 'a', identity, 'd'
    ; 'b', identity, 'd'
    ; 's', identity, 'd'
    ; 's', wider, 's'
    ];
  [%expect
    {|
    ("a <= d" (m id) Not_implied)
    ("b <= d" (m id) Not_implied)
    ("s <= a" (m id) Not_implied)
    ("s <= b" (m id) Not_implied)
    ("s <= d" (m id) Unknown)
    ("s <= s" (m cross_portable) Implied)
    |}]
;;
