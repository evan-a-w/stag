open! Import
open Mode_helpers

let growth ~small ~large source =
  Work.Private.growth ~net_of:1 ~small ~large (fun n ->
    snd
      (Work.Private.measure (fun () ->
         assert (Result.is_ok (checked (source n))))))
;;

let print_growth name ~small ~large source =
  print_s
    [%message
      name
        ~growth:
          (growth ~small ~large source : Work.Private.Growth.t String.Map.t)]
;;

let names n = List.init n ~f:(sprintf "x%d")

let%expect_test "many parameters take at most quadratic time" =
  (* The signature's constraints are quadratic in the parameters. *)
  let first n = sprintf "let f %s = x0" (String.concat ~sep:" " (names n)) in
  let tuple n =
    sprintf
      "let f %s = (%s)"
      (String.concat ~sep:" " (names n))
      (String.concat ~sep:", " (names n))
  in
  let curried n =
    sprintf
      "let f = %s x0"
      (String.concat (List.map (names n) ~f:(sprintf "fun %s ->")))
  in
  List.iter
    [ "first", first; "tuple", tuple; "curried", curried ]
    ~f:(fun (name, source) -> print_growth name ~small:50 ~large:200 source);
  [%expect
    {|
    (first
     (growth
      ((allocated_words Quadratic) (mode.graph Quadratic) (syntax.format Linear)
       (typecheck.types Linear) (types.traversal Linear))))
    (tuple
     (growth
      ((allocated_words Quadratic) (mode.graph Quadratic) (syntax.format Linear)
       (typecheck.types Linear) (types.traversal Linear))))
    (curried
     (growth
      ((allocated_words Quadratic) (mode.graph Quadratic) (syntax.format Linear)
       (typecheck.types Linear) (types.traversal Linear))))
    |}]
;;

let%expect_test "a chain of lets takes linear time" =
  let chain n =
    sprintf
      "let g a0 =\n%s a%d"
      (String.concat
         (List.init n ~f:(fun i -> sprintf "let a%d = a%d in\n" (i + 1) i)))
      n
  in
  print_growth "let" ~small:500 ~large:2000 chain;
  [%expect
    {|
    (let
     (growth
      ((allocated_words Linear) (mode.graph Linear) (typecheck.types Linear)
       (types.traversal Linear) (types.unify Linear))))
    |}]
;;

let chain ~prelude ~step n =
  sprintf
    "%slet g a0 =\n%s a%d"
    prelude
    (String.concat
       (List.init n ~f:(fun i -> sprintf "let a%d = %s in\n" (i + 1) (step i))))
    n
;;

let%expect_test "chains of values each holding the one before take linear time" =
  List.iter
    [ "tuple", "", sprintf "(a%d, 1)"
    ; ("list", "", fun i -> sprintf "Cons (Box.make (1, a%d))" i)
    ]
    ~f:(fun (name, prelude, step) ->
      print_growth name ~small:200 ~large:800 (chain ~prelude ~step));
  [%expect
    {|
    (tuple
     (growth
      ((allocated_words Linear) (mode.graph Linear) (typecheck.types Linear)
       (types.traversal Linear) (types.unify Linear))))
    (list
     (growth
      ((allocated_words Linear) (mode.graph Linear) (typecheck.types Linear)
       (types.traversal Linear) (types.unify Linear))))
    |}]
;;

let%expect_test "chains of records and closures take at most quadratic time" =
  List.iter
    [ ( "record"
      , "type r['a] = { x : 'a; y : int }\n"
      , fun i -> sprintf "{ x = a%d; y = 1 }" i )
    ; "closure", "", sprintf "fun (u : unit) -> a%d"
    ]
    ~f:(fun (name, prelude, step) ->
      print_growth name ~small:50 ~large:200 (chain ~prelude ~step));
  [%expect
    {|
    (record
     (growth
      ((allocated_words Quadratic) (mode.graph Linear) (syntax.format Linear)
       (typecheck.types Quadratic) (types.traversal Quadratic)
       (types.unify Quadratic))))
    (closure
     (growth
      ((allocated_words Quadratic) (mode.graph Quadratic) (syntax.format Linear)
       (typecheck.types Linear) (types.traversal Quadratic) (types.unify Linear))))
    |}]
;;
