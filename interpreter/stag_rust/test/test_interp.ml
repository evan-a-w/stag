open! Import

let compare ?(gc_stress = Debug.Gc_stress.all) source ~name =
  match Source_helpers.typecheck ~prelude:false source with
  | None -> ()
  | Some t ->
    let program = (Source_helpers.checked t).program in
    let rust =
      List.map gc_stress ~f:(fun gc_stress ->
        Interp.eval_toplevel ~gc_stress program ~name
        |> [%sexp_of: Interp_value.t Or_error.t])
      |> List.dedup_and_sort ~compare:Sexp.compare
    in
    let treewalk =
      let driver = Source_helpers.driver t in
      match Driver.eval ~limits:Source_helpers.limits driver with
      | Error error -> [%sexp (error : Error.t)]
      | Ok () ->
        (match
           List.find (Source_helpers.members t) ~f:(fun member ->
             String.equal member.ident.name name)
         with
         | None -> [%sexp "no such toplevel"]
         | Some member ->
           [%sexp
             (Treewalk.find (Driver.interpreter driver) ~ident:member.ident
              : Treewalk.Value.t Or_error.t)])
    in
    print_s [%sexp { rust : Sexp.t list; treewalk : Sexp.t }]
;;

let%expect_test "fib" =
  compare
    ~name:"result"
    {|
let fib n =
  match Int.lt n 2 with
  | true -> n
  | false -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))

let result = fib 15
|};
  [%expect {| ((rust ((Ok (Int 610)))) (treewalk (Ok 610))) |}]
;;

let%expect_test "matching constructors" =
  compare
    ~name:"result"
    {|
type expr = Num of int | Add of expr * expr | Mul of expr * expr

let eval e =
  match e with
  | Num n -> n
  | Add (a, b) -> Int.add (eval a) (eval b)
  | Mul (a, b) -> Int.mul (eval a) (eval b)

let simplify e =
  match e with
  | Mul (Num 1, e) -> e
  | e -> e

let result =
  let e = simplify (Mul (Num 1, Add (Num 2, Num 3))) in
  (e, eval e)
|};
  [%expect
    {|
    ((rust
      ((Ok
        (Tuple
         ((Construct Add ((Construct Num ((Int 2))) (Construct Num ((Int 3)))))
          (Int 5))))))
     (treewalk (Ok (Tuple (Add (Num 2) (Num 3)) 5))))
    |}]
;;

let%expect_test "local recursive functions, partial application" =
  compare
    ~name:"result"
    {|
let result =
  let sum_to n =
    let go acc n =
      match Int.equal n 0 with
      | true -> acc
      | false -> go (Int.add acc n) (Int.sub n 1)
    in
    go 0 n
  in
  let add = Int.add in
  let add1 = add 1 in
  let twice f x = f (f x) in
  (sum_to 100, twice add1 0, add1, fun x -> x)
|};
  [%expect
    {|
    ((rust ((Ok (Tuple ((Int 5050) (Int 2) (Prim Int.add) Closure)))))
     (treewalk (Ok (Tuple 5050 2 <fun> <fun>))))
    |}]
;;

let%expect_test "records and literals" =
  compare
    ~name:"result"
    {|
type point = { x : int; y : int }

let result =
  let p = { y = 2; x = 1 } in
  let name c = match c with 'a' -> "a" | _ -> "other" in
  (p, p.y, name 'a', name 'b', true, ())
|};
  [%expect
    {|
    ((rust
      ((Ok
        (Tuple
         ((Record ((x (Int 1)) (y (Int 2)))) (Int 2) (String a) (String other)
          (Bool true) Unit)))))
     (treewalk
      (Ok (Tuple (point (x 1) (y 2)) 2 (String a) (String other) true "()"))))
    |}]
;;

let%expect_test "a quote is code" =
  compare
    ~name:"result"
    {|
let result = .< Int.add 1 2 >.
|};
  [%expect
    {|
    ((rust
      ((Ok
        (Code
         (App
          (App (Var (Prim Int.add) : "int -> int -> int") (Lit (Int 1) : int) :
           "int -> int")
          (Lit (Int 2) : int) : int)))))
     (treewalk
      (Ok
       (Code
        (App
         (App (Var (Prim Int.add) : "int -> int -> int") (Lit (Int 1) : int) :
          "int -> int")
         (Lit (Int 2) : int) : int)))))
    |}]
;;

let%expect_test "what is not implemented" =
  compare
    ~name:"result"
    {|
let result = `A 1
|};
  compare
    ~name:"result"
    {|
let result = print_int 1
|};
  [%expect
    {|
    ((rust ((Error "not implemented: PolyVariant (of type [..]), at 2:13")))
     (treewalk (Ok (`A 1))))
    1((rust ((Error "not implemented: prim print_int on [1]")))
     (treewalk (Ok "()")))
    |}]
;;
