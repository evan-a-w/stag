open! Import
open Syntax
open Helpers

let nat = Decl.variant "nat" [ "Z", []; "S", [ Ty.con "nat" ] ]

let rec nat_of_int n =
  if n = 0 then construct "Z" else construct "S" ~args:[ nat_of_int (n - 1) ]
;;

(* Takes 2^n steps, at depth n. *)
let spin =
  top
    "spin"
    [ "n" ]
    (match_
       (var "n")
       [ Pat.construct "Z", unit
       ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
         , seq (app (var "spin") [ var "m" ]) (app (var "spin") [ var "m" ]) )
       ])
;;

let loop = top "loop" [ "x" ] (app (var "loop") [ var "x" ])

let only_kind error =
  match Error.sexp_of_t error with
  | List (Atom kind :: _) | Atom kind -> print_endline kind
  | List _ as sexp -> print_s sexp
;;

(* Checks then evaluates [program], printing the values of [show], or the
   error of the phase that failed. *)
let run
      ?(compile_time = limits)
      ?(runtime = limits)
      ?(print_error = fun error -> print_s [%sexp (error : Error.t)])
      ?(show = [])
      program
  =
  match Driver.check ~limits:compile_time (prims @ program) with
  | Error { kind = Splice_evaluation_failed { error; code = _ }; loc = _ } ->
    print_string "splice failed: ";
    print_error error
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok driver ->
    (match Driver.eval ~limits:runtime driver with
     | Error error ->
       print_string "eval failed: ";
       print_error error
     | Ok () ->
       let { Typecheck.Checked.program; _ } = Driver.checked driver in
       List.iter program.sccs ~f:(fun scc ->
         List.iter scc.members ~f:(fun { ident; _ } ->
           if List.mem show ident.name ~equal:String.equal
           then
             print_s
               [%sexp
                 (ident.name : string)
               , (Treewalk.find (Driver.interpreter driver) ~ident |> ok_exn
                  : Treewalk.Value.t)])))
;;

let fuel_only fuel = { Limits.unlimited with fuel = Some fuel }

let%expect_test "a runaway splice runs out of fuel while checking" =
  run
    [ nat
    ; spin
    ; top
        "x"
        []
        (splice (seq (app (var "spin") [ nat_of_int 40 ]) (quote (int 1))))
    ];
  [%expect
    {| splice failed: ("Out of fuel: evaluation took more than 1000000 steps" @ _none_) |}]
;;

let%expect_test "a looping toplevel runs out of fuel when evaluated" =
  run ~runtime:(fuel_only 1000) [ loop; top "x" [] (app (var "loop") [ unit ]) ];
  [%expect
    {| eval failed: ("Out of fuel: evaluation took more than 1000 steps" @ _none_) |}]
;;

let%expect_test "terminating programs are unaffected" =
  run
    ~show:[ "x"; "y" ]
    [ nat
    ; spin
    ; top
        "x"
        []
        (splice (seq (app (var "spin") [ nat_of_int 5 ]) (quote (int 1))))
    ; top "y" [] (app (var "spin") [ nat_of_int 5 ])
    ];
  [%expect
    {|
    (x 1)
    (y "()")
    |}]
;;

let%expect_test "deep recursion exceeds the stack depth" =
  let double =
    top
      "double"
      [ "n" ]
      (match_
         (var "n")
         [ Pat.construct "Z", construct "Z"
         ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
           , construct
               "S"
               ~args:[ construct "S" ~args:[ app (var "double") [ var "m" ] ] ]
           )
         ])
  in
  let deep limits =
    run
      ~runtime:limits
      ~show:[ "deep" ]
      [ nat
      ; double
      ; top
          "deep"
          []
          (seq
             (List.fold (List.range 0 14) ~init:(nat_of_int 1) ~f:(fun n _ ->
                app (var "double") [ n ]))
             (int 0))
      ]
  in
  deep limits;
  deep Limits.unlimited;
  [%expect
    {|
    eval failed: ("Stack depth exceeded: evaluation nested more than 10000 steps deep" @
     _none_)
    (deep 0)
    |}]
;;

let%expect_test "compile time and runtime have separate budgets" =
  let at_compile_time =
    top
      "x"
      []
      (splice (seq (app (var "spin") [ nat_of_int 10 ]) (quote (int 1))))
  in
  let at_runtime = top "y" [] (app (var "spin") [ nat_of_int 10 ]) in
  let succeeds ~fuel program =
    match Driver.check ~limits:(fuel_only fuel) (prims @ program) with
    | Error _ -> false
    | Ok driver -> Driver.eval ~limits:(fuel_only fuel) driver |> Result.is_ok
  in
  let least_fuel program =
    let program = nat :: spin :: program in
    let rec search ~low ~high =
      if low = high
      then low
      else (
        let mid = (low + high) / 2 in
        if succeeds ~fuel:mid program
        then search ~low ~high:mid
        else search ~low:(mid + 1) ~high)
    in
    search ~low:0 ~high:1_000_000
  in
  print_s
    [%message
      ""
        ~compile_time:(least_fuel [ at_compile_time ] : int)
        ~runtime:(least_fuel [ at_runtime ] : int)
        ~both:(least_fuel [ at_compile_time; at_runtime ] : int)];
  [%expect {| ((compile_time 12296) (runtime 12292) (both 12296)) |}]
;;

let%expect_test "the time limit" =
  run
    ~runtime:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 1) }
    ~print_error:only_kind
    [ nat; spin; top "x" [] (app (var "spin") [ nat_of_int 60 ]) ];
  [%expect
    {| eval failed: Time limit exceeded: evaluation took more than 1ms |}]
;;

let%expect_test "deep recursion at compile time exceeds the stack depth" =
  run
    ~print_error:only_kind
    [ nat
    ; spin
    ; top
        "x"
        []
        (splice (seq (app (var "spin") [ nat_of_int 20_000 ]) (quote (int 1))))
    ];
  [%expect
    {| splice failed: Stack depth exceeded: evaluation nested more than 10000 steps deep |}]
;;

let%expect_test "the time limit at compile time" =
  run
    ~compile_time:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 1) }
    ~print_error:only_kind
    [ nat
    ; spin
    ; top
        "x"
        []
        (splice (seq (app (var "spin") [ nat_of_int 60 ]) (quote (int 1))))
    ];
  [%expect
    {| splice failed: Time limit exceeded: evaluation took more than 1ms |}]
;;

let%expect_test "values forced at compile time are not evaluated again" =
  let y = top "y" [] (seq (app (var "spin") [ nat_of_int 10 ]) (int 2)) in
  let x = top "x" [] (splice (seq (var "y") (quote (int 1)))) in
  run ~runtime:(fuel_only 100) ~show:[ "x"; "y" ] [ nat; spin; y; x ];
  run ~runtime:(fuel_only 100) ~show:[ "y" ] [ nat; spin; y ];
  [%expect
    {|
    (y 2)
    (x 1)
    eval failed: ("Out of fuel: evaluation took more than 100 steps" @ _none_)
    |}]
;;

(* Code of size 2^n, built in about 2n steps. *)
let blow =
  top
    "blow"
    [ "n" ]
    (match_
       (var "n")
       [ Pat.construct "Z", quote unit
       ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
         , let_
             "c"
             []
             (app (var "blow") [ var "m" ])
             (quote (seq (splice (var "c")) (splice (var "c")))) )
       ])
;;

let%expect_test "growing code exponentially runs out of fuel at compile time" =
  let program n =
    [ nat; blow; top "x" [] (splice (app (var "blow") [ nat_of_int n ])) ]
  in
  run ~compile_time:(fuel_only 2000) ~show:[ "x" ] (program 1);
  run ~compile_time:(fuel_only 2000) (program 16);
  run
    ~compile_time:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 50) }
    ~print_error:only_kind
    (program 30);
  [%expect
    {|
    (x "()")
    splice failed: ("Out of fuel: evaluation took more than 2000 steps" @ _none_)
    splice failed: Time limit exceeded: evaluation took more than 50ms
    |}]
;;

let%expect_test "growing code exponentially runs out of fuel at runtime" =
  let program n =
    [ nat; blow; top "x" [] (app (var "blow") [ nat_of_int n ]) ]
  in
  run ~runtime:(fuel_only 2000) ~show:[ "x" ] (program 1);
  run ~runtime:(fuel_only 2000) (program 20);
  run
    ~runtime:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 50) }
    ~print_error:only_kind
    (program 30);
  [%expect
    {|
    (x
     (Code
      (Let (ident _/18) (args ()) (generalized false) (body (Lit Unit : unit))
       (in_ (Lit Unit : unit)) : unit)))
    eval failed: ("Out of fuel: evaluation took more than 2000 steps" @ _none_)
    eval failed: Time limit exceeded: evaluation took more than 50ms
    |}]
;;
