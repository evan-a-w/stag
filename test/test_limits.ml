open! Core
open Stag
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
  | Error (Splice_evaluation_failed { error; code = _ }) ->
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
  [%expect {| splice failed: ("Out of fuel" (fuel 1000000)) |}]
;;

let%expect_test "a looping toplevel runs out of fuel when evaluated" =
  run ~runtime:(fuel_only 1000) [ loop; top "x" [] (app (var "loop") [ unit ]) ];
  [%expect {| eval failed: ("Out of fuel" (fuel 1000)) |}]
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
    eval failed: ("Stack depth exceeded" (max_depth 10000))
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
  [%expect {| ((compile_time 12294) (runtime 12292) (both 12294)) |}]
;;

let%expect_test "the time limit" =
  run
    ~runtime:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 1) }
    ~print_error:only_kind
    [ nat; spin; top "x" [] (app (var "spin") [ nat_of_int 60 ]) ];
  [%expect {| eval failed: Time limit exceeded |}]
;;
