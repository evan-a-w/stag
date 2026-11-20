open! Import
open Source_helpers

let nat_type = "type nat = Z | S of nat\n"

(* Takes 2^n steps, at depth n. *)
let spin =
  nat_type ^ "let spin n = match n with Z -> () | S m -> spin m; spin m\n"
;;

(* Code of size 2^n, built in about 2n steps. *)
let blow =
  nat_type
  ^ "let blow n = match n with Z -> .<()>. | S m -> let c = blow m in .<.~c; \
     .~c>.\n"
;;

let rec nat n = if n = 0 then "Z" else [%string "S (%{nat (n - 1)})"]

let only_kind error =
  match Error.sexp_of_t error with
  | List (Atom kind :: _) | Atom kind -> print_endline kind
  | List _ as sexp -> print_s sexp
;;

(* Checks then evaluates [source], printing the values of [show], or the error
   of the phase that failed. *)
let run
  ?(compile_time = limits)
  ?(runtime = limits)
  ?(print_error = fun error -> print_s [%sexp (error : Error.t)])
  ?(show = [])
  source
  =
  match Driver.check ~limits:compile_time (program source) with
  | Error
      { errors =
          [ { kind = Splice_evaluation_failed { error; code = _ }; loc = _ } ]
      ; partial = _
      } ->
    print_string "splice failed: ";
    print_error error
  | Error error ->
    List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
      print_s [%sexp (error : Typecheck_error.t)])
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
                 , (Interpreter.find (Driver.interpreter driver) ~ident
                    |> ok_exn
                    : Interpreter.Value.t)])))
;;

let fuel_only fuel = { Limits.unlimited with fuel = Some fuel }

let%expect_test "a runaway splice runs out of fuel while checking" =
  run (spin ^ [%string "let x = .~(spin (%{nat 40}); .<1>.)\n"]);
  [%expect
    {| splice failed: ("Out of fuel: evaluation took more than 1000000 steps" @ t.stag:2:43-57) |}]
;;

let%expect_test "a looping toplevel runs out of fuel when evaluated" =
  run ~runtime:(fuel_only 1000) {|
let loop x = loop x
let x = loop ()
|};
  [%expect
    {| eval failed: ("Out of fuel: evaluation took more than 1000 steps" @ t.stag:2:13-17) |}]
;;

let%expect_test "terminating programs are unaffected" =
  run
    ~show:[ "x"; "y" ]
    (spin
     ^ [%string
         {|
let x = .~(spin (%{nat 5}); .<1>.)
let y = spin (%{nat 5})
|}]);
  [%expect {|
    (x 1)
    (y "()")
    |}]
;;

let%expect_test "deep recursion exceeds the stack depth" =
  let deep limits =
    run
      ~runtime:limits
      ~show:[ "deep" ]
      (nat_type
       ^ [%string
           {|
let double n = match n with Z -> Z | S m -> S (S (double m))
let deep = %{String.concat (List.init 14 ~f:(fun _ -> "double ("))}%{nat 1}%{String.make 14 ')'}; 0
|}]
      )
  in
  deep limits;
  deep Limits.unlimited;
  [%expect
    {|
    eval failed: ("Stack depth exceeded: evaluation nested more than 10000 steps deep" @
     t.stag:3:50-56)
    (deep 0)
    |}]
;;

let%expect_test "compile time and runtime have separate budgets" =
  let at_compile_time = [%string "let x = .~(spin (%{nat 10}); .<1>.)\n"] in
  let at_runtime = [%string "let y = spin (%{nat 10})\n"] in
  let succeeds ~fuel source =
    let source = spin ^ source in
    match Driver.check ~limits:(fuel_only fuel) (program source) with
    | Error _ -> false
    | Ok driver -> Driver.eval ~limits:(fuel_only fuel) driver |> Result.is_ok
  in
  let least_fuel source =
    let rec search ~low ~high =
      if low = high
      then low
      else (
        let mid = (low + high) / 2 in
        if succeeds ~fuel:mid source
        then search ~low ~high:mid
        else search ~low:(mid + 1) ~high)
    in
    search ~low:0 ~high:1_000_000
  in
  print_s
    [%message
      ""
        ~compile_time:(least_fuel at_compile_time : int)
        ~runtime:(least_fuel at_runtime : int)
        ~both:(least_fuel (at_compile_time ^ at_runtime) : int)];
  [%expect {| ((compile_time 12296) (runtime 12292) (both 12296)) |}]
;;

let%expect_test "the time limit" =
  run
    ~runtime:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 1) }
    ~print_error:only_kind
    (spin ^ [%string "let x = spin (%{nat 60})\n"]);
  [%expect
    {| eval failed: Time limit exceeded: evaluation took more than 1ms |}]
;;

let%expect_test "deep recursion at compile time exceeds the stack depth" =
  run
    ~print_error:only_kind
    (spin ^ [%string "let x = .~(spin (%{nat 20_000}); .<1>.)\n"]);
  [%expect
    {| splice failed: Stack depth exceeded: evaluation nested more than 10000 steps deep |}]
;;

let%expect_test "the time limit at compile time" =
  run
    ~compile_time:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 1) }
    ~print_error:only_kind
    (spin ^ [%string "let x = .~(spin (%{nat 60}); .<1>.)\n"]);
  [%expect
    {| splice failed: Time limit exceeded: evaluation took more than 1ms |}]
;;

let%expect_test "values forced at compile time are not evaluated again" =
  let y = [%string "let y = spin (%{nat 10}); 2\n"] in
  let x = "let x = .~(y; .<1>.)\n" in
  run ~runtime:(fuel_only 100) ~show:[ "x"; "y" ] (spin ^ y ^ x);
  run ~runtime:(fuel_only 100) ~show:[ "y" ] (spin ^ y);
  [%expect
    {|
    (y 2)
    (x 1)
    eval failed: ("Out of fuel: evaluation took more than 100 steps" @ t.stag:2:31-33)
    |}]
;;

let%expect_test "growing code exponentially runs out of fuel at compile time" =
  let source n = blow ^ [%string "let x = .~(blow (%{nat n}))\n"] in
  run ~compile_time:(fuel_only 2000) ~show:[ "x" ] (source 1);
  run ~compile_time:(fuel_only 2000) (source 16);
  run
    ~compile_time:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 50) }
    ~print_error:only_kind
    (source 30);
  [%expect
    {|
    (x "()")
    splice failed: ("Out of fuel: evaluation took more than 2000 steps" @ t.stag:2:33-35)
    splice failed: Time limit exceeded: evaluation took more than 50ms
    |}]
;;

let%expect_test "growing code exponentially runs out of fuel at runtime" =
  let source n = blow ^ [%string "let x = blow (%{nat n})\n"] in
  run ~runtime:(fuel_only 2000) ~show:[ "x" ] (source 1);
  run ~runtime:(fuel_only 2000) (source 20);
  run
    ~runtime:
      { Limits.unlimited with time_limit = Some (Time_ns.Span.of_int_ms 50) }
    ~print_error:only_kind
    (source 30);
  [%expect
    {|
    (x
     (Code
      (Let (ident _/18) (args ()) (generalized false) (body (Lit Unit : unit))
       (in_ (Lit Unit : unit)) : unit)))
    eval failed: ("Out of fuel: evaluation took more than 2000 steps" @ t.stag:2:67-75)
    eval failed: Time limit exceeded: evaluation took more than 50ms
    |}]
;;

let%expect_test "an exception escaping evaluation leaves the depth as it was" =
  let contents =
    {x|
let id x = x
let%expect_test "t" = id (id (id (id (id ([%expect {||}])))))
|x}
  in
  match Driver.check_files [ { Source_file.path = "t.stag"; contents } ] with
  | Error errors -> print_s [%sexp (errors : Driver.Error.t list)]
  | Ok driver ->
    Driver.eval driver |> ok_exn;
    let interpreter = Driver.interpreter driver in
    Interpreter.set_limits
      interpreter
      { Limits.unlimited with max_depth = Some 20 };
    let test = List.hd_exn (Driver.checked driver).program.expect_tests in
    let outcomes =
      List.init 100 ~f:(fun _ ->
        match
          Interpreter.run_expect_test interpreter test ~on_expect:(fun _ ->
            raise_s [%message "on_expect raised"])
        with
        | exception exn -> Exn.to_string exn
        | Ok () -> "Ok"
        | Error error -> Error.to_string_hum error)
    in
    print_s
      [%sexp
        (List.sort_and_group outcomes ~compare:String.compare
         |> List.map ~f:(fun group -> List.hd_exn group, List.length group)
         : (string * int) list)];
    [%expect {| (("\"on_expect raised\"" 100)) |}]
;;
