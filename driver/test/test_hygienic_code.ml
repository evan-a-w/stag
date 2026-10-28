open! Import

let check_files = File_check.check_files

(* Calls [f] on the name and value of each toplevel [e_*] of [source]
   (gen.stag), once evaluated. *)
let iter_erased source ~f =
  match
    Driver.check_files
      ~limits:Source_helpers.limits
      [ { path = "gen.stag"; contents = source } ]
  with
  | Error error ->
    print_string
      (Diagnostic.to_string_hum
         ~contents:(fun _ -> Some source)
         (Diagnostic.of_driver_error error))
  | Ok driver ->
    (match Driver.eval ~limits:Source_helpers.limits driver with
     | Error error ->
       print_string
         (Diagnostic.to_string_hum
            ~contents:(fun _ -> Some source)
            (Diagnostic.of_runtime_error error))
     | Ok () ->
       List.iter (Driver.checked driver).program.sccs ~f:(fun scc ->
         List.iter scc.members ~f:(fun (member : Typed.Toplevel.t) ->
           let name = Prelude_path.short_name member.ident.name in
           if String.is_prefix name ~prefix:"e_"
           then
             f
               name
               (Treewalk.find (Driver.interpreter driver) ~ident:member.ident
                |> ok_exn))))
;;

let show_values source =
  iter_erased source ~f:(fun name value ->
    printf "%s: %s\n" name (Treewalk.Value.to_string_hum value))
;;

(* As syntax, printed, checked to print and parse back the same, and as a
   sexp, where refs are [<name>]. *)
let show_erased source =
  iter_erased source ~f:(fun name value ->
    let expr = Syntax_value.Expr.of_value value ~loc:Loc.none |> ok_exn in
    printf "%s:\n%s\n" name (Printer.expr expr);
    print_s (Round_trip.strip_locs [%sexp (expr : Syntax.Expr.t)]);
    Round_trip.expr expr)
;;

let%expect_test "an erased expression's value" =
  show_values
    {|let e_lib = Stag.Code.erase .< Core.Sexp.Atom (Core.Int.to_string x) >.|};
  [%expect
    {| e_lib: Stag.Expr.Construct (Stag.Path.Ref <constructor Core.Sexp.Atom>, Cons (Stag.Expr.App (Stag.Expr.Var (Stag.Path.Ref <value Core.Int.to_string>, None), Stag.Expr.Var (Stag.Path.Name "x", None)), Nil)) |}]
;;

let%expect_test
    "what code refers to is erased as refs, its free variables as names"
  =
  show_erased
    {|
let helper x = x
let n = 1
let e_lib = Stag.Code.erase .< Core.Sexp.Atom (Core.Int.to_string x) >.
let e_function = Stag.Code.erase .< fun x -> Core.Sexp.sexp_of_int x >.
let e_prim = Stag.Code.erase .< String.length "a" >.
let e_own = Stag.Code.erase .< helper n >.
let e_poly = Stag.Code.erase .< Core.List.map >.
let e_binders =
  Stag.Code.erase
    .< let y = x in
       match Some y with
       | Some z -> (fun w -> {`a = w; `b = `B z}) y
       | None -> raise (Failure "none")
       | exception Failure s -> helper {`a = s; `b = `C} >.
type r = { f : int }
let e_record = Stag.Code.erase .< fun x -> { f = x.f } >.
let e_nested = Stag.Code.erase .< .< Core.Int.to_string 3 >. >.
let e_type = Stag.Code.erase .< type list[r] >.
|};
  [%expect
    {|
    e_lib:
    Core.Sexp.Atom (Core.Int.to_string x)
    (Construct <Core.Sexp.Atom> ((App (Var <Core.Int.to_string> ()) (Var x ()))))
    e_function:
    fun x -> Core.Sexp.sexp_of_int x
    (Fun ((x ())) (App (Var <Core.Sexp.sexp_of_int> ()) (Var x ())))
    e_prim:
    Prim.String.length "a"
    (App (Var <Prim.String.length> ()) (Lit (String a)))
    e_own:
    Gen.helper Gen.n
    (App (Var <Gen.helper> ()) (Var <Gen.n> ()))
    e_poly:
    Core.List.map
    (Var <Core.List.map> ())
    e_binders:
    let y = x in
    match Some y with
    | Some z -> (fun w -> {`a = w; `b = `B z}) y
    | None -> raise (Failure "none")
    | exception Failure s -> Gen.helper {`a = s; `b = `C}
    (Let y () () (Var x ())
     (Match (Construct <Some> ((Var y ())))
      (((Construct <Some> ((Var z)))
        (App
         (Fun ((w ()))
          (Poly_record ((a (Var w ())) (b (Poly_variant B ((Var z ())))))))
         (Var y ())))
       ((Construct <None> ())
        (App (Var <raise> ()) (Construct <Failure> ((Lit (String none))))))
       ((Exception (Construct <Failure> ((Var s))))
        (App (Var <Gen.helper> ())
         (Poly_record ((a (Var s ())) (b (Poly_variant C ())))))))))
    e_record:
    fun x -> {Gen.f = x.Gen.f}
    (Fun ((x ())) (Record ((<Gen.f> (Field (Var x ()) <Gen.f>)))))
    e_nested:
    .< Core.Int.to_string 3 >.
    (Quote (App (Var <Core.Int.to_string> ()) (Lit (Int 3))))
    e_type:
    type list[Gen.r]
    (Type (Con <list> ((Ty (Con <Gen.r> ())))))
    |}]
;;

(* Explicit substitutions are lets around the code, which are recursive in
   syntax. *)
let%expect_test "binders are renamed only where they would capture" =
  show_erased
    {|
let under_z = .< fun z -> x >.
let e_explicit = Stag.Code.erase .< fun z -> .~under_z.{x = z} >.
let e_bound_in_let = Stag.Code.erase .< fun x -> .~under_z.{x = x} >.
let e_free_in_let = Stag.Code.erase .< .~under_z.{x = x} >.
let e_taken = Stag.Code.erase .< fun x_1 -> .~under_z.{x = (x, x_1)} >.
let e_implicit = Stag.Code.erase .< fun x -> .~under_z >.
let e_shadowing = Stag.Code.erase .< fun x -> fun x -> x >.
|};
  [%expect
    {|
    e_explicit:
    fun z ->
      let x = z in
      fun z -> x
    (Fun ((z ())) (Let x () () (Var z ()) (Fun ((z ())) (Var x ()))))
    e_bound_in_let:
    fun x ->
      let x_1 = x in
      fun z -> x_1
    (Fun ((x ())) (Let x_1 () () (Var x ()) (Fun ((z ())) (Var x_1 ()))))
    e_free_in_let:
    let x_1 = x in
    fun z -> x_1
    (Let x_1 () () (Var x ()) (Fun ((z ())) (Var x_1 ())))
    e_taken:
    fun x_1 ->
      let x_2 = (x, x_1) in
      fun z -> x_2
    (Fun ((x_1 ()))
     (Let x_2 () () (Tuple ((Var x ()) (Var x_1 ())))
      (Fun ((z ())) (Var x_2 ()))))
    e_implicit:
    fun x -> fun z -> x
    (Fun ((x ())) (Fun ((z ())) (Var x ())))
    e_shadowing:
    fun x -> fun x -> x
    (Fun ((x ())) (Fun ((x ())) (Var x ())))
    |}]
;;

let%expect_test "erasing code that embeds a local function is an error" =
  show_erased
    {|
let e_local = let k y = y in Stag.Code.erase .< k 1 >.
|};
  [%expect
    {|
    File "gen.stag", line 2, characters 29-54:
    2 | let e_local = let k y = y in Stag.Code.erase .< k 1 >.
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Stag.Code.erase: the code embeds k, a local from outside it, which nothing names where the code is spliced
    |}]
;;
