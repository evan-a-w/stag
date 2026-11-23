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
      (Diagnostic.list_to_string_hum
         ~contents:(fun _ -> Some source)
         (List.map error ~f:Diagnostic.of_driver_error))
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
               (Interpreter.find (Driver.interpreter driver) ~ident:member.ident
                |> ok_exn))))
;;

let show_values source =
  iter_erased source ~f:(fun name value ->
    printf "%s: %s\n" name (Interpreter.Value.to_string_hum value))
;;

(* As syntax, printed, checked to print and parse back the same, and as a sexp,
   where refs are [<name>]. *)
let show_erased source =
  iter_erased source ~f:(fun name value ->
    let expr =
      Interpreter.Value.to_syntax value Generated_syntax.Expr ~loc:Loc.none
      |> ok_exn
    in
    printf "%s:\n%s\n" name (Printer.expr expr);
    print_s (Round_trip.strip_locs [%sexp (expr : Syntax.Expr.t)]);
    Round_trip.expr expr)
;;

let%expect_test "an erased expression's value" =
  show_values
    {|let e_lib = Stag.Code.erase .< Core.Sexp.Atom (Core.Int.to_string x) >.|};
  [%expect
    {| e_lib: Stag.Expr.Construct (Stag.Path.Ref <constructor Core.Sexp.Atom>, Cons (Ptr.make (Stag.Expr.App (Ptr.make (Stag.Expr.Var (Stag.Path.Ref <value Core.Int.to_string>, {`mode = None; `ty = None}), Stag.Expr.Var (Stag.Path.Name "x", {`mode = None; `ty = None}))), Nil))) |}]
;;

let%expect_test "what code refers to is erased as refs, its free variables as \
                 names"
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
    .Core.Sexp.Atom (.Core.Int.to_string x)
    (Construct <Core.Sexp.Atom> ((App (Var <Core.Int.to_string> ()) (Var x ()))))
    e_function:
    fun x -> .Core.Sexp.sexp_of_int x
    (Fun ((x ())) (App (Var <Core.Sexp.sexp_of_int> ()) (Var x ())))
    e_prim:
    .String.length "a"
    (App (Var <Prim.String.length> ()) (Lit (String a)))
    e_own:
    .Gen.helper .Gen.n
    (App (Var <Gen.helper> ()) (Var <Gen.n> ()))
    e_poly:
    .Core.List.map
    (Var <Core.List.map> ())
    e_binders:
    let y = x in
    match .Some y with
    | .Some z -> (fun w -> {`a = w; `b = `B z}) y
    | .None -> .raise (.Failure "none")
    | exception .Failure s -> .Gen.helper {`a = s; `b = `C}
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
    fun x -> {.Gen.f = x.(.Gen.f)}
    (Fun ((x ())) (Record ((<Gen.f> (Field (Var x ()) <Gen.f>)))))
    e_nested:
    .< .Core.Int.to_string 3 >.
    (Quote (App (Var <Core.Int.to_string> ()) (Lit (Int 3))))
    e_type:
    type .list[.Gen.r]
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
    fun z -> fun z_1 -> z
    (Fun ((z ())) (Fun ((z_1 ())) (Var z ())))
    e_bound_in_let:
    fun x -> fun z -> x
    (Fun ((x ())) (Fun ((z ())) (Var x ())))
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
  show_erased {|
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

let gen =
  ( "gen.stag"
  , {|
let value name params code = Stag.Toplevel.let_ name params (Stag.Code.erase code)

let sexp_of_pair =
  .< fun p ->
       match p with
       | (a, b) ->
         Core.Sexp.List
           (Cons (Ptr.make ( (Core.Sexp.Atom (Core.Int.to_string a), Cons (Ptr.make ( (Core.Sexp.sexp_of_int b, Nil))))))) >.

let to_string_x = .< Core.Int.to_string x >.

let derive =
  Cons (Ptr.make ( (value "sexp_of_pair" Nil sexp_of_pair,
  Cons (Ptr.make ( (value "show" Nil .< fun s -> Core.Sexp.to_string s >.,
  Cons (Ptr.make ( (value "local" (Cons (Ptr.make ( ("to_string", Nil)))) .< Core.Int.to_string to_string >.,
  Cons (Ptr.make ( (value "helped" Nil .< H.helper 1 >.,
  Cons (Ptr.make ( (value "map" Nil .< Core.List.map >.,
  Cons (Ptr.make ( (value "code" Nil .< .< Core.Int.to_string 3 >. >., Nil))))))))))))))))))
|}
  )
;;

let h = "h.stag", {|let helper x = Core.Int.succ x|}

let%expect_test "generated code refers to what its quotes did, wherever spliced"
  =
  check_files
    ~run:true
    [ gen
    ; h
    ; ( "a.stag"
      , {|
module Core = struct let x = 1 end
module Sexp = struct end
module List = struct end
module Int = struct let to_string x = "shadowed" end
module H = struct end
type list['a] = Nil | Cons of 'a
let to_string x = "shadowed"
let sexp_of_int x = x
include .~(Gen.derive)
let%expect_test "derived" =
  print_endline (show (sexp_of_pair (1, 2)));
  print_endline (local 3);
  print_endline (show (sexp_of_pair (helped, 0)));
  print_endline (.~code);
  map (fun x -> x) (Cons (Ptr.make ( (1, Nil))));
  map (fun x -> x) (Cons (Ptr.make ( ("a", Nil))));
  [%expect]
|}
      )
    ];
  [%expect
    {xxx|
    module Gen : sig
      val value : string -> list[string] -> <'a>{..'b} -> Stag.Toplevel.t
      val sexp_of_pair : <int * int -> Core.Sexp.t>{..'a}
      val to_string_x : <string>{x : int; ..'a}
      val derive : list[Stag.Toplevel.t]
    end
    module H : sig
      val helper : int -> int
    end
    module A : sig
      module Core : sig
        val x : int
      end
      module Sexp : sig end
      module List : sig end
      module Int : sig
        val to_string : 'a -> string
      end
      module H : sig end
      type list['a] = Nil | Cons of 'a
      val to_string : 'a -> string
      val sexp_of_int : 'a -> 'a
      val sexp_of_pair : int * int -> .Core.Sexp.t
      val show : .Core.Sexp.t -> string
      val local : int -> string
      val helped : int
      val map : ('a -> 'b) -> .list['a] -> .list['b]
      val code : <string>{..'a}
    end
    --- output ---
    ((name (derived @ a.stag:11:16-25))
     (mismatches
      (((loc a.stag:18:10-10) (expected ("")) (actual  "(1 2)\
                                                      \n3\
                                                      \n(2 0)\
                                                      \n3"))))
     (error ()))
    --- corrected ---

    module Core = struct let x = 1 end
    module Sexp = struct end
    module List = struct end
    module Int = struct let to_string x = "shadowed" end
    module H = struct end
    type list['a] = Nil | Cons of 'a
    let to_string x = "shadowed"
    let sexp_of_int x = x
    include .~(Gen.derive)
    let%expect_test "derived" =
      print_endline (show (sexp_of_pair (1, 2)));
      print_endline (local 3);
      print_endline (show (sexp_of_pair (helped, 0)));
      print_endline (.~code);
      map (fun x -> x) (Cons (Ptr.make ( (1, Nil))));
      map (fun x -> x) (Cons (Ptr.make ( ("a", Nil))));
      [%expect {|
        (1 2)
        3
        (2 0)
        3
        |}]
    |xxx}]
;;

let%expect_test "refs to toplevels of the splice's own file" =
  check_files
    ~run:true
    [ ( "a.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
let before = 1
module M = struct
  include .~(value "g" .< "generated" >.)
  let decls = value "of_g" .< (before, after, g, Inner.z) >.
  module Inner = struct let z = 'z' end
end
module Inner = struct end
let g = "outer g"
include .~(M.decls)
let after = "after"
let%expect_test "values" =
  match of_g with
  | (a, b, c, d) ->
    print_endline
      (Core.String.concat ", "
        (Cons (Ptr.make ( (Core.Int.to_string a, Cons (Ptr.make ( (b, Cons (Ptr.make ( (c, Cons (Ptr.make ( (Core.Char.to_string d, Nil))))))))))))));
  [%expect]
|}
      )
    ];
  [%expect
    {xxx|
    module A : sig
      val value : string -> <'a>{..'b} -> list[Stag.Toplevel.t]
      val before : int
      module M : sig
        val g : string
        val decls : list[Stag.Toplevel.t]
        module Inner : sig
          val z : char
        end
      end
      module Inner : sig end
      val g : string
      val of_g : int * string * string * char
      val after : string
    end
    --- output ---
    ((name (values @ a.stag:13:16-24))
     (mismatches
      (((loc a.stag:19:10-10) (expected ("")) (actual "1, after, generated, z"))))
     (error ()))
    --- corrected ---

    let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
    let before = 1
    module M = struct
      include .~(value "g" .< "generated" >.)
      let decls = value "of_g" .< (before, after, g, Inner.z) >.
      module Inner = struct let z = 'z' end
    end
    module Inner = struct end
    let g = "outer g"
    include .~(M.decls)
    let after = "after"
    let%expect_test "values" =
      match of_g with
      | (a, b, c, d) ->
        print_endline
          (Core.String.concat ", "
            (Cons (Ptr.make ( (Core.Int.to_string a, Cons (Ptr.make ( (b, Cons (Ptr.make ( (c, Cons (Ptr.make ( (Core.Char.to_string d, Nil))))))))))))));
      [%expect {| 1, after, generated, z |}]
    |xxx}]
;;

let%expect_test "a ref can't name what a later include generates" =
  check_files
    [ ( "a.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
include .~(value "h" .< g >.)
include .~(value "g" .< 1 >.)
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 3, characters 0-29:
    3 | include .~(value "h" .< g >.)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let h` generated by this splice: Unbound value g
    File "a.stag", line 4, characters 0-29:
    4 | include .~(value "g" .< 1 >.)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include generates it, but it is visible only after the include
    Note: The generated declaration is:
      let h = g
    |}]
;;

let%expect_test "a ref to a function that uses what the running include \
                 generates"
  =
  check_files
    [ ( "a.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
include .~(value "h" .< f >.)
let f x = h
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 4, characters 10-11:
    4 | let f x = h
                  ^
    Error: Unbound value h
    File "a.stag", line 3, characters 0-29:
    3 | include .~(value "h" .< f >.)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}]
;;

let%expect_test "a ref to a value that uses what the include generates" =
  check_files
    ~run:true
    [ ( "a.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
include .~(value "h" .< v >.)
let v = Core.Int.succ h
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 4, characters 8-21:
    4 | let v = Core.Int.succ h
                ^^^^^^^^^^^^^
    Error: Core.Int.succ can't be resolved here: the include at a.stag:3:0-29 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 3, characters 0-29:
    3 | include .~(value "h" .< v >.)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}]
;;

let%expect_test "errors in generated code print refs as paths" =
  check_files
    [ gen
    ; h
    ; ( "a.stag"
      , {|
module Core = struct end
include .~(Cons (Ptr.make ( (Stag.Toplevel.let_ "bad" Nil
  (Stag.Expr.Let (Ptr.make ( ("x", Nil, Stag.Annot.none, Stag.Expr.string "s", Stag.Code.erase Gen.to_string_x)))), Nil))))
|}
      )
    ];
  [%expect
    {|
    File "a.stag", lines 3-4, characters 0-123:
    3 | include .~(Cons (Ptr.make ( (Stag.Toplevel.let_ "bad" Nil
    4 |   (Stag.Expr.Let (Ptr.make ( ("x", Nil, Stag.Annot.none, Stag.Expr.string "s", Stag.Code.erase Gen.to_string_x)))), Nil))))
    Error: In the declaration `let bad` generated by this splice: This expression has type string but an expression was expected of type int
    Note: The generated declaration is:
      let bad =
        let x = "s" in
        .Core.Int.to_string x
    |}]
;;

let%expect_test "erasing and naming refs at run time" =
  check_files
    ~run:true
    [ ( "a.stag"
      , {|
let name e =
  match e with
  | Stag.Expr.App * (Stag.Expr.Var (Stag.Path.Ref r, _), _) -> Stag.Value_ref.name r
  | _ -> "?"
let%expect_test "names" =
  print_endline (name (Stag.Code.erase .< Core.Int.to_string x >.));
  print_endline (name (Stag.Code.erase .< String.length x >.));
  [%expect]
|}
      )
    ];
  [%expect
    {xxx|
    module A : sig
      val name : Stag.Expr.t -> string
    end
    --- output ---
    ((name (names @ a.stag:6:16-23))
     (mismatches
      (((loc a.stag:9:10-10) (expected ("")) (actual  "to_string\
                                                     \nlength"))))
     (error ()))
    --- corrected ---

    let name e =
      match e with
      | Stag.Expr.App * (Stag.Expr.Var (Stag.Path.Ref r, _), _) -> Stag.Value_ref.name r
      | _ -> "?"
    let%expect_test "names" =
      print_endline (name (Stag.Code.erase .< Core.Int.to_string x >.));
      print_endline (name (Stag.Code.erase .< String.length x >.));
      [%expect {|
        to_string
        length
        |}]
    |xxx}]
;;

let%expect_test "a renamed binder still provides the implicit splices it did" =
  show_erased
    {|
let e_provider = Stag.Code.erase .< fun c -> .< fun x -> (.~c, .< x >.) >. >.
let e_nested = Stag.Code.erase .< fun c -> .< fun x -> .< (.~(.~c), .< x >.) >. >. >.
let e_kept = Stag.Code.erase .< fun c -> .< fun x -> .~c >. >.
let e_reversed = Stag.Code.erase .< fun x -> fun y -> .~(.< (x, y) >.).{y = x; x = y} >.
|};
  [%expect
    {|
    e_provider:
    fun c -> .< fun x_1 -> (.~c.{x = x_1}, .< x >.) >.
    (Fun ((c ()))
     (Quote
      (Fun ((x_1 ()))
       (Tuple
        ((Splice (expr (Var c ())) (subs (((x ((Var x_1 ())))))))
         (Quote (Var x ())))))))
    e_nested:
    fun c -> .< fun x_1 -> .< (.~(.~c.{x = x_1}), .< x >.) >. >.
    (Fun ((c ()))
     (Quote
      (Fun ((x_1 ()))
       (Quote
        (Tuple
         ((Splice (expr (Splice (expr (Var c ())) (subs (((x ((Var x_1 ()))))))))
           (subs ()))
          (Quote (Var x ()))))))))
    e_kept:
    fun c -> .< fun x -> .~c >.
    (Fun ((c ())) (Quote (Fun ((x ())) (Splice (expr (Var c ())) (subs ())))))
    e_reversed:
    fun x -> fun y -> (y, x)
    (Fun ((x ())) (Fun ((y ())) (Tuple ((Var y ()) (Var x ())))))
    |}];
  let gen =
    ( "gen.stag"
    , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
let c1 = .< fun c -> .< fun x -> (.~c, .< x >.) >. >.
|}
    )
  in
  check_files [ gen; "a.stag", {|include .~(Gen.value "shown" Gen.c1)|} ];
  [%expect
    {|
    module Gen : sig
      val value : string -> <'a>{..'b} -> list[Stag.Toplevel.t]
      val c1 : <<'a>{x?'b : immut 'c; ..'d} -> <'c -> 'a * <'e>{x : 'e; ..'f}>{..'g}>{..'h} where ..'d <= {..'g}
    end
    module A : sig
      val shown : <'a>{x?'b : immut 'c} -> <'c -> 'a * <'d>{x : 'd; ..'e}>{..'f}
    end
    |}]
;;

let%expect_test "code built from parameters, erased at run time" =
  show_erased
    {|
let around c = Stag.Code.erase .< fun x -> (x, .~c) >.
let e_free = around .< x >.
let e_closed = around .< 1 >.
let twice c = .< (.~c, .~c) >.
let e_twice = Stag.Code.erase (twice .< fun x -> x >.)
|};
  [%expect
    {|
    e_free:
    fun x -> (x, x)
    (Fun ((x ())) (Tuple ((Var x ()) (Var x ()))))
    e_closed:
    fun x -> (x, 1)
    (Fun ((x ())) (Tuple ((Var x ()) (Lit (Int 1)))))
    e_twice:
    ((fun x -> x), fun x -> x)
    (Tuple ((Fun ((x ())) (Var x ())) (Fun ((x ())) (Var x ()))))
    |}]
;;

let%expect_test "annotations are kept, their types as refs" =
  show_erased
    {|
type r = { f : int }
let e_fun = Stag.Code.erase .< (fun x -> x) (fun (y : int) -> y) >.
let e_let =
  Stag.Code.erase
    .< let f (x : option[r]) (y : 'a) : list['a] = Cons (Ptr.make ( (y, Nil))) in
       (f : option[r] -> int -> list[int]) >.
|};
  [%expect
    {|
    e_fun:
    (fun x -> x) (fun (y : .int) -> y)
    (App (Fun ((x ())) (Var x ())) (Fun ((y ((ty (Con <int> ()))))) (Var y ())))
    e_let:
    let f (x : .option[.Gen.r]) (y : 'a) : .list['a] = [y] in
    (f : .option[.Gen.r] -> .int -> .list[.int])
    (Let f ((x ((ty (Con <option> ((Ty (Con <Gen.r> ()))))))) (y ((ty (Var a)))))
     ((ty (Con <list> ((Ty (Var a))))))
     (Construct <Cons>
      ((App (Var <Ptr.make> ()) (Tuple ((Var y ()) (Construct <Nil> ()))))))
     (Var f
      ((ty
        (Arrow (Con <option> ((Ty (Con <Gen.r> ()))))
         (Arrow (Con <int> ()) (Con <list> ((Ty (Con <int> ()))))))))))
    |}];
  check_files
    [ ( "gen.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
type r = { f : int }
let v = .< (fun x -> x) (fun (y : int) -> y) >.
let w =
  .< let f (x : option[r]) (y : 'a) : list['a] = Cons (Ptr.make ( (y, Nil))) in
     (f : option[r] -> int -> list[int]) >.
|}
      )
    ; ( "a.stag"
      , {|
type int = bool
type option['a] = int
type list['a] = int
type r = int
include .~(Gen.value "v" Gen.v)
include .~(Gen.value "w" Gen.w)
|}
      )
    ];
  [%expect
    {|
    module Gen : sig
      val value : string -> <'a>{..'b} -> list[Stag.Toplevel.t]
      type r = { f : int }
      val v : <int -> int>{..'a}
      val w : <option[r] -> int -> list[int]>{..'a}
    end
    module A : sig
      type int = bool
      type option['a] = bool
      type list['a] = bool
      type r = bool
      val v : .int -> .int
      val w : .option[Gen.r] -> .int -> .list[.int]
    end
    |}]
;;

let%expect_test "a weak toplevel, referred to at two types" =
  check_files
    [ ( "gen.stag"
      , {|
let value name code = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Code.erase code), Nil)))
let w = (fun x -> x) (fun x -> x)
|}
      )
    ; ( "a.stag"
      , {|
include .~(Gen.value "u1" .< Gen.w 1 >.)
include .~(Gen.value "u2" .< Gen.w "a" >.)
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 3, characters 0-42:
    3 | include .~(Gen.value "u2" .< Gen.w "a" >.)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let u2` generated by this splice: This expression has type string but an expression was expected of type int
    Note: The generated declaration is:
      let u2 = .Gen.w "a"
    |}]
;;

let%expect_test "printed refs read back, past binders and shadowing" =
  show_erased
    {|
let e_raise = Stag.Code.erase .< fun raise -> (raise 1, Int.to_string (String.length "ab"), fun x -> .raise (Failure x)) >.
|};
  [%expect
    {|
    e_raise:
    fun raise ->
      (raise 1, .Int.to_string (.String.length "ab"), fun x -> .raise (.Failure x))
    (Fun ((raise ()))
     (Tuple
      ((App (Var raise ()) (Lit (Int 1)))
       (App (Var <Prim.Int.to_string> ())
        (App (Var <Prim.String.length> ()) (Lit (String ab))))
       (Fun ((x ())) (App (Var <raise> ()) (Construct <Failure> ((Var x ()))))))))
    |}]
;;

let%expect_test "the printed refs read back where their names are shadowed" =
  check_files
    ~run:true
    [ ( "a.stag"
      , {|
module String = struct end
module Int = struct end
let raise = 1
exception Failure
let f =
  fun raise ->
    (raise 1, .Int.to_string (.String.length "ab"), fun x -> .raise (.Failure x))
let shown = match f (fun n -> n) with | (_, s, _) -> print_endline s
|}
      )
    ];
  [%expect
    {|
    module A : sig
      module String : sig end
      module Int : sig end
      val raise : int
      exception Failure
      val f : (int -> 'a) -> 'a * string * (string -> 'b)
      val shown : unit
    end
    --- output ---
    2
    |}]
;;

let%expect_test "list literals are erased as the prelude's Cons and Nil" =
  show_erased
    {|
type list['a] = Nil | Cons of 'a
let e_literal = Stag.Code.erase .< match [x; [1]] with [y; []] -> [] | _ -> [y] >.
|};
  [%expect
    {|
    e_literal:
    match [x; [1]] with
    | [y; []] -> []
    | _ -> [y]
    (Match
     (Construct <Cons>
      ((App (Var <Ptr.make> ())
        (Tuple
         ((Var x ())
          (Construct <Cons>
           ((App (Var <Ptr.make> ())
             (Tuple
              ((Construct <Cons>
                ((App (Var <Ptr.make> ())
                  (Tuple ((Lit (Int 1)) (Construct <Nil> ()))))))
               (Construct <Nil> ())))))))))))
     (((Construct <Cons>
        ((Deref
          (Tuple
           ((Var y)
            (Construct <Cons>
             ((Deref (Tuple ((Construct <Nil> ()) (Construct <Nil> ())))))))))))
       (Construct <Nil> ()))
      (Any
       (Construct <Cons>
        ((App (Var <Ptr.make> ()) (Tuple ((Var y ()) (Construct <Nil> ())))))))))
    |}]
;;
