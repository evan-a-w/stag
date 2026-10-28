open! Import

(* [Core.Sexp.derive]'s errors, at the splice, and what it generates across
   modules and files. Its expansions are in [tests/core_derive_sexp.stag]. *)

(* Without the lines locating the raise within [core/sexp.stag]. *)
let without_core_location diagnostic =
  String.split_lines diagnostic
  |> List.take_while ~f:(fun line ->
    not (String.is_prefix line ~prefix:"File \"<core>"))
  |> List.map ~f:(fun line -> line ^ "\n")
  |> String.concat
;;

let check_files ?run files =
  File_check.check_files ?run ~filter_diagnostic:without_core_location files
;;

let check ?run contents = check_files ?run [ "a.stag", contents ]

let%expect_test "functions have no conversion" =
  check
    {|
type t = {f : int -> int}
include .~(Core.Sexp.derive (type t))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-37:
    3 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: functions have no sexp conversion"
    |}]
;;

let%expect_test "unsupported types" =
  check
    {|
type t = {c : <int>{}}
include .~(Core.Sexp.derive (type t))
|};
  check
    {|
type r[{..'r}] = {`a : int; ..'r}
type t = {r : r[{`b : int}]}
include .~(Core.Sexp.derive (type t))
|};
  check
    {|
type t = {r : {`a : int}}
include .~(Core.Sexp.derive (type t))
|};
  check
    {|
type t = |
include .~(Core.Sexp.derive (type t))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-37:
    3 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: code has no sexp conversion"
    File "a.stag", line 4, characters 0-37:
    4 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: row arguments are not supported"
    File "a.stag", line 3, characters 0-37:
    3 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: polymorphic records are not supported"
    File "a.stag", line 3, characters 0-37:
    3 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: empty variants are not supported"
    |}]
;;

let%expect_test "abstract types" =
  check
    {|
type t
include .~(Core.Sexp.derive (type t))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-37:
    3 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: t is abstract"
    |}]
;;

let%expect_test "open polymorphic variants" =
  check
    {|
type t[{..'r}] = [`A; ..'r]
include .~(Core.Sexp.derive (type t[{`B : unit}]))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-50:
    3 | include .~(Core.Sexp.derive (type t[{`B : unit}]))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: row parameters are not supported"
    |}]
;;

let%expect_test "not a declared type" =
  check
    {|
include .~(Core.Sexp.derive (type 'a))
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-38:
    2 | include .~(Core.Sexp.derive (type 'a))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: a declared type is needed, as in [derive (type t)]"
    |}]
;;

let%expect_test "a type named sexp" =
  check
    {|
type sexp = S
include .~(Core.Sexp.derive (type sexp))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-40:
    3 | include .~(Core.Sexp.derive (type sexp))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: a type named sexp would have two conversions named sexp_of_sexp"
    |}]
;;

let%expect_test "the prelude's types" =
  check
    {|
include .~(Core.Sexp.derive (type option[int]))
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-47:
    2 | include .~(Core.Sexp.derive (type option[int]))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.derive: option is the prelude's"
    |}]
;;

let%expect_test "a type without its referenced type's converters" =
  check
    {|
type u = A
type t = B of u
include .~(Core.Sexp.derive (type t))
|};
  [%expect
    {|
    File "a.stag", line 4, characters 0-37:
    4 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let sexp_of_t` generated by this splice: Unbound value .A.sexp_of_u
    Note: The generated declaration is:
      let sexp_of_t v =
        match v with
        | .A.B v0 -> .Core.Sexp.List [.Core.Sexp.Atom "B"; .A.sexp_of_u v0]
    File "a.stag", line 4, characters 0-37:
    4 | include .~(Core.Sexp.derive (type t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let t_of_sexp_exn` generated by this splice: Unbound value .A.u_of_sexp_exn
    Note: The generated declaration is:
      let t_of_sexp_exn sexp =
        match .Core.Sexp.Deriving.capitalize sexp with
        | .Core.Sexp.List [.Core.Sexp.Atom "B"; v0] -> .A.B (.A.u_of_sexp_exn v0)
        | _ -> .Core.Sexp.Deriving.variant_error "t_of_sexp" [] ["B"] sexp
    |}]
;;

let%expect_test "types of another file, beside a local module of its name" =
  check_files
    ~run:true
    [ ( "b.stag"
      , {|
module M = struct
  type u = U
  include .~(Core.Sexp.derive (type u))
end
|}
      )
    ; ( "a.stag"
      , {|
module B = struct end
type t = {u : .B.M.u}
include .~(Core.Sexp.derive (type t))
let main = print_endline (Core.Sexp.to_string (sexp_of_t {u = .B.M.U}))
|}
      )
    ];
  [%expect
    {|
    module B : sig
      module M : sig
        type u = U
        val sexp_of_u : u -> Core.Sexp.t
        val u_of_sexp_exn : Core.Sexp.t -> u
        val u_of_sexp : Core.Sexp.t -> result[u, Core.Sexp.Of_sexp_error.t]
      end
    end
    module A : sig
      module B : sig end
      type t = { u : .B.M.u }
      val sexp_of_t : t -> Core.Sexp.t
      val t_of_sexp_exn : Core.Sexp.t -> t
      val t_of_sexp : Core.Sexp.t -> result[t, Core.Sexp.Of_sexp_error.t]
      val main : unit
    end
    --- output ---
    ((u U))
    |}]
;;

(* Types other than the derived one are named by their root paths, so [M.t]
   may be derived outside [M]. *)
let%expect_test "deriving a type of another module" =
  check
    ~run:true
    {|
module M = struct
  type u = U
  type t = T of u
  include .~(Core.Sexp.derive (type u))
end
include .~(Core.Sexp.derive (type M.t))
let main = print_endline (Core.Sexp.to_string (sexp_of_t (M.T M.U)))
|};
  [%expect
    {|
    module A : sig
      module M : sig
        type u = U
        type t = T of u
        val sexp_of_u : u -> Core.Sexp.t
        val u_of_sexp_exn : Core.Sexp.t -> u
        val u_of_sexp : Core.Sexp.t -> result[u, Core.Sexp.Of_sexp_error.t]
      end
      val sexp_of_t : M.t -> Core.Sexp.t
      val t_of_sexp_exn : Core.Sexp.t -> M.t
      val t_of_sexp : Core.Sexp.t -> result[M.t, Core.Sexp.Of_sexp_error.t]
      val main : unit
    end
    --- output ---
    (T U)
    |}]
;;

let%expect_test "conversions of type expressions that have none" =
  let check_type ?(generator = "sexp_of") ty =
    check
      (sprintf
         {|
include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.%s (type %s))])
|}
         generator
         ty)
  in
  check_type "(int -> int)";
  check_type ~generator:"of_sexp" "<int>{}";
  check_type ~generator:"of_sexp_raising" "[`A; ..'r]";
  check_type "Stag.Expr.t";
  [%expect
    {|
    File "a.stag", line 2, characters 0-79:
    2 | include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.sexp_of (type (int -> int)))])
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.sexp_of: functions have no sexp conversion"
    File "a.stag", line 2, characters 0-74:
    2 | include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.of_sexp (type <int>{}))])
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.of_sexp: code has no sexp conversion"
    File "a.stag", line 2, characters 0-85:
    2 | include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.of_sexp_raising (type [`A; ..'r]))])
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.of_sexp_raising: open polymorphic variants are not supported"
    File "a.stag", line 2, characters 0-78:
    2 | include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.sexp_of (type Stag.Expr.t))])
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "Sexp.sexp_of: no sexp conversion of Stag.Expr.t"
    |}]
;;

let%expect_test "a type variable's conversion is needed where it is spliced" =
  check
    {|
include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.sexp_of (type list['a]))])
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-75:
    2 | include .~([Stag.Toplevel.let_ "f" [] (Core.Sexp.sexp_of (type list['a]))])
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let f` generated by this splice: Unbound value _of_a
    Note: The generated declaration is:
      let f = .Core.Sexp.sexp_of_list _of_a
    |}]
;;
