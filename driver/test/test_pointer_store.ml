open! Import
open Source_helpers

(* Records are values: a store goes through a pointer, as [p.* <- v] or
   [p.*.x <- v]. *)

let%expect_test "whole pointer stores, by both spellings" =
  run
    {|
let p = Ptr.make 1
let set = p.* <- 2
let postfix = p.*
let set_prefix = *p <- 3
let prefix = p.*
let store_is_unit = (p.* <- 4)
|};
  [%expect
    {|
    val p : ptr[int]
    val set : unit
    val postfix : int
    val set_prefix : unit
    val prefix : int
    val store_is_unit : unit
    -- evaluated at compile time
    -- evaluated
    (p (Ptr 4))
    (set "()")
    (postfix 2)
    (set_prefix "()")
    (prefix 3)
    (store_is_unit "()")
    |}]
;;

let%expect_test "copies of a pointer see its stores" =
  run
    {|
let p = Ptr.make 1
let q = p
let set = q.* <- 5
let through_p = p.*
let cells =
  let pair = (Ptr.make 1, Ptr.make 1) in
  match pair with
  | a, b -> a.* <- 9; (a.*, b.*)
|};
  [%expect
    {|
    val p : ptr[int]
    val q : ptr[int]
    val set : unit
    val through_p : int
    val cells : int * int
    -- evaluated at compile time
    -- evaluated
    (p (Ptr 5))
    (q (Ptr 5))
    (set "()")
    (through_p 5)
    (cells (Tuple 9 1))
    |}]
;;

let%expect_test "storing to a field of a record" =
  run
    {|
type point = {x : int; y : int}
type segment = {from : point; to_ : point}
let p = Ptr.make {x = 1; y = 2}
let before = p.*
let set = p.*.x <- 10
let after = p.*
let unchanged = before.x
let changed = after.x
let s = Ptr.make {from = {x = 0; y = 0}; to_ = {x = 1; y = 1}}
let nested_set = s.*.from.x <- 5
let nested = s.*
|};
  [%expect
    {|
    type point = { x : int; y : int }
    type segment = { from : point; to_ : point }
    val p : ptr[point]
    val before : point
    val set : unit
    val after : point
    val unchanged : int
    val changed : int
    val s : ptr[segment]
    val nested_set : unit
    val nested : segment
    -- evaluated at compile time
    -- evaluated
    (p (Ptr (point (x 10) (y 2))))
    (before (point (x 1) (y 2)))
    (set "()")
    (after (point (x 10) (y 2)))
    (unchanged 1)
    (changed 10)
    (s (Ptr (segment (from (point (x 5) (y 0))) (to_ (point (x 1) (y 1))))))
    (nested_set "()")
    (nested (segment (from (point (x 5) (y 0))) (to_ (point (x 1) (y 1)))))
    |}]
;;

let%expect_test "storing to a field of a polymorphic record" =
  run
    {|
let r = Ptr.make {`x = 1; `y = true}
let before = r.*
let set = r.*.`x <- 2
let after = r.*
|};
  [%expect
    {|
    val r : ptr[{`x : int; `y : bool}]
    val before : {`x : int; `y : bool}
    val set : unit
    val after : {`x : int; `y : bool}
    -- evaluated at compile time
    -- evaluated
    (r (Ptr ((x 2) (y true))))
    (before ((x 1) (y true)))
    (set "()")
    (after ((x 2) (y true)))
    |}]
;;

let%expect_test "the destination is evaluated once, before the value" =
  run
    {|
let p = Ptr.make 0
let q = Ptr.make 0
let mut rhs_done = false
let pick (_ : unit) = match rhs_done with true -> q | false -> p
let set = (pick ()).* <- (rhs_done <- true; 7)
let both = (p.*, q.*)
let mut count = 0
let counted (_ : unit) = count <- Int.add count 1; p
let set_counted = (counted ()).* <- 1
let evaluations = count
|};
  [%expect
    {|
    val p : ptr[int]
    val q : ptr[int]
    val rhs_done : bool
    val pick : unit -> ptr[int]
    val set : unit
    val both : int * int
    val count : int
    val counted : unit -> ptr[int]
    val set_counted : unit
    val evaluations : int
    -- evaluated at compile time
    -- evaluated
    (p (Ptr 1))
    (q (Ptr 0))
    (rhs_done true)
    (pick <fun>)
    (set "()")
    (both (Tuple 7 0))
    (count 1)
    (counted <fun>)
    (set_counted "()")
    (evaluations 1)
    |}]
;;

let%expect_test "the last store of a chain wins" =
  run
    {|
let p = Ptr.make 1
let sets = p.* <- 2; p.* <- 3
let last = p.*
let s = Ptr.make {`a = 1; `b = 2}
let field_sets = s.*.`a <- 3; s.*.`b <- 4
let both = s.*
|};
  [%expect
    {|
    val p : ptr[int]
    val sets : unit
    val last : int
    val s : ptr[{`a : int; `b : int}]
    val field_sets : unit
    val both : {`a : int; `b : int}
    -- evaluated at compile time
    -- evaluated
    (p (Ptr 3))
    (sets "()")
    (last 3)
    (s (Ptr ((a 3) (b 4))))
    (field_sets "()")
    (both ((a 3) (b 4)))
    |}]
;;

let%expect_test "only pointers can be stored to" =
  run
    {|
type point = {x : int; y : int}
let r = {x = 1; y = 2}
let bad = r.x <- 3
|};
  [%expect
    {| t.stag:4:10-13: only a field of a dereference is mutable, as in [p.*.x <- v]: records are values, and are mutated through pointers |}]
;;

let%expect_test "a store is typechecked against the pointee" =
  run {|
let p = Ptr.make 1
let bad = p.* <- true
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:17-21)
    |}]
;;

(* Calls [f] on the erased [e_*] toplevels of [source], once evaluated. *)
let iter_erased source ~f =
  match
    Driver.check_files ~limits [ { path = "gen.stag"; contents = source } ]
  with
  | Error error ->
    print_string
      (Diagnostic.list_to_string_hum
         ~contents:(fun _ -> Some source)
         (List.map error ~f:Diagnostic.of_driver_error))
  | Ok driver ->
    (match Driver.eval ~limits driver with
     | Error error -> print_s [%sexp (error : Error.t)]
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

let show_erased source =
  iter_erased source ~f:(fun name value ->
    let expr =
      Interpreter.Value.to_syntax value Generated_syntax.Expr ~loc:Loc.none
      |> ok_exn
    in
    printf "%s:\n%s\n" name (Printer.expr expr);
    Round_trip.expr expr)
;;

let%expect_test "a store is checked in quotes, and erases back to syntax" =
  run
    {|
let quoted = .< (Ptr.make 1).* <- 2 >.
let ran = .~quoted
let erased = Stag.Code.erase .< (Ptr.make 1).* <- 2 >.
|};
  [%expect
    {|
    val quoted : <unit>{..'a}
    val ran : unit
    val erased : Stag.Expr.t
    -- evaluated at compile time
    (quoted
     (Code
      (Store
       (Deref
        (App
         (Var (Persistent (Toplevel prelude.Ptr.make/15)) : "int -> ptr[int]")
         (Lit (Int 1) : int) : ptr[int])
        : int)
       (Lit (Int 2) : int) : unit)))
    -- evaluated
    (quoted
     (Code
      (Store
       (Deref
        (App
         (Var (Persistent (Toplevel prelude.Ptr.make/15)) : "int -> ptr[int]")
         (Lit (Int 1) : int) : ptr[int])
        : int)
       (Lit (Int 2) : int) : unit)))
    (ran "()")
    (erased
     (Stag.Expr.Store
      (Ptr
       (Tuple
        (Stag.Expr.Deref
         (Ptr
          (Stag.Expr.App
           (Ptr
            (Tuple
             (Stag.Expr.Var (Stag.Path.Ref (Ref (value Ptr.make)))
              ((mode (None)) (ty (None))))
             (Stag.Expr.Lit (Stag.Lit.Int 1)))))))
        (Stag.Expr.Lit (Stag.Lit.Int 2))))))
    |}];
  show_erased
    {|type point = {x : int; y : int}
let e_store = Stag.Code.erase .< (Ptr.make 1).* <- 2 >.
let e_field = Stag.Code.erase .< (Ptr.make {x = 1; y = 2}).*.x <- 3 >.|};
  [%expect
    {|
    e_store:
    (.Ptr.make 1).* <- 2
    e_field:
    (.Ptr.make {.Gen.x = 1; .Gen.y = 2}).*.(.Gen.x) <- 3
    |}]
;;

let%expect_test "a store built by reflection" =
  run
    {|
let cell = Ptr.make 0
let store =
  Stag.Expr.Store
    (Ptr.make ( (Stag.Expr.Deref (Ptr.make (Stag.Expr.var "cell")), Stag.Expr.int 5)))
include .~(Cons (Box.make ( (Stag.Toplevel.let_ "run_store" Nil store, Nil))))
let stored = cell.*
|};
  [%expect
    {|
    val cell : ptr[int]
    val store : Stag.Expr.t
    val run_store : unit
    val stored : int
    -- evaluated at compile time
    (store
     (Stag.Expr.Store
      (Ptr
       (Tuple
        (Stag.Expr.Deref
         (Ptr
          (Stag.Expr.Var (Stag.Path.Name (String cell))
           ((mode (None)) (ty (None))))))
        (Stag.Expr.Lit (Stag.Lit.Int 5))))))
    -- evaluated
    (cell (Ptr 5))
    (store
     (Stag.Expr.Store
      (Ptr
       (Tuple
        (Stag.Expr.Deref
         (Ptr
          (Stag.Expr.Var (Stag.Path.Name (String cell))
           ((mode (None)) (ty (None))))))
        (Stag.Expr.Lit (Stag.Lit.Int 5))))))
    (run_store "()")
    (stored 5)
    |}]
;;

let%expect_test "a field store keeps what the value stored to another field" =
  run
    {|
type point = {x : int; y : int}
let p = Ptr.make {x = 1; y = 2}
let set = p.*.x <- (p.*.y <- 1; 2)
let after = p.*
|};
  [%expect
    {|
    type point = { x : int; y : int }
    val p : ptr[point]
    val set : unit
    val after : point
    -- evaluated at compile time
    -- evaluated
    (p (Ptr (point (x 2) (y 1))))
    (set "()")
    (after (point (x 2) (y 1)))
    |}]
;;

let%expect_test "a nested pointer's store goes to the cell it designated" =
  run
    {|
type cell = {value : int}
type node = {next : ptr[cell]; label : int}
let leaf = Ptr.make {value = 0}
let repl = Ptr.make {value = 2}
let orig = Ptr.make {next = leaf; label = 1}
let set = orig.*.next.*.value <- (orig.*.next <- repl; 99)
let leaf_value = leaf.*.value
let repl_value = repl.*.value
let now = orig.*.next.*.value
|};
  [%expect
    {|
    type cell = { value : int }
    type node = { next : ptr[cell]; label : int }
    val leaf : ptr[cell]
    val repl : ptr[cell]
    val orig : ptr[node]
    val set : unit
    val leaf_value : int
    val repl_value : int
    val now : int
    -- evaluated at compile time
    -- evaluated
    (leaf (Ptr (cell (value 99))))
    (repl (Ptr (cell (value 2))))
    (orig (Ptr (node (next (Ptr (cell (value 2)))) (label 1))))
    (set "()")
    (leaf_value 99)
    (repl_value 2)
    (now 2)
    |}]
;;

let%expect_test "a field store applies its path to the contents at write time" =
  run
    {|
type point = {x : int; y : int}
let q = Ptr.make {x = 0; y = 0}
let set = q.*.x <- (q.* <- {x = 1; y = 9}; 3)
let after = q.*
|};
  [%expect
    {|
    type point = { x : int; y : int }
    val q : ptr[point]
    val set : unit
    val after : point
    -- evaluated at compile time
    -- evaluated
    (q (Ptr (point (x 3) (y 9))))
    (set "()")
    (after (point (x 3) (y 9)))
    |}]
;;
