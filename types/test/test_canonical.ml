open! Import
open Type

(* [canonical] of the first of [vars], each linked to the next, printing the
   variables before and after, and whether calling it again leaves the first's
   (now direct) link as it was. *)
let compress vars ~canonical ~var ~sexp_of_var ~sexp_of =
  let show () = print_s [%sexp (vars : var list)] in
  let first = List.hd_exn vars in
  show ();
  print_s (sexp_of (canonical (var first)));
  show ();
  let state = (first : _ Var.t).state in
  ignore (canonical (var first) : _);
  print_s [%sexp "unchanged", (phys_equal state first.state : bool)]
;;

let%expect_test "canonical_ty" =
  let c = Var.create 2 (Link (TCon ("int", []))) in
  let b = Var.create 1 (Link (TVar c)) in
  let a = Var.create 0 (Link (TVar b)) in
  compress
    [ a; b; c ]
    ~canonical:canonical_ty
    ~var:(fun v -> TVar v)
    ~sexp_of_var:[%sexp_of: (ty, Nothing.t) Var.t]
    ~sexp_of:[%sexp_of: ty];
  [%expect
    {|
    ((Link (TVar (Link (TVar (Link (TCon int ()))))))
     (Link (TVar (Link (TCon int ())))) (Link (TCon int ())))
    (TCon int ())
    ((Link (TCon int ())) (Link (TCon int ())) (Link (TCon int ())))
    (unchanged true)
    |}]
;;

let%expect_test "canonical_row" =
  let c = Var.create 2 (Link REmpty) in
  let b = Var.create 1 (Link (RVar c)) in
  let a = Var.create 0 (Link (RVar b)) in
  compress
    [ a; b; c ]
    ~canonical:canonical_row
    ~var:(fun v -> RVar v)
    ~sexp_of_var:[%sexp_of: (row, bound) Var.t]
    ~sexp_of:[%sexp_of: row];
  [%expect
    {|
    ((Link (RVar (Link (RVar (Link REmpty))))) (Link (RVar (Link REmpty)))
     (Link REmpty))
    REmpty
    ((Link REmpty) (Link REmpty) (Link REmpty))
    (unchanged true)
    |}]
;;

let%expect_test "canonical_presence" =
  let c = Var.create 2 (Link Present) in
  let b = Var.create 1 (Link (PVar c)) in
  let a = Var.create 0 (Link (PVar b)) in
  compress
    [ a; b; c ]
    ~canonical:canonical_presence
    ~var:(fun v -> PVar v)
    ~sexp_of_var:[%sexp_of: (presence, presence_bound) Var.t]
    ~sexp_of:[%sexp_of: presence];
  [%expect
    {|
    ((Link (PVar (Link (PVar (Link Present))))) (Link (PVar (Link Present)))
     (Link Present))
    Present
    ((Link Present) (Link Present) (Link Present))
    (unchanged true)
    |}]
;;
