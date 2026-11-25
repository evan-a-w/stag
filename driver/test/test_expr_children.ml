open! Import

(* The children of [expr] as its [sexp_of] converts them, which derives from its
   type: every [Typed.Expr.t] in it. [sexp_of] converts a record's fields last
   first, so they are not in order. *)
let sexp_children (expr : Typed.Expr.t) =
  let children = Queue.create () in
  ignore
    (Typed.Expr.Desc.sexp_of_t
       (fun child ->
         Queue.enqueue children child;
         Sexp.Atom "")
       expr.desc
     : Sexp.t);
  Queue.to_list children
;;

let same_children a b =
  List.length a = List.length b
  && List.for_all a ~f:(fun e -> List.mem b e ~equal:phys_equal)
;;

(* Checks that every node of [exprs] has as children what its type holds, but
   for a toplevel splice's code, and that every walker visits them alike. Prints
   the children of the first node of each constructor, with their scopes. *)
let check exprs =
  let first = String.Table.create () in
  let disagreeing = Queue.create () in
  let rec go (expr : Typed.Expr.t) =
    let constructor =
      match Typed.Expr.Desc.sexp_of_t (fun _ -> Sexp.Atom "") expr.desc with
      | Atom name | List (Atom name :: _) -> name
      | List _ -> "?"
    in
    let expected =
      match expr.desc with
      | Toplevel_splice { code; expansion = _ } ->
        List.filter (sexp_children expr) ~f:(fun e -> not (phys_equal e code))
      | _ -> sexp_children expr
    in
    let folded =
      List.rev
        (Typed.Expr.fold_children expr ~init:[] ~f:(fun acc child e ->
           (child, e) :: acc))
    in
    let iterated = Queue.create () in
    Typed.Expr.iter_children expr ~f:(fun child e ->
      Queue.enqueue iterated (child, e));
    let mapped = Queue.create () in
    ignore
      (Typed.Expr.map_children expr ~f:(fun child e ->
         Queue.enqueue mapped (child, e);
         e)
       : Typed.Expr.t);
    let agree visited =
      List.equal
        (fun (c, e) (c', e') ->
          phys_equal e e'
          && Sexp.equal
               [%sexp (c : Typed.Expr.Child.t)]
               [%sexp (c' : Typed.Expr.Child.t)])
        folded
        visited
    in
    if not
         (same_children (List.map folded ~f:snd) expected
          && List.equal
               phys_equal
               (Typed.Expr.sub_exprs expr)
               (List.map folded ~f:snd)
          && agree (Queue.to_list iterated)
          && agree (Queue.to_list mapped))
    then Queue.enqueue disagreeing constructor;
    if not (Hashtbl.mem first constructor)
    then
      Hashtbl.set
        first
        ~key:constructor
        ~data:(List.map folded ~f:(fun (child, _) -> child));
    List.iter (sexp_children expr) ~f:go
  in
  List.iter exprs ~f:go;
  print_s [%message (Queue.to_list disagreeing : string list)];
  List.iter
    (List.sort (Hashtbl.to_alist first) ~compare:(fun (a, _) (b, _) ->
       String.compare a b))
    ~f:(fun (constructor, children) ->
      print_s [%message constructor (children : Typed.Expr.Child.t list)])
;;

let%expect_test "every walker's children are those the type holds" =
  match
    Source_helpers.typecheck
      {|
type t = A of int | B
type rr = { a : int; b : int }
exception E of int
let f x =
  let mut m = x in
  m <- 1;
  let p = Ptr.make 1 in
  p.* <- 2;
  let r = { a = 1; b = 2 } in
  let r2 = { r with a = 3 } in
  let q = { `c = 1; `d = r2.b } in
  let q2 = { `e = 1 | q } in
  let q3 = q2 \ `c in
  match `T (A 1, q3.`d) with
  | `T (A n, _) -> (n, p.*, m)
  | `T (_, _) -> (0, 0, 0)
  | exception E n -> (n, 0, 0)
let g = fun (x : int) -> .< Int.add .~(.< x >.) 1 >.
let h = .~(.< 1 >.)
let s = .< .~(g 1) >.
|}
  with
  | None -> ()
  | Some checked ->
    check
      (List.map
         (Source_helpers.members checked)
         ~f:(fun (t : Typed.Toplevel.t) -> t.body));
    [%expect
      {|
      ("Queue.to_list disagreeing" ())
      (App (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Assign (children (((depth 0) (binds ())))))
      (Construct (children (((depth 0) (binds ())))))
      (Deref (children (((depth 0) (binds ())))))
      (Field (children (((depth 0) (binds ())))))
      (Fun (children (((depth 0) (binds (x/30))))))
      (Let (children (((depth 0) (binds (_/19))) ((depth 0) (binds (_/19))))))
      (Let_mut (children (((depth 0) (binds ())) ((depth 0) (binds (m/18))))))
      (Lit (children ()))
      (Match
       (children
        (((depth 0) (binds ())) ((depth 0) (binds (n/28))) ((depth 0) (binds ()))
         ((depth 0) (binds (n/29))))))
      (Poly_extend (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Poly_field (children (((depth 0) (binds ())))))
      (Poly_record (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Poly_restrict (children (((depth 0) (binds ())))))
      (Poly_variant (children (((depth 0) (binds ())))))
      (Quote (children (((depth 1) (binds ())))))
      (Record (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Record_update (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Splice (children (((depth -1) (binds ())))))
      (Store (children (((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Toplevel_splice (children (((depth 0) (binds ())))))
      (Tuple
       (children
        (((depth 0) (binds ())) ((depth 0) (binds ())) ((depth 0) (binds ())))))
      (Var (children ()))
      |}]
;;
