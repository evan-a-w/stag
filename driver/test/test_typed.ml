open! Import
open Source_helpers

let print ?idents source =
  Option.iter (typecheck source) ~f:(print_typed ?idents)
;;

let%expect_test "lets and shadowing" =
  print
    ~idents:true
    {|
let f x =
  let x = 1 in
  match (x, x) with x -> let g y = y in g x
|};
  [%expect
    {|
    ((members
      (((ident f/4) (kind (Function (x/13))) (scheme "'a -> int * int")
        (body
         (Let (ident x/14) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Match
            (scrutinee
             (Tuple
              ((Var (Bound (Local x/14)) : int) (Var (Bound (Local x/14)) : int))
              : "int * int"))
            (cases
             (((Var x/15 : "int * int")
               (Let (ident g/17) (args (y/16)) (generalized true)
                (body (Var (Bound (Local y/16)) : 'b))
                (in_
                 (App (Var (Bound (Local g/17)) : "int * int -> int * int")
                  (Var (Bound (Local x/15)) : "int * int") : "int * int"))
                : "int * int"))))
            : "int * int"))
          : "int * int"))))))
    ((f/4 ("'a -> int * int" 0)) (g/17 ("'a -> 'a" 0)) (x/13 ('a 0))
     (x/14 (int 0)) (x/15 ("int * int" 0)) (y/16 ('a 0)))
    |}]
;;

let%expect_test "instantiation at variables" =
  print {|
let use = let pair x = (x, x) in (id 1, id true, pair (), pair)
|};
  [%expect
    {|
    ((members
      (((ident use/4) (kind Value)
        (scheme "int * bool * (unit * unit) * ('_a -> '_a * '_a)")
        (body
         (Let (ident pair/14) (args (x/13)) (generalized true)
          (body
           (Tuple
            ((Var (Bound (Local x/13)) : 'b) (Var (Bound (Local x/13)) : 'b)) :
            "'b * 'b"))
          (in_
           (Tuple
            ((App (Var (Bound (Toplevel id/3)) : "int -> int")
              (Lit (Int 1) : int) : int)
             (App (Var (Bound (Toplevel id/3)) : "bool -> bool")
              (Lit (Bool true) : bool) : bool)
             (App (Var (Bound (Local pair/14)) : "unit -> unit * unit")
              (Lit Unit : unit) : "unit * unit")
             (Var (Bound (Local pair/14)) : "'_a -> '_a * '_a"))
            : "int * bool * (unit * unit) * ('_a -> '_a * '_a)"))
          : "int * bool * (unit * unit) * ('_a -> '_a * '_a)"))))))
    |}]
;;

let%expect_test "constructors and fields in modules" =
  print
    {|[@@@warning "-non-exhaustive"]
module M = struct
  type t = A | B of int | C of bool * int
  type r = {x : int; y : bool}
  let in_m = C (true, 1)
end
let outside v =
  match v with
  | M.A -> {M.y = true; M.x = 1}
  | M.B n -> {M.x = n; M.y = false}
  | M.C (_, 2) -> let r = {M.x = 2; M.y = true} in (match r with {M.y = y} -> r)
let get r = r.M.x
|};
  [%expect
    {|
    ((members
      (((ident M.in_m/4) (kind Value) (scheme M.t)
        (body
         (Construct ((type_name M.t) (name M.C) (index 2))
          ((Lit (Bool true) : bool) (Lit (Int 1) : int)) : M.t))))))
    ((members
      (((ident outside/5) (kind (Function (v/15))) (scheme "M.t -> M.r")
        (body
         (Match (scrutinee (Var (Bound (Local v/15)) : M.t))
          (cases
           (((Construct ((type_name M.t) (name M.A) (index 0)) () : M.t)
             (Record (record M.r)
              (fields
               ((((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool))
                (((type_name M.r) (name M.x) (index 0)) (Lit (Int 1) : int))))
              : M.r))
            ((Construct ((type_name M.t) (name M.B) (index 1)) ((Var n/16 : int))
              : M.t)
             (Record (record M.r)
              (fields
               ((((type_name M.r) (name M.x) (index 0))
                 (Var (Bound (Local n/16)) : int))
                (((type_name M.r) (name M.y) (index 1))
                 (Lit (Bool false) : bool))))
              : M.r))
            ((Construct ((type_name M.t) (name M.C) (index 2))
              ((Any : bool) (Lit (Int 2) : int)) : M.t)
             (Let (ident r/17) (args ()) (generalized true)
              (body
               (Record (record M.r)
                (fields
                 ((((type_name M.r) (name M.x) (index 0)) (Lit (Int 2) : int))
                  (((type_name M.r) (name M.y) (index 1))
                   (Lit (Bool true) : bool))))
                : M.r))
              (in_
               (Match (scrutinee (Var (Bound (Local r/17)) : M.r))
                (cases
                 (((Record
                    ((((type_name M.r) (name M.y) (index 1)) (Var y/18 : bool)))
                    : M.r)
                   (Var (Bound (Local r/17)) : M.r))))
                : M.r))
              : M.r))))
          : M.r))))))
    ((members
      (((ident get/6) (kind (Function (r/19))) (scheme "M.r -> int")
        (body
         (Field (Var (Bound (Local r/19)) : M.r)
          ((type_name M.r) (name M.x) (index 0)) : int))))))
    |}]
;;

let%expect_test "polymorphic records" =
  print
    ~idents:true
    {|
let ops r = (r.`a, {`c = 1 | r}, r \ `b, {r with `a = true}, {`a = 1; `b = ()})
let rest r = match r with {`a = a; `b = _; ..others} -> (a, others)
let open_ r = match r with {`a = a; ..} -> a
|};
  [%expect
    {|
    ((members
      (((ident ops/4) (kind (Function (r/15)))
        (scheme
         "{`a : 'a; `b : 'b; ..'c} -> 'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}")
        (body
         (Tuple
          ((Poly_field (Var (Bound (Local r/15)) : "{`a : 'a; `b : 'b; ..'c}") a
            : 'a)
           (Poly_extend ((c (Lit (Int 1) : int)))
            (Var (Bound (Local r/15)) : "{`a : 'a; `b : 'b; ..'c}") :
            "{`a : 'a; `b : 'b; `c : int; ..'c}")
           (Poly_restrict (Var (Bound (Local r/15)) : "{`a : 'a; `b : 'b; ..'c}")
            b : "{`a : 'a; ..'c}")
           (Poly_update (Var (Bound (Local r/15)) : "{`a : 'a; `b : 'b; ..'c}")
            ((a (Lit (Bool true) : bool))) : "{`a : bool; `b : 'b; ..'c}")
           (Poly_record ((a (Lit (Int 1) : int)) (b (Lit Unit : unit))) :
            "{`a : int; `b : unit}"))
          :
          "'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}"))))))
    ((members
      (((ident rest/5) (kind (Function (r/16)))
        (scheme "{`a : 'a; `b : 'b; ..'c} -> 'a * {..'c}")
        (body
         (Match
          (scrutinee (Var (Bound (Local r/16)) : "{`a : 'a; `b : 'b; ..'c}"))
          (cases
           (((Poly_record (fields ((a (Var a/17 : 'a)) (b (Any : 'b))))
              (rest (Bind others/18)) : "{`a : 'a; `b : 'b; ..'c}")
             (Tuple
              ((Var (Bound (Local a/17)) : 'a)
               (Var (Bound (Local others/18)) : {..'c}))
              : "'a * {..'c}"))))
          : "'a * {..'c}"))))))
    ((members
      (((ident open_/6) (kind (Function (r/19))) (scheme "{`a : 'a; ..'b} -> 'a")
        (body
         (Match (scrutinee (Var (Bound (Local r/19)) : "{`a : 'a; ..'b}"))
          (cases
           (((Poly_record (fields ((a (Var a/20 : 'a)))) (rest Open) :
              "{`a : 'a; ..'b}")
             (Var (Bound (Local a/20)) : 'a))))
          : 'a))))))
    ((a/17 ('a 0)) (a/20 ('a 0)) (open_/6 ("{`a : 'a; ..'b} -> 'a" 0))
     (ops/4
      ("{`a : 'a; `b : 'b; ..'c} -> 'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}"
       0))
     (others/18 ({..'a} 0)) (r/15 ("{`a : 'a; `b : 'b; ..'c}" 0))
     (r/16 ("{`a : 'a; `b : 'b; ..'c}" 0)) (r/19 ("{`a : 'a; ..'b}" 0))
     (rest/5 ("{`a : 'a; `b : 'b; ..'c} -> 'a * {..'c}" 0)))
    |}]
;;

let%expect_test "polymorphic variants" =
  print
    ~idents:true
    {|
let tags = (`A, `B 1)
let handle v = match v with `A -> 0 | `B n -> n | other -> 2
|};
  [%expect
    {|
    ((members
      (((ident tags/4) (kind Value) (scheme "[`A; ..'a] * [`B of int; ..'b]")
        (body
         (Tuple
          ((Poly_variant A () : "[`A; ..'a]")
           (Poly_variant B ((Lit (Int 1) : int)) : "[`B of int; ..'b]"))
          : "[`A; ..'a] * [`B of int; ..'b]"))))))
    ((members
      (((ident handle/5) (kind (Function (v/14)))
        (scheme "[`A | `B of int; ..'a] -> int")
        (body
         (Match (scrutinee (Var (Bound (Local v/14)) : "[`A | `B of int; ..'a]"))
          (cases
           (((Poly_variant A () : "[`A | `B of int; ..'a]") (Lit (Int 0) : int))
            ((Poly_variant B ((Var n/15 : int)) : "[`A | `B of int; ..'a]")
             (Var (Bound (Local n/15)) : int))
            ((Var other/16 : "[`A | `B of int; ..'a]") (Lit (Int 2) : int))))
          : int))))))
    ((handle/5 ("[`A | `B of int; ..'a] -> int" 0)) (n/15 (int 0))
     (other/16 ([..'a] 0)) (tags/4 ("[`A; ..'a] * [`B of int; ..'b]" 0))
     (v/14 ("[`A | `B of int; ..'a]" 0)))
    |}]
;;

let%expect_test "quotes" =
  print
    ~idents:true
    {|
let closed = .<let y = 1 in y>.
let nested = .<.<add x 1>.>.
|};
  [%expect
    {|
    ((members
      (((ident closed/4) (kind Value) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident y/14) (args ()) (generalized true)
           (body (Lit (Int 1) : int)) (in_ (Var (Bound (Local y/14)) : int)) :
           int)
          : <int>{..'a}))))))
    ((members
      (((ident nested/5) (kind Value) (scheme "<<int>{x : int; ..'a}>{..'b}")
        (body
         (Quote
          (Quote
           (App
            (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
             (Var (Free x) : int) : "int -> int")
            (Lit (Int 1) : int) : int)
           : "<int>{x : int; ..'a}")
          : "<<int>{x : int; ..'a}>{..'b}"))))))
    ((closed/4 (<int>{..'a} 0)) (nested/5 ("<<int>{x : int; ..'a}>{..'b}" 0))
     (y/14 (int 1)))
    |}]
;;

let%expect_test "level crossing" =
  print
    {|
let n = 3
let not_persistent = .<n>.
let persistent = .<id 1>.
let outer_arg x = forget x in .<x>.
let local_persistent n = let g x = add x n in forget g in .<g 1>.
let local_not_persistent = let k = 1 in .<k>.
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme int) (body (Lit (Int 3) : int))))))
    ((members
      (((ident not_persistent/5) (kind Value) (scheme <int>{..'a})
        (body (Quote (Var (Persistent (Toplevel n/4)) : int) : <int>{..'a}))))))
    ((members
      (((ident persistent/6) (kind Value) (scheme <int>{..'a})
        (body
         (Quote
          (App (Var (Persistent (Toplevel id/3)) : "int -> int")
           (Lit (Int 1) : int) : int)
          : <int>{..'a}))))))
    ((members
      (((ident outer_arg/7) (kind (Function (x/18)))
        (scheme "'a -> <'b>{x : 'b; ..'c}")
        (body (Quote (Var (Free x) : 'b) : "<'b>{x : 'b; ..'c}"))))))
    ((members
      (((ident local_persistent/8) (kind (Function (n/19)))
        (scheme "int -> <'a>{g : int -> 'a; ..'b}")
        (body
         (Let (ident g/21) (args (x/20)) (generalized true)
          (body
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/20)) : int) : "int -> int")
            (Var (Bound (Local n/19)) : int) : int))
          (in_
           (Quote (App (Var (Free g) : "int -> 'a") (Lit (Int 1) : int) : 'a) :
            "<'a>{g : int -> 'a; ..'b}"))
          : "<'a>{g : int -> 'a; ..'b}"))))))
    ((members
      (((ident local_not_persistent/9) (kind Value) (scheme <int>{..'a})
        (body
         (Let (ident k/22) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_ (Quote (Var (Persistent (Local k/22)) : int) : <int>{..'a})) :
          <int>{..'a}))))))
    |}]
;;

let%expect_test "explicit splices" =
  print {|
let explicit = .<let x = 1 in .~(.<add x y>.).{x; y = z}>.
|};
  [%expect
    {|
    ((members
      (((ident explicit/4) (kind Value) (scheme "<int>{z : int; ..'a}")
        (body
         (Quote
          (Let (ident x/13) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Var (Free y) : int) : int)
               : "<int>{x : immut int; y : immut int}"))
             (subs
              (Explicit
               ((x (Var (Bound (Local x/13)) : int)) (y (Var (Free z) : int)))))
             : int))
           : int)
          : "<int>{z : int; ..'a}"))))))
    |}]
;;

let%expect_test "implicit splices" =
  print
    ~idents:true
    {|
let bound = .<let x = 1 in .~(.<add x 1>.)>.
let propagates = .<.~(.<add x 1>.)>.
let tail c = let outer = 1 in .<let y = 1 in let f a = a in .~c>.
let later_level c = .<.<let y = 1 in .~(.~c)>.>.
|};
  [%expect
    {|
    ((members
      (((ident bound/4) (kind Value) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident x/16) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Lit (Int 1) : int) : int)
               : "<int>{x : immut int; ..'_b}"))
             (subs (Implicit ((x (Local x/16))))) : int))
           : int)
          : <int>{..'a}))
        (where "..'_b <= {..'a}")))))
    ((members
      (((ident propagates/5) (kind Value) (scheme "<int>{x : int; ..'a}")
        (body
         (Quote
          (Splice
           (code
            (Quote
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Free x) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : "<int>{x : immut int; ..'_b}"))
           (subs (Implicit ())) : int)
          : "<int>{x : int; ..'a}"))
        (where "..'_b <= {x : int; ..'a}")))))
    ((members
      (((ident tail/6) (kind (Function (c/17)))
        (scheme
         "<'a>{f?'b : immut 'c -> 'c; y?'d : immut int; ..'e} -> <'a>{..'f}")
        (body
         (Let (ident outer/18) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Let (ident y/19) (args ()) (generalized true)
             (body (Lit (Int 1) : int))
             (in_
              (Let (ident f/21) (args (a/20)) (generalized true)
               (body (Var (Bound (Local a/20)) : 'g))
               (in_
                (Splice
                 (code
                  (Var (Bound (Local c/17)) :
                   "<'a>{f?'b : immut 'c -> 'c; y?'d : immut int; ..'e}"))
                 (subs (Implicit ((f (Local f/21)) (y (Local y/19))))) : 'a))
               : 'a))
             : 'a)
            : <'a>{..'f}))
          : <'a>{..'f}))
        (where "..'e <= {..'f}")))))
    ((members
      (((ident later_level/7) (kind (Function (c/22)))
        (scheme "<<'a>{y?'b : immut int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f}")
        (body
         (Quote
          (Quote
           (Let (ident y/23) (args ()) (generalized true)
            (body (Lit (Int 1) : int))
            (in_
             (Splice
              (code
               (Splice
                (code
                 (Var (Bound (Local c/22)) :
                  "<<'a>{y?'b : immut int; ..'c}>{-y; ..'d}"))
                (subs (Implicit ())) : "<'a>{y?'b : immut int; ..'c}"))
              (subs (Implicit ((y (Local y/23))))) : 'a))
            : 'a)
           : <'a>{..'e})
          : <<'a>{..'e}>{..'f}))
        (where "..'c <= {..'e}, ..'d <= {..'f}")))))
    ((a/20 ('a 1)) (bound/4 (<int>{..'a} 0))
     (c/17
      ("<'a>{f?'b : immut 'c -> 'c; y?'d : immut int; ..'e} where ..'e <= {..'f}"
       0))
     (c/22
      ("<<'a>{y?'b : immut int; ..'c}>{-y; ..'d} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (f/21 ("'a -> 'a" 1))
     (later_level/7
      ("<<'a>{y?'b : immut int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (outer/18 (int 0)) (propagates/5 ("<int>{x : int; ..'a}" 0))
     (tail/6
      ("<'a>{f?'b : immut 'c -> 'c; y?'d : immut int; ..'e} -> <'a>{..'f} where ..'e <= {..'f}"
       0))
     (x/16 (int 1)) (y/19 (int 1)) (y/23 (int 2)))
    |}]
;;

let%expect_test "toplevel splices" =
  print
    ~idents:true
    {|
let n = 3
let m = n
let f x = .~(.<add x (add n 1)>.)
let closed = .~(.<1>.)
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme int) (body (Lit (Int 3) : int))))))
    ((members
      (((ident m/5) (kind Value) (scheme int)
        (body (Var (Bound (Toplevel n/4)) : int))))))
    ((members
      (((ident f/6) (kind (Function (x/16))) (scheme "int -> int")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
              (Var (Free x) : int) : "int -> int")
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Persistent (Toplevel n/4)) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : int)
            : "<int>{x : immut int}"))
          (expansion
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/16)) : int) : "int -> int")
            (App
             (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
              (Var (Bound (Toplevel n/4)) : int) : "int -> int")
             (Lit (Int 1) : int) : int)
            : int))
          : int))))))
    ((members
      (((ident closed/7) (kind Value) (scheme int)
        (body
         (Toplevel_splice (code (Quote (Lit (Int 1) : int) : <int>{}))
          (expansion (Lit (Int 1) : int)) : int))))))
    ((closed/7 (int 0)) (f/6 ("int -> int" 0)) (m/5 (int 0)) (n/4 (int 0))
     (x/16 (int 0)))
    |}]
;;

let%expect_test "components in evaluation order" =
  print
    {|
let main = even 3
let even n = cond (eq n 0) true (odd n)
let odd n = cond (eq n 0) false (even n)
let a = b
let b = 1
let gen n = forget n in .<let _ = gen in n>.
|};
  [%expect
    {|
    ((members
      (((ident even/5) (kind (Function (n/18))) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/18)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool true) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel odd/6)) : "int -> bool")
           (Var (Bound (Local n/18)) : int) : bool)
          : bool)))
       ((ident odd/6) (kind (Function (n/19))) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/19)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool false) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel even/5)) : "int -> bool")
           (Var (Bound (Local n/19)) : int) : bool)
          : bool))))))
    ((members
      (((ident main/4) (kind Value) (scheme bool)
        (body
         (App (Var (Bound (Toplevel even/5)) : "int -> bool") (Lit (Int 3) : int)
          : bool))))))
    ((members
      (((ident b/8) (kind Value) (scheme int) (body (Lit (Int 1) : int))))))
    ((members
      (((ident a/7) (kind Value) (scheme int)
        (body (Var (Bound (Toplevel b/8)) : int))))))
    ((members
      (((ident gen/9) (kind (Function (n/20)))
        (scheme "'a -> <'b>{n : 'b; ..'c}")
        (body
         (Quote
          (Let (ident _/21) (args ()) (generalized false)
           (body
            (Var (Persistent (Toplevel gen/9)) : "'a -> <'b>{n : 'b; ..'c}"))
           (in_ (Var (Free n) : 'b)) : 'b)
          : "<'b>{n : 'b; ..'c}"))))))
    |}]
;;

let%expect_test "recursive components" =
  print
    {|
let gen n = forget n in .<let _ = gen in n>.
let a = .<let _ = b in 1>.
let b = .<let _ = a in 2>.
let loop x = loop x
let captured x = .~(.<captured x>.)
|};
  [%expect
    {|
    ((members
      (((ident gen/4) (kind (Function (n/17)))
        (scheme "'a -> <'b>{n : 'b; ..'c}")
        (body
         (Quote
          (Let (ident _/18) (args ()) (generalized false)
           (body
            (Var (Persistent (Toplevel gen/4)) : "'a -> <'b>{n : 'b; ..'c}"))
           (in_ (Var (Free n) : 'b)) : 'b)
          : "<'b>{n : 'b; ..'c}"))))))
    ((members
      (((ident b/6) (kind Value) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident _/20) (args ()) (generalized false)
           (body (Var (Persistent (Toplevel a/5)) : <int>{..'_b}))
           (in_ (Lit (Int 2) : int)) : int)
          : <int>{..'a}))))))
    ((members
      (((ident a/5) (kind Value) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident _/19) (args ()) (generalized true)
           (body (Var (Persistent (Toplevel b/6)) : <int>{..'b}))
           (in_ (Lit (Int 1) : int)) : int)
          : <int>{..'a}))))))
    ((members
      (((ident loop/7) (kind (Function (x/21))) (scheme "'a -> 'b")
        (body
         (App (Var (Bound (Toplevel loop/7)) : "'a -> 'b")
          (Var (Bound (Local x/21)) : 'a) : 'b))))))
    ((members
      (((ident captured/8) (kind (Function (x/22))) (scheme "'a -> 'b")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App (Var (Persistent (Toplevel captured/8)) : "'a -> 'b")
             (Var (Free x) : 'a) : 'b)
            : "<'b>{x : immut 'a}"))
          (expansion
           (App (Var (Bound (Toplevel captured/8)) : "'a -> 'b")
            (Var (Bound (Local x/22)) : 'a) : 'b))
          : 'b))))))
    |}]
;;

let%expect_test "literals that look like placeholders" =
  print {|[@@@warning "-non-exhaustive"]
let lits s = match s with "\0000" -> ('\000', "\0003")
|};
  [%expect
    {|
    ((members
      (((ident lits/4) (kind (Function (s/13)))
        (scheme "string -> char * string")
        (body
         (Match (scrutinee (Var (Bound (Local s/13)) : string))
          (cases
           (((Lit (String "\0000") : string)
             (Tuple ((Lit (Char "\000") : char) (Lit (String "\0003") : string))
              : "char * string"))))
          : "char * string"))))))
    |}]
;;

let%expect_test "level crossing at splices" =
  print
    ~idents:true
    {|
let n = 3
let persistent_local = .<let f x = x in .~(id .<f 1>.)>.
let outside_binder = let x = 1 in .<.~(.<x>.)>.
let shadowed c = .<let x = 1 in let x = 2 in .~c>.
let n_shadowed = let n = 1 in .~(.<n>.)
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme int) (body (Lit (Int 3) : int))))))
    ((members
      (((ident persistent_local/5) (kind Value) (scheme <int>{..'_a})
        (body
         (Quote
          (Let (ident f/18) (args (x/17)) (generalized true)
           (body (Var (Bound (Local x/17)) : 'b))
           (in_
            (Splice
             (code
              (App
               (Var (Bound (Toplevel id/3)) :
                "<int>{f : immut int -> int; ..'_c} -> <int>{f : immut int -> int; ..'_c}")
               (Quote
                (App (Var (Free f) : "int -> int") (Lit (Int 1) : int) : int) :
                "<int>{f : immut int -> int; ..'_c}")
               : "<int>{f : immut int -> int; ..'_c}"))
             (subs (Implicit ((f (Local f/18))))) : int))
           : int)
          : <int>{..'_a}))
        (where "..'_c <= {..'_a}")))))
    ((members
      (((ident outside_binder/6) (kind Value) (scheme <int>{..'a})
        (body
         (Let (ident x/19) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Splice
             (code (Quote (Var (Persistent (Local x/19)) : int) : <int>{..'_b}))
             (subs (Implicit ())) : int)
            : <int>{..'a}))
          : <int>{..'a}))
        (where "..'_b <= {..'a}")))))
    ((members
      (((ident shadowed/7) (kind (Function (c/20)))
        (scheme "<'a>{x?'b : immut int; ..'c} -> <'a>{..'d}")
        (body
         (Quote
          (Let (ident x/21) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Let (ident x/22) (args ()) (generalized true)
             (body (Lit (Int 2) : int))
             (in_
              (Splice
               (code (Var (Bound (Local c/20)) : "<'a>{x?'b : immut int; ..'c}"))
               (subs (Implicit ((x (Local x/22))))) : 'a))
             : 'a))
           : 'a)
          : <'a>{..'d}))
        (where "..'c <= {..'d}")))))
    ((members
      (((ident n_shadowed/8) (kind Value) (scheme int)
        (body
         (Let (ident n/23) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Toplevel_splice
            (code (Quote (Var (Free n) : int) : "<int>{n : immut int}"))
            (expansion (Var (Bound (Local n/23)) : int)) : int))
          : int))))))
    ((c/20 ("<'a>{x?'b : immut int; ..'c} where ..'c <= {..'d}" 0))
     (f/18 ("'a -> 'a" 1)) (n/4 (int 0)) (n/23 (int 0)) (n_shadowed/8 (int 0))
     (outside_binder/6 (<int>{..'a} 0)) (persistent_local/5 (<int>{..'_a} 0))
     (shadowed/7
      ("<'a>{x?'b : immut int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}" 0))
     (x/17 ('a 1)) (x/19 (int 0)) (x/21 (int 1)) (x/22 (int 1)))
    |}]
;;

let%expect_test "toplevel splice in a level -1 let" =
  print
    ~idents:true
    {|
let n = 3
let deep =
  .~(let x = .<1>. in
     .~(let k c = (.<let n = 1 in .~c>.; c) in k .<x>.))
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme int) (body (Lit (Int 3) : int))))))
    ((members
      (((ident deep/5) (kind Value) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/14) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident k/16) (args (c/15)) (generalized true)
                (body
                 (Seq
                  (Quote
                   (Let (ident n/17) (args ()) (generalized true)
                    (body (Lit (Int 1) : int))
                    (in_
                     (Splice
                      (code
                       (Var (Bound (Local c/15)) :
                        "<'b>{n?'c : immut int; ..'d}"))
                      (subs (Implicit ((n (Local n/17))))) : 'b))
                    : 'b)
                   : <'b>{..'e})
                  (Var (Bound (Local c/15)) : "<'b>{n?'c : immut int; ..'d}") :
                  "<'b>{n?'c : immut int; ..'d}"))
                (in_
                 (App
                  (Var (Bound (Local k/16)) :
                   "<<int>{}>{x : immut <int>{}} -> <<int>{}>{x : immut <int>{}}")
                  (Quote (Var (Free x) : <int>{}) :
                   "<<int>{}>{x : immut <int>{}}")
                  : "<<int>{}>{x : immut <int>{}}"))
                : "<<int>{}>{x : immut <int>{}}"))
              (expansion (Var (Bound (Local x/14)) : <int>{})) : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int))))))
    ((c/15 ("<'a>{n?'b : immut int; ..'c}" -2)) (deep/5 (int 0))
     (k/16 ("<'a>{n?'b : immut int; ..'c} -> <'a>{n?'b : immut int; ..'c}" -2))
     (n/4 (int 0)) (n/17 (int -1)) (x/14 (<int>{..'a} -1)))
    |}]
;;

let%expect_test "toplevel used at level -1 by spliced code" =
  print
    {|
let n = 3
let deep = .~(let x = .<1>. in .~(let n = 5 in .<n; x>.))
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme int) (body (Lit (Int 3) : int))))))
    ((members
      (((ident deep/5) (kind Value) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/14) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident n/15) (args ()) (generalized true)
                (body (Lit (Int 5) : int))
                (in_
                 (Quote
                  (Seq (Var (Persistent (Local n/15)) : int)
                   (Var (Free x) : <int>{}) : <int>{})
                  : "<<int>{}>{x : immut <int>{}}"))
                : "<<int>{}>{x : immut <int>{}}"))
              (expansion
               (Seq (Var (Persistent (Local n/16)) : int)
                (Var (Bound (Local x/14)) : <int>{}) : <int>{}))
              : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int))))))
    |}]
;;

let%expect_test "explicit substitutions" =
  print
    {|
let n = Int.to_string 3
let persistent_shorthand = let f y = y in .<.~(.<id (f 1)>.).{f; id}>.
let at_toplevel = .~(forget n in .<String.append n y>.).{n; y = "1"}
|};
  [%expect
    {|
    ((members
      (((ident n/4) (kind Value) (scheme string)
        (body
         (App (Var (Prim Int.to_string) : "int -> string") (Lit (Int 3) : int) :
          string))))))
    ((members
      (((ident persistent_shorthand/5) (kind Value) (scheme <int>{..'a})
        (body
         (Let (ident f/16) (args (y/15)) (generalized true)
          (body (Var (Bound (Local y/15)) : 'b))
          (in_
           (Quote
            (Splice
             (code
              (Quote
               (App (Var (Persistent (Toplevel id/3)) : "int -> int")
                (App (Var (Persistent (Local f/16)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : int)
               : "<int>{f?'_c : immut '_d -> '_d; id?'_e : immut '_f -> '_f}"))
             (subs
              (Explicit
               ((f (Var (Persistent (Local f/16)) : "'_d -> '_d"))
                (id (Var (Persistent (Toplevel id/3)) : "'_f -> '_f")))))
             : int)
            : <int>{..'a}))
          : <int>{..'a}))))))
    ((members
      (((ident at_toplevel/6) (kind Value) (scheme string)
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Prim String.append) : "string -> string -> string")
              (Var (Free n) : string) : "string -> string")
             (Var (Free y) : string) : string)
            : "<string>{n : immut string; y : immut string}"))
          (expansion
           (Let (ident y/17) (args ()) (generalized false)
            (body (Lit (String 1) : string))
            (in_
             (App
              (App (Var (Prim String.append) : "string -> string -> string")
               (Var (Bound (Toplevel n/4)) : string) : "string -> string")
              (Var (Bound (Local y/17)) : string) : string))
            : string))
          : string))))))
    |}]
;;

let%expect_test "local recursion" =
  print
    {|
let direct y = let f x = f x in f y
let in_quote = let f x = forget f, x in .<f x>. in .<f>.
|};
  [%expect
    {|
    ((members
      (((ident direct/4) (kind (Function (y/14))) (scheme "'a -> 'b")
        (body
         (Let (ident f/16) (args (x/15)) (generalized true)
          (body
           (App (Var (Bound (Local f/16)) : "'c -> 'd")
            (Var (Bound (Local x/15)) : 'c) : 'd))
          (in_
           (App (Var (Bound (Local f/16)) : "'a -> 'b")
            (Var (Bound (Local y/14)) : 'a) : 'b))
          : 'b))))))
    ((members
      (((ident in_quote/5) (kind Value)
        (scheme "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}")
        (body
         (Let (ident f/18) (args (x/17)) (generalized true)
          (body
           (Quote (App (Var (Free f) : "'f -> 'g") (Var (Free x) : 'f) : 'g) :
            "<'g>{f : 'f -> 'g; x : 'f; ..'h}"))
          (in_
           (Quote
            (Var (Persistent (Local f/18)) :
             "'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}")
            : "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}"))
          : "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}"))))))
    |}]
;;

let%expect_test "tuple patterns and annotations" =
  print ~idents:true {|
let first p = match p with (a, _) -> (a : int)
|};
  [%expect
    {|
    ((members
      (((ident first/4) (kind (Function (p/13))) (scheme "int * 'a -> int")
        (body
         (Match (scrutinee (Var (Bound (Local p/13)) : "int * 'a"))
          (cases
           (((Tuple ((Var a/14 : int) (Any : 'a)) : "int * 'a")
             (Annotated (Var (Bound (Local a/14)) : int)
              ((ty ((Con (<int> @ t.stag:2:42-45) ()) @ t.stag:2:42-45))) : int))))
          : int))))))
    ((a/14 (int 0)) (first/4 ("int * 'a -> int" 0)) (p/13 ("int * 'a" 0)))
    |}]
;;

let%expect_test "the code of a scheme refined once a toplevel it awaits is \
                 finished has the refined types"
  =
  print {|
let f (x : int) : int = .~(let _ = c in .< x >.)
let c = .< f >.
|};
  [%expect
    {|
    ((members
      (((ident c/5) (kind Value) (scheme "<int -> int>{..'a}")
        (body
         (Quote (Var (Persistent (Toplevel f/4)) : "int -> int") :
          "<int -> int>{..'a}"))))))
    ((members
      (((ident f/4) (kind (Function (x/14))) (scheme "int -> int")
        (body
         (Toplevel_splice
          (code
           (Let (ident _/15) (args ()) (generalized true)
            (body (Var (Bound (Toplevel c/5)) : "<int -> int>{..'a}"))
            (in_ (Quote (Var (Free x) : int) : "<int>{x : immut int}")) :
            "<int>{x : immut int}"))
          (expansion (Var (Bound (Local x/14)) : int)) : int))))))
    |}]
;;

let%expect_test "the code of a local generalized with awaiting uses has the \
                 refined types"
  =
  print
    {|
let ident (x : int) : int = .~(let _ = h () in .< x >.)
let c y = .< ident .~y >.
let h (u : unit) = let f = fun v -> c v in f .< 1 >.
|};
  [%expect
    {|
    ((members
      (((ident c/5) (kind (Function (y/20)))
        (scheme "<int>{..'a} -> <int>{..'b}")
        (body
         (Quote
          (App (Var (Persistent (Toplevel ident/4)) : "int -> int")
           (Splice (code (Var (Bound (Local y/20)) : <int>{..'a}))
            (subs (Implicit ())) : int)
           : int)
          : <int>{..'b}))
        (where "..'a <= {..'b}")))))
    ((members
      (((ident h/6) (kind (Function (u/17))) (scheme "unit -> <int>{..'a}")
        (body
         (Let (ident f/18) (args ()) (generalized true)
          (body
           (Fun (args (v/19))
            (body
             (App (Var (Bound (Toplevel c/5)) : "<int>{..'b} -> <int>{..'c}")
              (Var (Bound (Local v/19)) : <int>{..'b}) : <int>{..'c}))
            : "<int>{..'b} -> <int>{..'c}"))
          (in_
           (App (Var (Bound (Local f/18)) : "<int>{..'_d} -> <int>{..'a}")
            (Quote (Lit (Int 1) : int) : <int>{..'_d}) : <int>{..'a}))
          : <int>{..'a}))
        (where "..'b <= {..'c}, ..'_d <= {..'a}")))))
    ((members
      (((ident ident/4) (kind (Function (x/15))) (scheme "int -> int")
        (body
         (Toplevel_splice
          (code
           (Let (ident _/16) (args ()) (generalized false)
            (body
             (App (Var (Bound (Toplevel h/6)) : "unit -> <int>{..'_a}")
              (Lit Unit : unit) : <int>{..'_a}))
            (in_ (Quote (Var (Free x) : int) : "<int>{x : immut int}")) :
            "<int>{x : immut int}"))
          (expansion (Var (Bound (Local x/15)) : int)) : int))))))
    |}]
;;

let%expect_test "refining an awaited scheme refines the schemes of the locals \
                 generalized with uses of it"
  =
  (* [f]'s scheme relates [v]'s row to [k]'s, which is [c]'s: only reopening
     [f]'s scheme with the uses keeps that once [ident] is finished. *)
  print
    {|
let ident x = .~(let _ = h () in .< x >.)
let c y = .< ident .~y >.
let h (u : unit) = let f = fun v -> let k = c v in (k, (v : <'a>{..'r})) in f .< 1 >.
|};
  [%expect
    {|
    ((members
      (((ident c/5) (kind (Function (y/21))) (scheme "<'a>{..'b} -> <'a>{..'c}")
        (body
         (Quote
          (App (Var (Persistent (Toplevel ident/4)) : "'a -> 'a")
           (Splice (code (Var (Bound (Local y/21)) : <'a>{..'b}))
            (subs (Implicit ())) : 'a)
           : 'a)
          : <'a>{..'c}))
        (where "..'b <= {..'c}")))))
    ((members
      (((ident h/6) (kind (Function (u/17)))
        (scheme "unit -> <int>{..'a} * <int>{..'b}")
        (body
         (Let (ident f/18) (args ()) (generalized true)
          (body
           (Fun (args (v/19))
            (body
             (Let (ident k/20) (args ()) (generalized false)
              (body
               (App (Var (Bound (Toplevel c/5)) : "<int>{..'b} -> <int>{..'c}")
                (Var (Bound (Local v/19)) : <int>{..'b}) : <int>{..'c}))
              (in_
               (Tuple
                ((Var (Bound (Local k/20)) : <int>{..'c})
                 (Annotated (Var (Bound (Local v/19)) : <int>{..'b})
                  ((ty
                    ((Code (inner ((Var a) @ t.stag:4:61-63))
                      (context ((Var (r @ t.stag:4:67-69)) @ t.stag:4:64-70)))
                     @ t.stag:4:60-70)))
                  : <int>{..'b}))
                : "<int>{..'c} * <int>{..'b}"))
              : "<int>{..'c} * <int>{..'b}"))
            : "<int>{..'b} -> <int>{..'c} * <int>{..'b}"))
          (in_
           (App
            (Var (Bound (Local f/18)) :
             "<int>{..'b} -> <int>{..'a} * <int>{..'b}")
            (Quote (Lit (Int 1) : int) : <int>{..'b}) :
            "<int>{..'a} * <int>{..'b}"))
          : "<int>{..'a} * <int>{..'b}"))
        (where "..'b <= {..'c}, ..'b <= {..'a}")))))
    ((members
      (((ident ident/4) (kind (Function (x/15))) (scheme "'a -> 'a")
        (body
         (Toplevel_splice
          (code
           (Let (ident _/16) (args ()) (generalized false)
            (body
             (App
              (Var (Bound (Toplevel h/6)) :
               "unit -> <int>{..'_b} * <int>{..'_c}")
              (Lit Unit : unit) : "<int>{..'_b} * <int>{..'_c}"))
            (in_ (Quote (Var (Free x) : 'a) : "<'a>{x : immut 'a}")) :
            "<'a>{x : immut 'a}"))
          (expansion (Var (Bound (Local x/15)) : 'a)) : 'a))
        (where "..'_c <= {..'_d}, ..'_c <= {..'_b}")))))
    |}]
;;
