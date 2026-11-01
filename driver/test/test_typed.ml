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
      (((ident f/4) (args (x/14)) (scheme "'a -> int * int")
        (body
         (Let (ident x/15) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Match
            (scrutinee
             (Tuple
              ((Var (Bound (Local x/15)) : int) (Var (Bound (Local x/15)) : int))
              : "int * int"))
            (cases
             (((Var x/16 : "int * int")
               (Let (ident g/18) (args (y/17)) (generalized true)
                (body (Var (Bound (Local y/17)) : 'b))
                (in_
                 (App (Var (Bound (Local g/18)) : "int * int -> int * int")
                  (Var (Bound (Local x/16)) : "int * int") : "int * int"))
                : "int * int"))))
            : "int * int"))
          : "int * int")))))
     (recursive false))
    ((f/4 ("'a -> int * int" 0)) (g/18 ("'a -> 'a" 0)) (x/14 ('a 0))
     (x/15 (int 0)) (x/16 ("int * int" 0)) (y/17 ('a 0)))
    |}]
;;

let%expect_test "instantiation at variables" =
  print
    {|
let use = let pair x = (x, x) in (id 1, id true, pair (), pair)
|};
  [%expect
    {|
    ((members
      (((ident use/4) (args ())
        (scheme "int * bool * (unit * unit) * ('_a -> '_a * '_a)")
        (body
         (Let (ident pair/15) (args (x/14)) (generalized true)
          (body
           (Tuple
            ((Var (Bound (Local x/14)) : 'b) (Var (Bound (Local x/14)) : 'b)) :
            "'b * 'b"))
          (in_
           (Tuple
            ((App (Var (Bound (Toplevel id/3)) : "int -> int")
              (Lit (Int 1) : int) : int)
             (App (Var (Bound (Toplevel id/3)) : "bool -> bool")
              (Lit (Bool true) : bool) : bool)
             (App (Var (Bound (Local pair/15)) : "unit -> unit * unit")
              (Lit Unit : unit) : "unit * unit")
             (Var (Bound (Local pair/15)) : "'_a -> '_a * '_a"))
            : "int * bool * (unit * unit) * ('_a -> '_a * '_a)"))
          : "int * bool * (unit * unit) * ('_a -> '_a * '_a)")))))
     (recursive false))
    |}]
;;

let%expect_test "constructors and fields in modules" =
  print
    {|
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
      (((ident M.in_m/4) (args ()) (scheme M.t)
        (body
         (Construct ((type_name M.t) (name M.C) (index 2))
          ((Lit (Bool true) : bool) (Lit (Int 1) : int)) : M.t)))))
     (recursive false))
    ((members
      (((ident outside/5) (args (v/16)) (scheme "M.t -> M.r")
        (body
         (Match (scrutinee (Var (Bound (Local v/16)) : M.t))
          (cases
           (((Construct ((type_name M.t) (name M.A) (index 0)) () : M.t)
             (Record
              ((((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool))
               (((type_name M.r) (name M.x) (index 0)) (Lit (Int 1) : int)))
              : M.r))
            ((Construct ((type_name M.t) (name M.B) (index 1)) ((Var n/17 : int))
              : M.t)
             (Record
              ((((type_name M.r) (name M.x) (index 0))
                (Var (Bound (Local n/17)) : int))
               (((type_name M.r) (name M.y) (index 1)) (Lit (Bool false) : bool)))
              : M.r))
            ((Construct ((type_name M.t) (name M.C) (index 2))
              ((Any : bool) (Lit (Int 2) : int)) : M.t)
             (Let (ident r/18) (args ()) (generalized true)
              (body
               (Record
                ((((type_name M.r) (name M.x) (index 0)) (Lit (Int 2) : int))
                 (((type_name M.r) (name M.y) (index 1))
                  (Lit (Bool true) : bool)))
                : M.r))
              (in_
               (Match (scrutinee (Var (Bound (Local r/18)) : M.r))
                (cases
                 (((Record
                    ((((type_name M.r) (name M.y) (index 1)) (Var y/19 : bool)))
                    : M.r)
                   (Var (Bound (Local r/18)) : M.r))))
                : M.r))
              : M.r))))
          : M.r)))))
     (recursive false))
    ((members
      (((ident get/6) (args (r/20)) (scheme "M.r -> int")
        (body
         (Field (Var (Bound (Local r/20)) : M.r)
          ((type_name M.r) (name M.x) (index 0)) : int)))))
     (recursive false))
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
      (((ident ops/4) (args (r/16))
        (scheme
         "{`a : 'a; `b : 'b; ..'c} -> 'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}")
        (body
         (Tuple
          ((Poly_field (Var (Bound (Local r/16)) : "{`a : 'a; `b : 'b; ..'c}") a
            : 'a)
           (Poly_extend ((c (Lit (Int 1) : int)))
            (Var (Bound (Local r/16)) : "{`a : 'a; `b : 'b; ..'c}") :
            "{`a : 'a; `b : 'b; `c : int; ..'c}")
           (Poly_restrict (Var (Bound (Local r/16)) : "{`a : 'a; `b : 'b; ..'c}")
            b : "{`a : 'a; ..'c}")
           (Poly_update (Var (Bound (Local r/16)) : "{`a : 'a; `b : 'b; ..'c}")
            ((a (Lit (Bool true) : bool))) : "{`a : bool; `b : 'b; ..'c}")
           (Poly_record ((a (Lit (Int 1) : int)) (b (Lit Unit : unit))) :
            "{`a : int; `b : unit}"))
          :
          "'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}")))))
     (recursive false))
    ((members
      (((ident rest/5) (args (r/17))
        (scheme "{`a : 'a; `b : 'b; ..'c} -> 'a * {..'c}")
        (body
         (Match
          (scrutinee (Var (Bound (Local r/17)) : "{`a : 'a; `b : 'b; ..'c}"))
          (cases
           (((Poly_record (fields ((a (Var a/18 : 'a)) (b (Any : 'b))))
              (rest (Bind others/19)) : "{`a : 'a; `b : 'b; ..'c}")
             (Tuple
              ((Var (Bound (Local a/18)) : 'a)
               (Var (Bound (Local others/19)) : {..'c}))
              : "'a * {..'c}"))))
          : "'a * {..'c}")))))
     (recursive false))
    ((members
      (((ident open_/6) (args (r/20)) (scheme "{`a : 'a; ..'b} -> 'a")
        (body
         (Match (scrutinee (Var (Bound (Local r/20)) : "{`a : 'a; ..'b}"))
          (cases
           (((Poly_record (fields ((a (Var a/21 : 'a)))) (rest Open) :
              "{`a : 'a; ..'b}")
             (Var (Bound (Local a/21)) : 'a))))
          : 'a)))))
     (recursive false))
    ((a/18 ('a 0)) (a/21 ('a 0)) (open_/6 ("{`a : 'a; ..'b} -> 'a" 0))
     (ops/4
      ("{`a : 'a; `b : 'b; ..'c} -> 'a * {`a : 'a; `b : 'b; `c : int; ..'c} * {`a : 'a; ..'c} * {`a : bool; `b : 'b; ..'c} * {`a : int; `b : unit}"
       0))
     (others/19 ({..'a} 0)) (r/16 ("{`a : 'a; `b : 'b; ..'c}" 0))
     (r/17 ("{`a : 'a; `b : 'b; ..'c}" 0)) (r/20 ("{`a : 'a; ..'b}" 0))
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
      (((ident tags/4) (args ()) (scheme "[`A; ..'a] * [`B of int; ..'b]")
        (body
         (Tuple
          ((Poly_variant A () : "[`A; ..'a]")
           (Poly_variant B ((Lit (Int 1) : int)) : "[`B of int; ..'b]"))
          : "[`A; ..'a] * [`B of int; ..'b]")))))
     (recursive false))
    ((members
      (((ident handle/5) (args (v/15)) (scheme "[`A | `B of int; ..'a] -> int")
        (body
         (Match (scrutinee (Var (Bound (Local v/15)) : "[`A | `B of int; ..'a]"))
          (cases
           (((Poly_variant A () : "[`A | `B of int; ..'a]") (Lit (Int 0) : int))
            ((Poly_variant B ((Var n/16 : int)) : "[`A | `B of int; ..'a]")
             (Var (Bound (Local n/16)) : int))
            ((Var other/17 : "[`A | `B of int; ..'a]") (Lit (Int 2) : int))))
          : int)))))
     (recursive false))
    ((handle/5 ("[`A | `B of int; ..'a] -> int" 0)) (n/16 (int 0))
     (other/17 ([..'a] 0)) (tags/4 ("[`A; ..'a] * [`B of int; ..'b]" 0))
     (v/15 ("[`A | `B of int; ..'a]" 0)))
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
      (((ident closed/4) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident y/15) (args ()) (generalized true)
           (body (Lit (Int 1) : int)) (in_ (Var (Bound (Local y/15)) : int)) :
           int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident nested/5) (args ()) (scheme "<<int>{x : mut?'a int; ..'b}>{..'c}")
        (body
         (Quote
          (Quote
           (App
            (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
             (Var (Free x) : int) : "int -> int")
            (Lit (Int 1) : int) : int)
           : "<int>{x : mut?'a int; ..'b}")
          : "<<int>{x : mut?'a int; ..'b}>{..'c}")))))
     (recursive false))
    ((closed/4 (<int>{..'a} 0))
     (nested/5 ("<<int>{x : mut?'a int; ..'b}>{..'c}" 0)) (y/15 (int 1)))
    |}]
;;

let%expect_test "level crossing" =
  print
    {|
let n = 3
let not_persistent = .<n>.
let persistent = .<id 1>.
let outer_arg x = .<x>.
let local_persistent n = let g x = add x n in .<g 1>.
let local_not_persistent = let k = 1 in .<k>.
|};
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident not_persistent/5) (args ()) (scheme "<'a>{n : mut?'b 'a; ..'c}")
        (body (Quote (Var (Free_toplevel n) : 'a) : "<'a>{n : mut?'b 'a; ..'c}")))))
     (recursive false))
    ((members
      (((ident persistent/6) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (App (Var (Persistent (Toplevel id/3)) : "int -> int")
           (Lit (Int 1) : int) : int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident outer_arg/7) (args (x/19))
        (scheme "'a -> <'b>{x : mut?'c 'b; ..'d}")
        (body (Quote (Var (Free x) : 'b) : "<'b>{x : mut?'c 'b; ..'d}")))))
     (recursive false))
    ((members
      (((ident local_persistent/8) (args (n/20)) (scheme "int -> <int>{..'a}")
        (body
         (Let (ident g/22) (args (x/21)) (generalized true)
          (body
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/21)) : int) : "int -> int")
            (Var (Bound (Local n/20)) : int) : int))
          (in_
           (Quote
            (App (Var (Persistent (Local g/22)) : "int -> int")
             (Lit (Int 1) : int) : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident local_not_persistent/9) (args ())
        (scheme "<'a>{k : mut?'b 'a; ..'c}")
        (body
         (Let (ident k/23) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_ (Quote (Var (Free k) : 'a) : "<'a>{k : mut?'b 'a; ..'c}")) :
          "<'a>{k : mut?'b 'a; ..'c}")))))
     (recursive false))
    |}]
;;

let%expect_test "explicit splices" =
  print
    {|
let explicit = .<let x = 1 in .~(.<add x y>.).{x; y = z}>.
|};
  [%expect
    {|
    ((members
      (((ident explicit/4) (args ()) (scheme "<int>{z : mut?'a int; ..'b}")
        (body
         (Quote
          (Let (ident x/14) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Var (Free y) : int) : int)
               : "<int>{x : int; y : int}"))
             (subs
              (Explicit
               ((x (Var (Bound (Local x/14)) : int)) (y (Var (Free z) : int)))))
             : int))
           : int)
          : "<int>{z : mut?'a int; ..'b}")))))
     (recursive false))
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
      (((ident bound/4) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident x/17) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Lit (Int 1) : int) : int)
               : "<int>{x : int; ..'_b}"))
             (subs (Implicit ((x (Local x/17))))) : int))
           : int)
          : <int>{..'a}))
        (where "..'_b <= {..'a}"))))
     (recursive false))
    ((members
      (((ident propagates/5) (args ()) (scheme "<int>{x : mut?'a int; ..'b}")
        (body
         (Quote
          (Splice
           (code
            (Quote
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Free x) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : "<int>{x : int; ..'_c}"))
           (subs (Implicit ())) : int)
          : "<int>{x : mut?'a int; ..'b}"))
        (where "..'_c <= {x : mut?'a int; ..'b}"))))
     (recursive false))
    ((members
      (((ident tail/6) (args (c/18))
        (scheme "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} -> <'a>{..'f}")
        (body
         (Let (ident outer/19) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Let (ident y/20) (args ()) (generalized true)
             (body (Lit (Int 1) : int))
             (in_
              (Let (ident f/22) (args (a/21)) (generalized true)
               (body (Var (Bound (Local a/21)) : 'g))
               (in_
                (Splice
                 (code
                  (Var (Bound (Local c/18)) :
                   "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e}"))
                 (subs (Implicit ((f (Local f/22)) (y (Local y/20))))) : 'a))
               : 'a))
             : 'a)
            : <'a>{..'f}))
          : <'a>{..'f}))
        (where "..'e <= {..'f}"))))
     (recursive false))
    ((members
      (((ident later_level/7) (args (c/23))
        (scheme "<<'a>{y?'b : int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f}")
        (body
         (Quote
          (Quote
           (Let (ident y/24) (args ()) (generalized true)
            (body (Lit (Int 1) : int))
            (in_
             (Splice
              (code
               (Splice
                (code
                 (Var (Bound (Local c/23)) :
                  "<<'a>{y?'b : int; ..'c}>{-y; ..'d}"))
                (subs (Implicit ())) : "<'a>{y?'b : int; ..'c}"))
              (subs (Implicit ((y (Local y/24))))) : 'a))
            : 'a)
           : <'a>{..'e})
          : <<'a>{..'e}>{..'f}))
        (where "..'c <= {..'e}, ..'d <= {..'f}"))))
     (recursive false))
    ((a/21 ('a 1)) (bound/4 (<int>{..'a} 0))
     (c/18 ("<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} where ..'e <= {..'f}" 0))
     (c/23
      ("<<'a>{y?'b : int; ..'c}>{-y; ..'d} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (f/22 ("'a -> 'a" 1))
     (later_level/7
      ("<<'a>{y?'b : int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (outer/19 (int 0)) (propagates/5 ("<int>{x : mut?'a int; ..'b}" 0))
     (tail/6
      ("<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} -> <'a>{..'f} where ..'e <= {..'f}"
       0))
     (x/17 (int 1)) (y/20 (int 1)) (y/24 (int 2)))
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
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident m/5) (args ()) (scheme int)
        (body (Var (Bound (Toplevel n/4)) : int)))))
     (recursive false))
    ((members
      (((ident f/6) (args (x/17)) (scheme "int -> int")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
              (Var (Free x) : int) : "int -> int")
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Free_toplevel n) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : int)
            : "<int>{n : int; x : int}"))
          (expansion
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/17)) : int) : "int -> int")
            (App
             (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
              (Var (Bound (Toplevel n/4)) : int) : "int -> int")
             (Lit (Int 1) : int) : int)
            : int))
          : int)))))
     (recursive false))
    ((members
      (((ident closed/7) (args ()) (scheme int)
        (body
         (Toplevel_splice (code (Quote (Lit (Int 1) : int) : <int>{}))
          (expansion (Lit (Int 1) : int)) : int)))))
     (recursive false))
    ((closed/7 (int 0)) (f/6 ("int -> int" 0)) (m/5 (int 0)) (n/4 (int 0))
     (x/17 (int 0)))
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
let gen n = .<gen n>.
|};
  [%expect
    {|
    ((members
      (((ident even/5) (args (n/19)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/19)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool true) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel odd/6)) : "int -> bool")
           (Var (Bound (Local n/19)) : int) : bool)
          : bool)))
       ((ident odd/6) (args (n/20)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/20)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool false) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel even/5)) : "int -> bool")
           (Var (Bound (Local n/20)) : int) : bool)
          : bool)))))
     (recursive true))
    ((members
      (((ident main/4) (args ()) (scheme bool)
        (body
         (App (Var (Bound (Toplevel even/5)) : "int -> bool") (Lit (Int 3) : int)
          : bool)))))
     (recursive false))
    ((members (((ident b/8) (args ()) (scheme int) (body (Lit (Int 1) : int)))))
     (recursive false))
    ((members
      (((ident a/7) (args ()) (scheme int)
        (body (Var (Bound (Toplevel b/8)) : int)))))
     (recursive false))
    ((members
      (((ident gen/9) (args (n/21))
        (scheme "'a -> <'b>{gen : mut?'c 'd -> 'b; n : mut?'e 'd; ..'f}")
        (body
         (Quote
          (App (Var (Free_toplevel gen) : "'d -> 'b") (Var (Free n) : 'd) : 'b) :
          "<'b>{gen : mut?'c 'd -> 'b; n : mut?'e 'd; ..'f}")))))
     (recursive false))
    |}]
;;

let%expect_test "recursive components" =
  print
    {|
let gen n = .<gen n>.
let a = .<b>.
let b = .<a>.
let loop x = loop x
let captured x = .~(.<captured x>.)
|};
  [%expect
    {|
    ((members
      (((ident gen/4) (args (n/18))
        (scheme "'a -> <'b>{gen : mut?'c 'd -> 'b; n : mut?'e 'd; ..'f}")
        (body
         (Quote
          (App (Var (Free_toplevel gen) : "'d -> 'b") (Var (Free n) : 'd) : 'b) :
          "<'b>{gen : mut?'c 'd -> 'b; n : mut?'e 'd; ..'f}")))))
     (recursive false))
    ((members
      (((ident a/5) (args ()) (scheme "<'a>{b : mut?'b 'a; ..'c}")
        (body (Quote (Var (Free_toplevel b) : 'a) : "<'a>{b : mut?'b 'a; ..'c}")))))
     (recursive false))
    ((members
      (((ident b/6) (args ()) (scheme "<'a>{a : mut?'b 'a; ..'c}")
        (body (Quote (Var (Free_toplevel a) : 'a) : "<'a>{a : mut?'b 'a; ..'c}")))))
     (recursive false))
    ((members
      (((ident loop/7) (args (x/19)) (scheme "'a -> 'b")
        (body
         (App (Var (Bound (Toplevel loop/7)) : "'a -> 'b")
          (Var (Bound (Local x/19)) : 'a) : 'b)))))
     (recursive true))
    ((members
      (((ident captured/8) (args (x/20)) (scheme "'a -> 'b")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App (Var (Free_toplevel captured) : "'a -> 'b") (Var (Free x) : 'a)
             : 'b)
            : "<'b>{captured : 'a -> 'b; x : 'a}"))
          (expansion
           (App (Var (Bound (Toplevel captured/8)) : "'a -> 'b")
            (Var (Bound (Local x/20)) : 'a) : 'b))
          : 'b)))))
     (recursive true))
    |}]
;;

let%expect_test "literals that look like placeholders" =
  print
    {|
let lits s = match s with "\0000" -> ('\000', "\0003")
|};
  [%expect
    {|
    ((members
      (((ident lits/4) (args (s/14)) (scheme "string -> char * string")
        (body
         (Match (scrutinee (Var (Bound (Local s/14)) : string))
          (cases
           (((Lit (String "\0000") : string)
             (Tuple ((Lit (Char "\000") : char) (Lit (String "\0003") : string))
              : "char * string"))))
          : "char * string")))))
     (recursive false))
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
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident persistent_local/5) (args ()) (scheme <int>{..'_a})
        (body
         (Quote
          (Let (ident f/19) (args (x/18)) (generalized true)
           (body (Var (Bound (Local x/18)) : 'b))
           (in_
            (Splice
             (code
              (App
               (Var (Bound (Toplevel id/3)) :
                "<int>{f?'_c : '_d -> '_d; ..'_e} -> <int>{f?'_c : '_d -> '_d; ..'_e}")
               (Quote
                (App (Var (Persistent (Local f/19)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : "<int>{f?'_c : '_d -> '_d; ..'_e}")
               : "<int>{f?'_c : '_d -> '_d; ..'_e}"))
             (subs (Implicit ((f (Local f/19))))) : int))
           : int)
          : <int>{..'_a}))
        (where "..'_e <= {..'_a}"))))
     (recursive false))
    ((members
      (((ident outside_binder/6) (args ()) (scheme "<'a>{x : mut?'b 'a; ..'c}")
        (body
         (Let (ident x/20) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Splice (code (Quote (Var (Free x) : 'a) : "<'a>{x : 'a; ..'_d}"))
             (subs (Implicit ())) : 'a)
            : "<'a>{x : mut?'b 'a; ..'c}"))
          : "<'a>{x : mut?'b 'a; ..'c}"))
        (where "..'_d <= {x : mut?'b 'a; ..'c}"))))
     (recursive false))
    ((members
      (((ident shadowed/7) (args (c/21))
        (scheme "<'a>{x?'b : int; ..'c} -> <'a>{..'d}")
        (body
         (Quote
          (Let (ident x/22) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Let (ident x/23) (args ()) (generalized true)
             (body (Lit (Int 2) : int))
             (in_
              (Splice
               (code (Var (Bound (Local c/21)) : "<'a>{x?'b : int; ..'c}"))
               (subs (Implicit ((x (Local x/23))))) : 'a))
             : 'a))
           : 'a)
          : <'a>{..'d}))
        (where "..'c <= {..'d}"))))
     (recursive false))
    ((members
      (((ident n_shadowed/8) (args ()) (scheme int)
        (body
         (Let (ident n/24) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Toplevel_splice
            (code (Quote (Var (Free n) : int) : "<int>{n : int}"))
            (expansion (Var (Bound (Local n/24)) : int)) : int))
          : int)))))
     (recursive false))
    ((c/21 ("<'a>{x?'b : int; ..'c} where ..'c <= {..'d}" 0))
     (f/19 ("'a -> 'a" 1)) (n/4 (int 0)) (n/24 (int 0)) (n_shadowed/8 (int 0))
     (outside_binder/6 ("<'a>{x : mut?'b 'a; ..'c}" 0))
     (persistent_local/5 (<int>{..'_a} 0))
     (shadowed/7 ("<'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}" 0))
     (x/18 ('a 1)) (x/20 (int 0)) (x/22 (int 1)) (x/23 (int 1)))
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
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident deep/5) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/15) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident k/17) (args (c/16)) (generalized true)
                (body
                 (Let (ident _/18) (args ()) (generalized true)
                  (body
                   (Quote
                    (Let (ident n/19) (args ()) (generalized true)
                     (body (Lit (Int 1) : int))
                     (in_
                      (Splice
                       (code
                        (Var (Bound (Local c/16)) : "<'b>{n?'c : int; ..'d}"))
                       (subs (Implicit ((n (Local n/19))))) : 'b))
                     : 'b)
                    : <'b>{..'e}))
                  (in_ (Var (Bound (Local c/16)) : "<'b>{n?'c : int; ..'d}")) :
                  "<'b>{n?'c : int; ..'d}"))
                (in_
                 (App
                  (Var (Bound (Local k/17)) :
                   "<<int>{}>{x : <int>{}} -> <<int>{}>{x : <int>{}}")
                  (Quote (Var (Free x) : <int>{}) : "<<int>{}>{x : <int>{}}") :
                  "<<int>{}>{x : <int>{}}"))
                : "<<int>{}>{x : <int>{}}"))
              (expansion (Var (Bound (Local x/15)) : <int>{})) : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int))
        (where "..'d <= {..'e}"))))
     (recursive false))
    ((_/18 ("<'a>{..'b} where ..'c <= {..'b}" -2))
     (c/16 ("<'a>{n?'b : int; ..'c}" -2)) (deep/5 (int 0))
     (k/17 ("<'a>{n?'b : int; ..'c} -> <'a>{n?'b : int; ..'c}" -2)) (n/4 (int 0))
     (n/19 (int -1)) (x/15 (<int>{..'a} -1)))
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
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident deep/5) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/15) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident n/16) (args ()) (generalized true)
                (body (Lit (Int 5) : int))
                (in_
                 (Quote
                  (Let (ident _/17) (args ()) (generalized true)
                   (body (Var (Free n) : int)) (in_ (Var (Free x) : <int>{})) :
                   <int>{})
                  : "<<int>{}>{n : int; x : <int>{}}"))
                : "<<int>{}>{n : int; x : <int>{}}"))
              (expansion
               (Let (ident _/18) (args ()) (generalized true)
                (body (Var (Bound (Toplevel n/4)) : int))
                (in_ (Var (Bound (Local x/15)) : <int>{})) : <int>{}))
              : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int)))))
     (recursive false))
    |}]
;;

let%expect_test "explicit substitutions" =
  print
    {|
let n = 3
let persistent_shorthand = let f y = y in .<.~(.<id (f 1)>.).{f; id}>.
let at_toplevel = .~(.<add n y>.).{n; y = 1}
|};
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident persistent_shorthand/5) (args ()) (scheme <int>{..'a})
        (body
         (Let (ident f/17) (args (y/16)) (generalized true)
          (body (Var (Bound (Local y/16)) : 'b))
          (in_
           (Quote
            (Splice
             (code
              (Quote
               (App (Var (Persistent (Toplevel id/3)) : "int -> int")
                (App (Var (Persistent (Local f/17)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : int)
               : "<int>{f?'_c : '_d -> '_d; id?'_e : '_f -> '_f}"))
             (subs
              (Explicit
               ((f (Var (Persistent (Local f/17)) : "'_d -> '_d"))
                (id (Var (Persistent (Toplevel id/3)) : "'_f -> '_f")))))
             : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident at_toplevel/6) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
              (Var (Free_toplevel n) : int) : "int -> int")
             (Var (Free y) : int) : int)
            : "<int>{n : int; y : int}"))
          (expansion
           (Let (ident y/18) (args ()) (generalized false)
            (body (Lit (Int 1) : int))
            (in_
             (App
              (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
               (Var (Bound (Toplevel n/4)) : int) : "int -> int")
              (Var (Bound (Local y/18)) : int) : int))
            : int))
          : int)))))
     (recursive false))
    |}]
;;

let%expect_test "local recursion" =
  print
    {|
let direct y = let f x = f x in f y
let in_quote = let f x = .<f x>. in .<f>.
|};
  [%expect
    {|
    ((members
      (((ident direct/4) (args (y/15)) (scheme "'a -> 'b")
        (body
         (Let (ident f/17) (args (x/16)) (generalized true)
          (body
           (App (Var (Bound (Local f/17)) : "'c -> 'd")
            (Var (Bound (Local x/16)) : 'c) : 'd))
          (in_
           (App (Var (Bound (Local f/17)) : "'a -> 'b")
            (Var (Bound (Local y/15)) : 'a) : 'b))
          : 'b)))))
     (recursive false))
    ((members
      (((ident in_quote/5) (args ())
        (scheme "<'a -> <'b>{f : mut?'c 'd -> 'b; x : mut?'e 'd; ..'f}>{..'g}")
        (body
         (Let (ident f/19) (args (x/18)) (generalized true)
          (body
           (Quote (App (Var (Free f) : "'h -> 'i") (Var (Free x) : 'h) : 'i) :
            "<'i>{f : mut?'j 'h -> 'i; x : mut?'k 'h; ..'l}"))
          (in_
           (Quote
            (Var (Persistent (Local f/19)) :
             "'a -> <'b>{f : mut?'c 'd -> 'b; x : mut?'e 'd; ..'f}")
            : "<'a -> <'b>{f : mut?'c 'd -> 'b; x : mut?'e 'd; ..'f}>{..'g}"))
          : "<'a -> <'b>{f : mut?'c 'd -> 'b; x : mut?'e 'd; ..'f}>{..'g}")))))
     (recursive false))
    |}]
;;

let%expect_test "tuple patterns and annotations" =
  print
    ~idents:true
    {|
let first p = match p with (a, _) -> (a : int)
|};
  [%expect
    {|
    ((members
      (((ident first/4) (args (p/14)) (scheme "int * 'a -> int")
        (body
         (Match (scrutinee (Var (Bound (Local p/14)) : "int * 'a"))
          (cases
           (((Tuple ((Var a/15 : int) (Any : 'a)) : "int * 'a")
             (Annotated (Var (Bound (Local a/15)) : int)
              ((Con (<int> @ t.stag:2:42-45) ()) @ t.stag:2:42-45) : int))))
          : int)))))
     (recursive false))
    ((a/15 (int 0)) (first/4 ("int * 'a -> int" 0)) (p/14 ("int * 'a" 0)))
    |}]
;;
