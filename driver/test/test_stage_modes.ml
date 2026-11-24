open! Import
open Source_helpers

(* Checks [source] after the test prelude, printing its signature or the
   messages of its errors. *)
let modes ?(const_toplevels = false) source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter ~const_toplevels () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

let%expect_test "toplevel constants resolve directly inside quotes" =
  run {|
let n = 3
let code = .<Int.add n 1>.
let ran = .~code
|};
  [%expect
    {|
    val n : int
    val code : <int>{..'a}
    val ran : int
    -- evaluated at compile time
    (code
     (Code
      (App
       (App (Var (Prim Int.add) : "int -> int -> int")
        (Var (Persistent (Toplevel n/4)) : int) : "int -> int")
       (Lit (Int 1) : int) : int)))
    -- evaluated
    (n 3)
    (code
     (Code
      (App
       (App (Var (Prim Int.add) : "int -> int -> int")
        (Var (Persistent (Toplevel n/4)) : int) : "int -> int")
       (Lit (Int 1) : int) : int)))
    (ran 4)
    |}]
;;

let%expect_test "computed toplevels are free variables of quotes" =
  check {|
let f x = x
let n = f 3
let code = .<n>.
|};
  [%expect
    {|
    val f : 'a -> 'a
    val n : int
    val code : <int>{..'a}
    |}]
;;

let%expect_test "quote lock: locals are embedded only when portable" =
  check
    {|
let constant = let k = 1 in .<k>.
let closure = let k = 1 in let g u = k in .<g ()>.
let computed = let k = Int.to_string 3 in .<k>.
let computed_int = let k = Int.add 1 2 in .<k>.
let argument x = .<x>.
let mutable_ = let mut m = 1 in let g u = m in .<g ()>.
|};
  [%expect
    {|
    val constant : <int>{..'a}
    val closure : <int>{..'a}
    val computed : <'_a>{k : '_a; ..'_b}
    val computed_int : <int>{..'_a}
    val argument : 'a -> <'b>{x : 'b; ..'c}
    val mutable_ : <'_a>{g : unit -> '_a; ..'_b}
    |}]
;;

let%expect_test "closure lock: a closure is as portable as what it captures" =
  check
    {|
let through_closures =
  let k = Int.to_string 3 in
  let f u = k in
  let g u = f () in
  .<g ()>.
let through_data =
  let pair = (1, fun u -> 2) in
  let g u = match pair with (_, h) -> h () in
  .<g ()>.
|};
  [%expect
    {|
    val through_closures : <'_a>{g : unit -> '_a; ..'_b}
    val through_data : <int>{..'a}
    |}]
;;

let%expect_test "generalization is decided by const" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let nil = Nil
let applied = let f x = x in f Nil
let quoted = .<Nil>.
let matched = match Nil with x -> x
let local = let x = Nil in let y = (x, x) in y
let computed = let x = Int.add 1 2 in Nil
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val nil : list['a]
    val applied : list['_a]
    val quoted : <list['a]>{..'b}
    val matched : list['a]
    val local : list['a] * list['b]
    val computed : list['_a]
    |}]
;;

let%expect_test "mode annotations on the stage axis" =
  modes
    {|
let ok @ const = 1
let portable_closure @ portable = let k = 1 in fun u -> k
let bad @ const = Int.add 1 2
let bad_local = let x @ portable = (fun u -> u) 1 in x
|};
  [%expect
    {| This is portable, because it applies a function (at t.stag:4:18-29), but must be at most const, because it is annotated const (at t.stag:4:10-15), then it is bound by an annotated let (at t.stag:4:10-15) |}]
;;

let%expect_test "const_toplevels" =
  let source =
    {|
let n = 1
let f x = x
let applied = f 1
let mut counter = 0
let mut computed_counter = f 0
|}
  in
  modes source;
  modes ~const_toplevels:true source;
  [%expect
    {|
    val n : int
    val f : 'a -> 'a
    val applied : int
    val counter : int
    val computed_counter : int
    This is portable, because it applies a function (at t.stag:4:14-17), but must be at most const, because applied is a toplevel (at t.stag:4:14-17)
    This is portable, because it applies a function (at t.stag:6:27-30), but must be at most const, because computed_counter is a toplevel (at t.stag:6:27-30)
    |}]
;;

let%expect_test "initialization cycles through non-functions are reported" =
  modes {|
let x = f 1
let f y = x
|};
  [%expect {| The value of x may be needed to compute itself: x -> f -> x |}]
;;

let%expect_test "a splice of unknown code captures what it may use" =
  check
    {|
let argument c = .< fun a -> let h b = .~c in .< h 1 >. >.
let uses_argument = argument .<a>.
let mutable_ c = .< let mut m = 1 in let h b = .~c in .< h 1 >. >.
let uses_mutable = mutable_ .<m>.
let free_of_the_quote c = .< let h b = .~c in .< h 1 >. >.
let closed (c : <int>{}) = .< let h b = .~c in .< h 1 >. >.
|};
  [%expect
    {|
    val argument : <'a>{a?'b : immut 'c; b?'d : immut 'e; h?'f : immut 'e -> 'a; ..'g} -> <'c -> <'h>{h : int -> 'h; ..'i}>{..'j} where ..'g <= {..'j}
    val uses_argument : <'_a -> <'_b>{h : int -> '_b; ..'_c}>{..'_d}
    val mutable_ : <'a>{b?'b : immut 'c; h?'d : immut 'c -> 'a; m?'e : int; ..'f} -> <<'g>{h : int -> 'g; ..'h}>{..'i} where ..'f <= {..'i}
    val uses_mutable : <<'_a>{h : int -> '_a; ..'_b}>{..'_c}
    val free_of_the_quote : <'a>{b?'b : immut 'c; h?'d : immut 'c -> 'a; ..'e} -> <<'f>{h : int -> 'f; ..'g}>{..'h} where ..'e <= {..'h}
    val closed : <int>{} -> <<int>{..'a}>{..'b}
    |}]
;;

let%expect_test "a function used in its own definition isn't decided on early" =
  check
    {|
let quote_first n = let g x = let c = .<g>. in Int.add x n in g 1
let quote_last n = let g x = let y = Int.add x n in let c = .<g>. in y in g 1
let bound_first n = let g x = let y = g in Int.add x n in g 1
let bound_last n = let g x = let z = Int.add x n in let y = g in z in g 1
let constant = let g x = let c = .<g>. in let y = g in x in .<g>.
|};
  [%expect
    {|
    val quote_first : int -> int
    val quote_last : int -> int
    val bound_first : int -> int
    val bound_last : int -> int
    val constant : <'a -> 'a>{..'b}
    |}]
;;

let%expect_test "splices leave the locks of their quote" =
  let id = ref 0 in
  let lock name (kind : Env.Lock.Kind.t) =
    incr id;
    name, { Env.Lock.id = !id; kind }
  in
  let mode () = Mode.Var.create (Mode.Id_source.create ()) ~rank:0 in
  let closure name = lock name (Closure (mode ())) in
  let quote name = lock name (Quote (mode ())) in
  let splice name = lock name Splice in
  let names = ref [] in
  let show locks =
    print_s
      [%sexp
        (List.map locks ~f:(fun (lock : Env.Lock.t) ->
           List.Assoc.find_exn !names lock.id ~equal:Int.equal)
         : string list)]
  in
  let make locks =
    names
    := List.map locks ~f:(fun (name, (lock : Env.Lock.t)) -> lock.id, name)
       @ !names;
    List.map locks ~f:snd
  in
  (* .< let y = .. in fun b -> .~(.< y >.) >. *)
  let outer = make [ quote "outer" ] in
  let b = make [ closure "b" ] in
  let inner = make [ quote "inner"; splice "splice" ] in
  let at_use = inner @ b @ outer in
  show (Env.Lock.cancel_splices at_use);
  let y : Env.Entry.t =
    { (Env.Entry.toplevel
         ~ident:{ Typed.Ident.name = "y"; stamp = 0 }
         ~scheme:Type.error
         ~mode:(mode ()))
      with
      locks = outer
    }
  in
  show (Env.Entry.crossed y ~locks:at_use);
  let x = { y with locks = [] } in
  show (Env.Entry.crossed x ~locks:at_use);
  (* .< fun a -> .< fun b -> .~(fun c -> use) >. >. *)
  let nested =
    make
      [ closure "c"
      ; splice "splice'"
      ; closure "b'"
      ; quote "q2"
      ; closure "a"
      ; quote "q1"
      ]
  in
  show (Env.Lock.cancel_splices nested);
  [%expect {|
    (inner)
    (inner b)
    (inner)
    (c a q1)
    |}]
;;

let%expect_test "values computed at compile time are portable, not const" =
  modes {|
let embeds = .~(let k x = x in .<k>.)
let quoted = .<embeds>.
|};
  modes
    ~const_toplevels:true
    {|
let embeds = .~(let k x = x in .<k>.)
let literal = .~(.<1>.)
let applied = .~(.<Int.to_string 3>.)
|};
  [%expect
    {|
    val embeds : '_a -> '_a
    val quoted : <'_a -> '_a>{..'b}
    This is portable, because k was computed at compile time (at t.stag:2:33-34), but must be at most const, because embeds is a toplevel (at t.stag:2:13-37)
    This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:4:19-34), but must be at most const, because applied is a toplevel (at t.stag:4:14-37)
    |}]
;;

let%expect_test "reading through a pointer and matching exceptions compute" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
exception E of list[int]
let cell = Ptr.make Nil
let read = let x = cell.* in x
let read_in_quote = let x = cell.* in .<x>.
let caught = match Nil with x -> x | exception E l -> l
let caught_in_quote f =
  match f () with x -> .<Nil>. | exception E l -> let y = l in .<y>.
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    exception E of list[int]
    val cell : ptr[list['_a]]
    val read : list['_a]
    val read_in_quote : <'_a>{x : '_a; ..'_b}
    val caught : list[int]
    val caught_in_quote : (unit -> 'a) -> <list['b]>{y : list['b]; ..'c}
    |}]
;;

let%expect_test "mode annotations on functions and mode variables" =
  modes
    {|
let f @ const = fun x -> x
let g n = let h @ const = fun x -> Int.add x n in h
let k @ portable = let n = Int.to_string 3 in fun x -> String.append x n
let v @ 'm = 1
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:4:27-42), then n is bound (at t.stag:4:19-72), but must be at most portable, because it is annotated portable (at t.stag:4:8-16), then it is bound by an annotated let (at t.stag:4:8-16) |}]
;;

let%expect_test "a limit is reported once, at what first exceeds it" =
  modes
    {|
let bad @ const = (Int.add 1 2, Int.add 3 4, Int.add (Int.add 5 6) 7)
|};
  [%expect
    {| This is portable, because it applies a function (at t.stag:2:19-30), but must be at most const, because it is annotated const (at t.stag:2:10-15), then it is bound by an annotated let (at t.stag:2:10-15) |}]
;;

let%expect_test "substituting a name the code doesn't need is an error" =
  modes
    {|
let n = 3
let computed = Int.to_string 3
let resolved_directly = .~(.<n>.).{n = true}
let free = .~(.<computed>.).{computed = true}
let closed (c : <int>{}) = .<.~c.{x = 1}>.
let absent (c : <int>{x : int}) = .<let y = 1 in .~c.{x = 1; y}>.
let unknown c = .<.~c.{x = 1}>.
|};
  [%expect
    {|
    n is substituted, but the spliced code has no free variable n
    x is substituted, but the spliced code has no free variable x
    y is substituted, but the spliced code has no free variable y
    |}]
;;

let%expect_test "a quote checks the toplevels it uses first" =
  modes {|
let q = .<later>.
let later = .~(let z = q in .<1>.)
|};
  modes {|
let later = .~(let z = q in .<1>.)
let q = .<later>.
|};
  [%expect
    {|
    The toplevel q is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    val later : int
    val q : <'a>{later : 'a; ..'b}
    |}]
;;

let%expect_test "closures using free variables of the quote through a splice \
                 are not embedded"
  =
  modes
    {|
let q = .< let h a = .~(.<x>.) in .<h>. >.
let q_direct = .< let h a = x in .<h>. >.
let q_param c = .< let h a = .~c in .<h>. >.
|};
  [%expect
    {|
    val q : <<'a>{h : 'a; ..'b}>{x : 'c; ..'d}
    val q_direct : <<'a>{h : 'a; ..'b}>{x : 'c; ..'d}
    val q_param : <'a>{a?'b : immut 'c; h?'d : immut 'c -> 'a; ..'e} -> <<'f>{h : 'f; ..'g}>{..'h} where ..'e <= {..'h}
    |}];
  run
    {|
let q = .< let h a = .~(.<x>.) in .<h>. >.
let g = .< fun (y : int) -> .~q.{x = y} >.
let k : int -> <int -> int>{..'r} = .~g
|};
  [%expect
    {|
    val q : <<'a>{h : 'a; ..'b}>{x : 'c; ..'d}
    val g : <int -> <'a>{h : 'a; ..'b}>{..'c}
    val k : int -> <int -> int>{h : int -> int; ..'a}
    -- evaluated at compile time
    (q
     (Code
      (Let (ident h/17) (args (a/16)) (generalized true)
       (body (Var (Free x) : 'a))
       (in_ (Quote (Var (Free h) : 'b) : "<'b>{h : 'b; ..'c}")) :
       "<'b>{h : 'b; ..'c}")))
    (g
     (Code
      (Fun (args (y/18))
       (annots
        ((y/18 ((ty ((Con (<int> @ t.stag:3:20-23) ()) @ t.stag:3:20-23))))))
       (body
        (Let (ident h/19) (args (a/20)) (generalized true)
         (body (Var (Bound (Local y/18)) : 'a))
         (in_ (Quote (Var (Free h) : 'b) : "<'b>{h : 'b; ..'c}")) :
         "<'b>{h : 'b; ..'c}"))
       : "int -> <'d>{h : 'd; ..'e}")))
    -- evaluated
    (q
     (Code
      (Let (ident h/17) (args (a/16)) (generalized true)
       (body (Var (Free x) : 'a))
       (in_ (Quote (Var (Free h) : 'b) : "<'b>{h : 'b; ..'c}")) :
       "<'b>{h : 'b; ..'c}")))
    (g
     (Code
      (Fun (args (y/18))
       (annots
        ((y/18 ((ty ((Con (<int> @ t.stag:3:20-23) ()) @ t.stag:3:20-23))))))
       (body
        (Let (ident h/19) (args (a/20)) (generalized true)
         (body (Var (Bound (Local y/18)) : 'a))
         (in_ (Quote (Var (Free h) : 'b) : "<'b>{h : 'b; ..'c}")) :
         "<'b>{h : 'b; ..'c}"))
       : "int -> <'d>{h : 'd; ..'e}")))
    (k <fun>)
    |}]
;;

let%expect_test "a closure capturing a function being defined is not embedded" =
  modes
    {|
let outer n =
  let f x = let g u = f in let c = .<g>. in Int.add x n in f 1
let outer2 n =
  let f x = let p = (fun u -> f) in let c = .<p>. in Int.add x n in f 1
|};
  [%expect {|
    val outer : int -> int
    val outer2 : int -> int
    |}]
;;

let%expect_test "a toplevel being checked is nonportable, in either order" =
  modes {|
let r = (.<q>., Int.add 1 2)
let q = let z = r in 3
|};
  [%expect {|
    val r : <'_a>{q : '_a; ..'_b} * int
    val q : int
    |}];
  modes {|
let q = let z = r in 3
let r = (.<q>., Int.add 1 2)
|};
  [%expect {|
    val q : int
    val r : <'_a>{q : '_a; ..'_b} * int
    |}]
;;

let%expect_test "substituting a free variable of a quote inside the generated \
                 code is an error: the inner code is provided it where it is \
                 spliced"
  =
  modes {|
let a = .~(.< .<x>. >.).{x = 1}
let b = .~(.< .<x>. >.)
|};
  [%expect {| x is substituted, but the spliced code has no free variable x |}]
;;

let%expect_test "splices in closures make them nonportable (known limitation)" =
  modes {|
let q = .< fun a -> let p = fun u -> .~(.<1>.) in (p 1, p true) >.
|};
  [%expect
    {| This expression has type bool but an expression was expected of type int |}];
  modes {|
let q = .< fun a -> let h @ const = fun u -> .~(.<1>.) in h >.
|};
  [%expect
    {| This is nonportable, because h is being defined (at t.stag:2:20-59), then h is captured by a closure (at t.stag:2:45-54), then the closure is made (at t.stag:2:36-54), then it is bound by an annotated let (at t.stag:2:28-33), but must be at most const, because it is annotated const (at t.stag:2:28-33) |}]
;;

let%expect_test "data holding a toplevel function being checked is const" =
  let types = {|
type list['a] = Nil | Cons of ptr['a * list['a]]
|} in
  modes
    ~const_toplevels:true
    (types ^ {|
let table = (f, Nil)
let f x = match table with (g, _) -> x
|});
  modes
    ~const_toplevels:true
    (types ^ {|
let f x = match table with (g, _) -> x
let table = (f, Nil)
|});
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val table : ('_a -> '_a) * list['_b]
    val f : '_a -> '_a
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val f : '_a -> '_a
    val table : ('_a -> '_a) * list['_b]
    |}]
;;

let%expect_test "each expansion has its own annotation variables" =
  modes
    {|
let p = .< fun u -> (u : 'a) >.
let use = (.~p 1, .~p true)
let q = .< fun (u : 'a) -> u >.
let use2 = (.~q 1, .~q true)
|};
  [%expect
    {|
    val p : <'a -> 'a>{..'b}
    val use : int * bool
    val q : <'a -> 'a>{..'b}
    val use2 : int * bool
    |}]
;;

let%expect_test "a binding annotation is the variable's mode; an expression's \
                 only bounds it"
  =
  modes {|
let exact = let n @ portable = 1 in let m @ const = n in m
|};
  [%expect
    {| This is portable, because it is annotated portable (at t.stag:2:20-28), then n is used (at t.stag:2:52-53), then it is bound by an annotated let (at t.stag:2:44-49), but must be at most const, because it is annotated const (at t.stag:2:44-49) |}];
  modes
    {|
let checked = let one = 1 in let n = (one @ portable) in let m @ const = n in m
|};
  [%expect {| val checked : int |}];
  modes {|
let n @ portable = 1
let m @ const = n
|};
  [%expect
    {| This is portable, because it is annotated portable (at t.stag:2:8-16), then n is used (at t.stag:3:16-17), but must be at most const, because it is annotated const (at t.stag:3:8-13), then it is bound by an annotated let (at t.stag:3:8-13) |}];
  modes {|
let f u = let p @ local = Ptr.make (1, 2) in p
|};
  [%expect
    {| This is local, because it is annotated local (at t.stag:2:18-23), then p is used (at t.stag:2:45-46), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-46) |}]
;;
