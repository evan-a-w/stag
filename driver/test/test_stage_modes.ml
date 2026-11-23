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
    val code : <'a>{n : 'a; ..'b}
    |}]
;;

let%expect_test "quote lock: locals are embedded only when portable" =
  check
    {|
let constant = let k = 1 in .<k>.
let closure = let k = 1 in let g u = k in .<g ()>.
let computed = let k = Int.add 1 2 in .<k>.
let argument x = .<x>.
let mutable_ = let mut m = 1 in let g u = m in .<g ()>.
|};
  [%expect
    {|
    val constant : <int>{..'a}
    val closure : <int>{..'a}
    val computed : <'_a>{k : '_a; ..'_b}
    val argument : 'a -> <'b>{x : 'b; ..'c}
    val mutable_ : <'_a>{g : unit -> '_a; ..'_b}
    |}]
;;

let%expect_test "closure lock: a closure is as portable as what it captures" =
  check
    {|
let through_closures =
  let k = Int.add 1 2 in
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
    {|
    This is nonportable, because it applies a function (at t.stag:4:18-29), but must be at most const, because it is annotated const (at t.stag:4:10-15)
    This is nonportable, because it applies a function (at t.stag:5:35-49), but must be at most portable, because it is annotated portable (at t.stag:5:24-32)
    |}]
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
    This is nonportable, because it applies a function (at t.stag:4:14-17), but must be at most const, because applied is a toplevel (at t.stag:4:14-17)
    This is nonportable, because it applies a function (at t.stag:6:27-30), but must be at most const, because computed_counter is a toplevel (at t.stag:6:27-30)
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
let nothing_captured c = .< let h b = .~c in .< h 1 >. >.
|};
  [%expect
    {|
    val argument : <'a>{a?'b : immut 'c; b?'d : immut 'e; h?'f : immut 'e -> 'a; ..'g} -> <'c -> <'h>{h : int -> 'h; ..'i}>{..'j} where ..'g <= {..'j}
    val uses_argument : <'_a -> <'_b>{h : int -> '_b; ..'_c}>{..'_d}
    val mutable_ : <'a>{b?'b : immut 'c; h?'d : immut 'c -> 'a; m?'e : int; ..'f} -> <<'g>{h : int -> 'g; ..'h}>{..'i} where ..'f <= {..'i}
    val uses_mutable : <<'_a>{h : int -> '_a; ..'_b}>{..'_c}
    val nothing_captured : <'a>{b?'b : immut int; h?'c : immut int -> 'a; ..'d} -> <<'a>{..'e}>{..'f} where ..'d <= {..'f}
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
let applied = .~(.<Int.add 1 2>.)
|};
  [%expect
    {|
    val embeds : '_a -> '_a
    val quoted : <'_a -> '_a>{..'b}
    This is portable, because k was computed at compile time (at t.stag:2:33-34), but must be at most const, because embeds is a toplevel (at t.stag:2:13-37)
    This is nonportable, because it applies a function (at t.stag:4:19-30), but must be at most const, because applied is a toplevel (at t.stag:4:14-33)
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
let k @ portable = let n = Int.add 1 2 in fun x -> Int.add x n
let v @ 'm = 1
|};
  [%expect
    {|
    This is nonportable, because it is a parameter, then n is captured by a closure (at t.stag:3:45-46), then the closure is made (at t.stag:3:26-46), but must be at most const, because it is annotated const (at t.stag:3:18-23)
    This is nonportable, because it applies a function (at t.stag:4:27-38), then n is bound (at t.stag:4:19-62), but must be at most portable, because it is annotated portable (at t.stag:4:8-16)
    Mode variables like 'm are not supported on bindings yet
    |}]
;;

let%expect_test "a limit is reported once, at what first exceeds it" =
  modes
    {|
let bad @ const = (Int.add 1 2, Int.add 3 4, Int.add (Int.add 5 6) 7)
|};
  [%expect
    {| This is nonportable, because it applies a function (at t.stag:2:19-30), but must be at most const, because it is annotated const (at t.stag:2:10-15) |}]
;;
