open! Import
open Source_helpers
open Mode_helpers

let round_trip source = round_trip source ~show:`Copies

let modes source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

let%expect_test "arrow modes are polymorphic" =
  modes
    {|
let ident x = x
let apply f x = f x
let const_ x y = x
let n = ident 3
let code = .<n>.
let m = Int.to_string 3
let code2 = .<m>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val apply : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and gc <= 'm3
    val const_ : 'a @ 'm0 -> ('b -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and gc <= 'm1
    val n : int
    val code : <int>{..'a}
    val m : string
    val code2 : <'a>{m : 'a; ..'b}
    |}]
;;

let%expect_test "instances are independent" =
  modes
    {|
let ident x = x
let a = ident 1
let b = ident (Int.to_string 3)
let qa = .<a>.
let qb = .<b>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val a : int
    val b : string
    val qa : <int>{..'a}
    val qb : <'a>{b : 'a; ..'b}
    |}]
;;

let%expect_test "annotations" =
  modes
    {|
let f (x @ portable) = .<x>.
let g : 'a @ 'm -> 'a @ 'm = fun x -> x
let h (x @ 'm) : int @ 'm = x
type t = { k : int @ portable -> int }

|};
  [%expect
    {|
    val f : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and gc <= 'm1
    val g : 'a @ 'm0 -> 'a @ 'm0 with release(locality('m0)) <= locality('m0)
    val h : int @ 'm0 -> int @ 'm0
    type t = { k : int @ portable -> int }
    |}]
;;

let%expect_test "parameters used in quotes are free variables" =
  modes {|
let f x = .<x>.
let g x = let h u = x in .<h>.
|};
  [%expect
    {|
    val f : 'a -> <'b>{x : 'b; ..'c}
    val g : 'a @ 'm0 -> <'b>{h : 'b; ..'c} @ 'm1 with stage('m0) <= stage('m1)
    |}]
;;

let%expect_test "an argument annotated portable must be" =
  modes {|
let f (x @ portable) = .<x>.
let bad = f (Int.to_string 3)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:12-29), then it is passed to a function (at t.stag:3:10-29), but must be at most portable, because it is annotated portable (at t.stag:2:11-19), then the function types are unified |}]
;;

let%expect_test "printed constraints are simplified" =
  modes
    {|
let h (x @ 'm) : int @ 'm = x
let chain f g x = f (g x)
let fixed (x @ portable) : int @ portable = x
let both (x @ portable) = x
|};
  [%expect
    {|
    val h : int @ 'm0 -> int @ 'm0
    val chain : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm7 and 'm0 <= 'm3 and 'm2 <= 'm9 and release(locality('m2)) <= locality('m9) and 'm2 <= regional and 'm4 <= 'm7 and portable <= 'm5 and 'm6 <= 'm1 and 'm6 <= regional and stage('m8) <= stage('m5) and provenance('m8) <= provenance('m5) and meet(locality('m8), regional) <= locality('m5) and gc <= 'm7 and gc <= 'm3
    val fixed : int @ portable -> int @ portable
    val both : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable
    |}]
;;

let%expect_test "printed constraints are exact" =
  (* A field's modality: what is read is at most the record's mode met with
     [gc], on provenance. *)
  round_trip
    {|
type r2 = { x : string @@ gc; y : string }
let fx (r : r2) = r.x
let fy (r : r2) = r.y
|};
  [%expect {|
    let fx_copy : r2 @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1) and meet(provenance('m0), gc) <= provenance('m1) and release(locality('m0)) <= locality('m1) = fx
    let fy_copy : r2 @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) = fy
    |}];
  (* [exclave_]: a regional value is local in the caller. *)
  round_trip
    {|
let mk (x : ptr[int] @ local) = exclave_ stack_ (Ptr.make 1)
let pass (p : ptr[int] @ local) = exclave_ p
let wrap x = exclave_ stack_ (Ptr.make x)
|};
  [%expect {|
    let mk_copy : ptr[int] @ local -> ptr[int @@ 'm1] @ 'm0 with nonportable gc local <= 'm0 = mk
    let pass_copy : ptr[int] @ 'm0 -> ptr[int] @ 'm1 with local <= 'm0 and 'm0 <= 'm1 = pass
    let wrap_copy : 'a @ 'm0 -> ptr['a @@ 'm2] @ 'm1 with 'm0 <= 'm2 and 'm0 <= global and portable <= 'm2 and nonportable gc local <= 'm1 = wrap
    |}];
  (* Crossing: an [int] is the same value at every mode. *)
  round_trip
    {|
let cross (x : int) = x
let pair (x : int) (s : string) = (x, s)
let embed (x : int) = .< x >.
|};
  [%expect {|
    let cross_copy : int -> int = cross
    let pair_copy : int @ 'm0 -> (string @ 'm2 -> int * string @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm1 = pair
    let embed_copy : int -> <int>{..'a} @ 'm0 with gc <= 'm0 = embed
    |}];
  (* [const] is [static], and a value allocated at run time is [gc]. *)
  round_trip
    {|
let closure x = fun (y : int) -> x
let keep (s : string) = s
let pinned : string @ 'm -> string @ 'n with 'm <= const = fun s -> s
|};
  [%expect {|
    let closure_copy : 'a @ 'm0 -> (int -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and gc <= 'm1 = closure
    let keep_copy : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) = keep
    let pinned_copy : string @ 'm0 -> string @ 'm1 with release(locality('m0)) <= locality('m1) and 'm0 <= const = pinned
    |}]
;;

let%expect_test "each written function reads back as the solver's" =
  round_trip
    {|
let diverge (u : unit) = diverge u
let meet_ : 'a @ 'm -> 'a @ 'n with meet(provenance('m), gc) <= provenance('n) = fun x -> diverge ()
let release_ : 'a @ 'm -> 'a @ 'n with release(locality('m)) <= locality('n) = fun x -> diverge ()
let nested : 'a @ 'm -> 'a @ 'n with meet(release(locality('m)), regional) <= locality('n) = fun x -> diverge ()
let static_if : 'a @ 'm -> 'a @ 'n with provenance('n) <= static_if_const(stage('m)) = fun x -> diverge ()
let const_if : 'a @ 'm -> 'a @ 'n with stage('n) <= const_if_static(provenance('m)) = fun x -> diverge ()
|};
  [%expect {|
    let diverge_copy : unit @ 'm0 -> 'a @ global with portable <= 'm0 = diverge
    let meet__copy : 'a @ 'm0 -> 'a @ 'm1 with meet(provenance('m0), gc) <= provenance('m1) = meet_
    let release__copy : 'a @ 'm0 -> 'a @ 'm1 with release(locality('m0)) <= locality('m1) = release_
    let nested_copy : 'a @ 'm0 -> 'a @ 'm1 with meet(locality('m0), regional) <= locality('m1) = nested
    let static_if_copy : 'a @ 'm0 -> 'a @ 'm1 with provenance('m1) <= static_if_const(stage('m0)) = static_if
    let const_if_copy : 'a @ 'm0 -> 'a with gc <= 'm0 = const_if
    |}]
;;

let%expect_test "printed types read back as themselves" =
  round_trip
    {|
let ident x = x
let apply f x = f x
let const_ x y = x
let chain f g x = f (g x)
let f (x @ portable) = .<x>.
let g x = let h u = x in .<h>.
let h (x @ 'm) : int @ 'm = x
let fixed (x @ portable) : int @ portable = x
|};
  [%expect
    {|
    let ident_copy : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) = ident
    let apply_copy : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and gc <= 'm3 = apply
    let const__copy : 'a @ 'm0 -> ('b -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and gc <= 'm1 = const_
    let chain_copy : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm7 and 'm0 <= 'm3 and 'm2 <= 'm9 and release(locality('m2)) <= locality('m9) and 'm2 <= regional and 'm4 <= 'm7 and portable <= 'm5 and 'm6 <= 'm1 and 'm6 <= regional and stage('m8) <= stage('m5) and provenance('m8) <= provenance('m5) and meet(locality('m8), regional) <= locality('m5) and gc <= 'm7 and gc <= 'm3 = chain
    let f_copy : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and gc <= 'm1 = f
    let g_copy : 'a @ 'm0 -> <'b>{h : 'b; ..'c} @ 'm1 with stage('m0) <= stage('m1) = g
    let h_copy : int @ 'm0 -> int @ 'm0 = h
    let fixed_copy : int @ portable -> int @ portable = fixed
    |}]
;;

let%expect_test "annotated constraints constrain" =
  modes
    {|
let loose : 'a @ 'm -> 'a @ 'n = fun x -> x
let tight : 'a @ 'm -> 'a @ 'n with 'n <= 'm = fun x -> x
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let ok = capped 1
|};
  [%expect
    {|
    val loose : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val tight : 'a @ 'm0 -> 'a @ 'm0 with release(locality('m0)) <= locality('m0)
    val capped : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm1 <= portable
    val ok : int
    |}];
  modes
    {|
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let bad = capped (Int.to_string 3)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:17-34), then it is passed to a function (at t.stag:3:10-34), but must be at most portable, because it is annotated 'n <= portable (at t.stag:2:37-51), then the function types are unified, then the function returns it (at t.stag:2:63-64), then x is used (at t.stag:2:63-64), then x is passed to the function (at t.stag:2:54-64), then the function types are unified |}];
  modes
    {|
let pinned : int @ 'm -> int @ 'n with 'n <= 'm and 'm <= const = fun x -> x
|};
  [%expect
    {| This is portable, because it is the result of a function (at t.stag:2:25-33), then it is annotated 'n <= 'm (at t.stag:2:39-47), but must be at most const, because it is annotated 'm <= const (at t.stag:2:52-63) |}];
  modes
    {|
let pinned : int @ 'm -> int @ 'n with 'n <= 'm and 'm <= portable = fun x -> x
|};
  [%expect
    {| val pinned : int @ 'm0 -> int @ 'm1 with 'm0 <= portable and 'm1 <= 'm0 |}]
;;

let%expect_test "equalities between arrows of different modes are kept" =
  modes
    {|
let f x = x
let user = .< let r = f (Int.to_string 1) in let s = String.append r "" in .<r>. >.
let pair c d = .< (.~c, .~d) >.
let both = pair user .< String.append (f "x") "" >.
let prov = .< let mut cell = "a" in let f (u : string) = cell in .~both >.
|};
  [%expect
    {|
    val f : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val user : <<'a>{r : 'a; ..'b}>{..'c}
    val pair : <'a>{..'b} @ 'm0 -> (<'c>{..'d} @ 'm2 -> <'a * 'c>{..'e} @ 'm3) @ 'm1 where ..'b <= {..'e}, ..'d <= {..'e} with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm3 and gc <= 'm1
    val both : <<'_a>{r : '_a; ..'_b} * string>{cell?'_c : string; f?'_d : immut string @ '_m0 -> string @ '_m1; ..'_e} where ..'_e <= {..'_f} with nonportable <= '_m1
    val prov : <<'_a>{r : '_a; ..'_b} * string>{..'_c}
    |}]
;;

let%expect_test "a parameter's mode annotation survives re-inference" =
  modes
    {|
let q = .< fun (x @ portable) -> .<x>. >.
let r = .~q
let s = .~(r 1)
|};
  [%expect
    {|
    val q : <'a @ 'm0 -> <'a>{..'b} @ 'm1>{..'c} with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and gc <= 'm1
    val r : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and gc <= 'm1
    val s : int
    |}]
;;

let%expect_test "results of function parameters are not assumed portable" =
  modes {|
let t f = let r = f 1 in .<r>.
let u = t (Int.add 1)
|};
  [%expect
    {|
    val t : (int -> 'a @ 'm0) -> <'b>{r : 'b; ..'c} @ 'm1 with stage('m0) <= stage('m1)
    val u : <'_a>{r : '_a; ..'_b}
    |}];
  modes
    {|
let b y = let w = a y in y
let a x = let r = b x in .<r>.
let c = a (Int.add 1 2)
|};
  [%expect
    {|
    val b : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val a : 'a @ 'm0 -> <'b>{r : 'b; ..'c} @ 'm1 with portable <= 'm0 and stage('m0) <= stage('m1)
    val c : <'_a>{r : '_a; ..'_b}
    |}];
  modes
    {|
let a x = let r = b x in .<r>.
let b y = let w = a y in y
let c = a (Int.add 1 2)
|};
  [%expect
    {|
    val a : 'a @ 'm0 -> <'b>{r : 'b; ..'c} @ 'm1 with portable <= 'm0 and stage('m0) <= stage('m1)
    val b : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : <'_a>{r : '_a; ..'_b}
    |}]
;;

let%expect_test "mutually recursive toplevels print constraints through each \
                 other"
  =
  round_trip {|
let a x = let r = b x in r
let b y = let w = a y in y
|};
  [%expect
    {|
    let a_copy : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and 'm0 <= global = a
    let b_copy : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and 'm0 <= global and 'm1 <= regional = b
    |}]
;;

let%expect_test "weak mode variables" =
  modes {|
let k = (fun f -> f) (fun w -> w)
let n = k 1
|};
  [%expect
    {|
    val k : int @ '_m0 -> int @ '_m1 with '_m0 <= '_m1 and release(locality('_m0)) <= locality('_m1)
    val n : int
    |}]
;;

let%expect_test "partial applications' constraints have locations" =
  modes
    {|
let make n = let mut cell = n in let f (u : int) = cell in f
let pair x y = x
let p @ portable = pair (make 1)
|};
  [%expect
    {| This is nonportable, because cell is mutable (at t.stag:2:13-60), then cell is captured by a closure (at t.stag:2:51-55), then the closure is made (at t.stag:2:33-60), then f is bound (at t.stag:2:33-60), then the function returns it (at t.stag:2:13-60), then the function types are unified, then the function returns it (at t.stag:4:24-32), then it is passed to a function (at t.stag:4:19-32), then the function types are unified, then a partial application holds it (at t.stag:3:15-16), then the function types are unified, then the function returns it (at t.stag:4:19-32), but must be at most portable, because it is annotated portable (at t.stag:4:8-16), then it is bound by an annotated let (at t.stag:4:8-16) |}]
;;

let%expect_test "annotations on uses bound the variable's mode" =
  modes
    {|
let cap x = (x @ portable)
let cap_with (x : string) = (x : string @ 'm with 'm <= portable)
let same (x @ 'm) = let y = (x @ 'm) in y
|};
  [%expect
    {|
    val cap : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable
    val cap_with : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable
    val same : 'a @ 'm0 -> 'a @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}];
  modes {|
let bad = let mut c = 1 in (c @ portable)
|};
  [%expect
    {| This is nonportable, because c is mutable (at t.stag:2:27-41), but must be at most portable, because it is annotated portable (at t.stag:2:32-40) |}];
  modes {|
let bad = cap (Int.to_string 3)
let cap x = (x @ portable)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:14-31), then it is passed to a function (at t.stag:2:10-31), but must be at most portable, because it is annotated portable (at t.stag:3:17-25), then x is used (at t.stag:3:12-26), then x is passed to the function (at t.stag:3:12-26), then the function types are unified |}]
;;

let%expect_test "a let mut's mode bounds the values it holds" =
  modes {|
let ok u = let mut x @ portable = "a" in x <- "b"; x
|};
  [%expect {| val ok : 'a -> string @ nonportable |}];
  modes {|
let bad u = let mut x @ const = "a" in x <- Int.to_string 3; x
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:44-59), then it is assigned to x (at t.stag:2:39-59), but must be at most const, because it is annotated const (at t.stag:2:24-29) |}];
  modes
    {|
let mut counter @ const = 0
let bump u = counter <- Int.add counter 1
|};
  [%expect
    {| This is portable, because it applies a function (at t.stag:3:24-41), then it is assigned to counter (at t.stag:3:13-41), but must be at most const, because it is annotated const (at t.stag:2:18-23) |}];
  modes {|
let mut counter @ portable = Int.to_string 3
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:29-44), but must be at most portable, because it is annotated portable (at t.stag:2:18-26), then it initializes counter (at t.stag:2:29-44) |}]
;;

let%expect_test "use and let mut annotations survive re-inference" =
  modes
    {|
let q = .< fun x -> (x @ portable) >.
let r = .~q
let q2 = .< fun x -> let mut c @ portable = "a" in c <- x; c >.
let r2 = .~q2
|};
  [%expect
    {|
    val q : <'a @ 'm0 -> 'a @ 'm1>{..'b} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable
    val r : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable
    val q2 : <string @ 'm0 -> string @ 'm1>{..'a} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and nonportable <= 'm1
    val r2 : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and nonportable <= 'm1
    |}]
;;

let%expect_test "provenance bounds read back" =
  round_trip
    {|
let ext (x : string) = let y @ external = x in y
let p = Ptr.make "a"
let store (s : string) = p.* <- s
let keep (s : string) = let y @ static = s in y
let in_ptr u = Ptr.make (ext "a")
let k (a : int) (b : int) = a
let captures (s : string) = fun (v : int) -> s
let closed u = fun (v : int) -> 1
let ints (q : ptr[int]) = q.* <- 1
let strings (q : ptr[string]) (s : string) = q.* <- s
let get_any (p : ptr['a @@ 'm]) = p.*
let id_cell (p : ptr['a @@ 'm]) = p
let static_cell : ptr[string @@ static] = Ptr.make "a"
let ext_cell : ptr[string @@ external] = Ptr.make "a"
let swap_any p q = let v = p.* in p.* <- q.*; q.* <- v
|};
  [%expect
    {|
    let ext_copy : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1 = ext
    let p_copy : ptr[string] = p
    let store_copy : string @ 'm0 -> unit with 'm0 <= gc global = store
    let keep_copy : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= static = keep
    let in_ptr_copy : 'a -> ptr[string @@ 'm1] @ 'm0 with external <= 'm1 and nonportable gc <= 'm0 = in_ptr
    let k_copy : int @ 'm0 -> (int -> int) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1 = k
    let captures_copy : string @ 'm0 -> (int -> string @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and gc <= 'm1 = captures
    let closed_copy : 'a @ 'm0 -> (int -> int) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1 = closed
    let ints_copy : ptr[int] -> unit = ints
    let strings_copy : ptr[string] @ 'm0 -> (string @ 'm2 -> unit) @ 'm1 with 'm0 <= 'm1 and 'm2 <= gc global and gc <= 'm1 = strings
    let get_any_copy : ptr['a @@ 'm0] -> 'a @ 'm1 with 'm0 <= 'm1 = get_any
    let id_cell_copy : ptr['a @@ 'm1] @ 'm0 -> ptr['a @@ 'm1] @ 'm2 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) = id_cell
    let static_cell_copy : ptr[string @@ static] = static_cell
    let ext_cell_copy : ptr[string @@ external] = ext_cell
    let swap_any_copy : ptr['a @@ 'm1] @ 'm0 -> (ptr['a @@ 'm1] -> unit) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and gc <= 'm2 = swap_any
    |}]
;;

let%expect_test "an arrow's result is never const" =
  modes {|
let f : int -> int @ const = fun (x : int) -> 1
|};
  [%expect {| This mode is invalid: the result of a function is never const |}];
  modes {|
let g (h : int -> int @ 'm with 'm <= const) = h
|};
  [%expect
    {| This is portable, because it is the result of a function (at t.stag:2:18-26), but must be at most const, because it is annotated 'm <= const (at t.stag:2:32-43) |}];
  modes {|
type t = { f : int -> int @ const }
|};
  [%expect {| This mode is invalid: the result of a function is never const |}];
  modes {|
type t = { f : int @ const -> int @ portable }
|};
  [%expect {| type t = { f : int @ const static -> int @ portable } |}]
;;

let%expect_test "a relation between variables is on every axis, or on the one \
                 named"
  =
  (* Through [through_all], [f]'s result is as external as its argument, so the
     result is [external]; through [through_stage], it need not be. *)
  modes
    {|
let ext (x : string) = let y @ external = x in y
let through_all (f : string @ 'm -> string @ 'n with 'm <= 'n) (x : string) = f (ext x)
let through_stage (f : string @ 'm -> string @ 'n with stage('m) <= stage('n)) (x : string) = f (ext x)
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val through_all : (string @ 'm1 -> string @ 'm2) @ 'm0 -> (string @ 'm4 -> string @ 'm5) @ 'm3 with 'm0 <= 'm3 and external <= 'm1 and 'm1 <= 'm2 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and 'm4 <= 'm1 and 'm4 <= global and gc <= 'm3
    val through_stage : (string @ 'm1 -> string @ 'm2) @ 'm0 -> (string @ 'm4 -> string @ 'm5) @ 'm3 with 'm0 <= 'm3 and external <= 'm1 and stage('m1) <= stage('m2) and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and 'm4 <= 'm1 and 'm4 <= global and gc <= 'm3
    |}]
;;

let%expect_test "relations on some axes read back" =
  round_trip
    {|
let pick (x : string) (y : string) = let z @ external = y in x
let ext (x : string) = let y @ external = x in y
let through_stage (f : string @ 'm -> string @ 'n with stage('m) <= stage('n)) (x : string) = f (ext x)
let fixed_stage u = Int.to_string 1
let fixed_and_related (x : string) = let y @ nonportable = x in y
let local_closure (x @ local) = exclave_ stack_ (fun (u : int) -> x)
let apply f x = f x
|};
  [%expect
    {|
    let pick_copy : string @ 'm0 -> (string @ 'm2 -> string @ 'm3) @ 'm1 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and stage('m2) <= stage('m3) and gc <= 'm1 = pick
    let ext_copy : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1 = ext
    let through_stage_copy : (string @ 'm1 -> string @ 'm2) @ 'm0 -> (string @ 'm4 -> string @ 'm5) @ 'm3 with 'm0 <= 'm3 and external <= 'm1 and stage('m1) <= stage('m2) and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and 'm4 <= 'm1 and 'm4 <= global and gc <= 'm3 = through_stage
    let fixed_stage_copy : 'a -> string @ 'm0 with nonportable gc <= 'm0 = fixed_stage
    let fixed_and_related_copy : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and nonportable <= 'm1 = fixed_and_related
    let local_closure_copy : 'a @ 'm0 -> (int -> 'a @ 'm2) @ 'm1 with local <= 'm0 and 'm0 <= 'm2 and 'm0 <= 'm1 and gc <= 'm1 = local_closure
    let apply_copy : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and gc <= 'm3 = apply
    |}]
;;
