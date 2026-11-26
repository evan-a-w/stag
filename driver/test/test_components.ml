open! Import
open Source_helpers

let check source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

let ext = {|
let ext (x : string) = let y @ external = x in y
|}

let%expect_test "a component bound from a tuple keeps its own mode" =
  check
    (ext
     ^ {|
let key (k : string) = let p = (k, ext "v") in match p with (k, _) -> k
let value (k : string) = let p = (k, ext "v") in match p with (_, v) -> v
let whole (k : string) = let p = (k, ext "v") in p
let direct (k : string) = match (k, ext "v") with (k, _) -> k
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val key : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val value : string @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1) and external <= 'm1
    val whole : string @ 'm0 -> string * string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val direct : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}]
;;

let%expect_test "records, constructors and polymorphic records and variants" =
  check
    (ext
     ^ {|
type r = { a : string; b : string }
type v = V of string * string
let record (k : string) = let r = { a = k; b = ext "b" } in match r with { a = a } -> a
let field (k : string) = let r = { a = k; b = ext "b" } in r.a
let field_ext (k : string) = let r = { a = k; b = ext "b" } in r.b
let construct (k : string) = let x = V (k, ext "b") in match x with V (a, _) -> a
let poly_record (k : string) = let r = {`a = k; `b = ext "b"} in r.`a
let poly_match (k : string) = match {`a = k; `b = ext "b"} with {`a = a; ..} -> a
let poly_variant (k : string) = let x = `A (k, ext "b") in match x with `A (a, _) -> a
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type r = { a : string; b : string }
    type v = V of string * string
    val record : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val field : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val field_ext : string @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1) and external <= 'm1
    val construct : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val poly_record : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val poly_match : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val poly_variant : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}]
;;

let%expect_test "nested by-value components" =
  check
    (ext
     ^ {|
let nested (k : string) =
  let p = ((k, ext "a"), (ext "b", k)) in
  match p with ((a, _), (_, d)) -> (a, d)
let rebound (k : string) =
  let p = ((k, ext "a"), k) in
  match p with (q, _) -> match q with (a, _) -> a
let renamed (k : string) = let p = (k, ext "a") in let q = p in match q with (a, _) -> a
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val nested : string @ 'm0 -> string * string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val rebound : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val renamed : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}]
;;

let%expect_test "match arms join component-wise" =
  check
    (ext
     ^ {|
let arms (b : bool) (k : string) =
  let p = match b with true -> (k, ext "a") | false -> (k, k) in
  match p with (a, _) -> a
let arms_other (b : bool) (k : string) =
  let p = match b with true -> (k, ext "a") | false -> (ext "b", k) in
  match p with (a, _) -> a
let options (b : bool) (k : string) =
  let o = match b with true -> Some (k, ext "a") | false -> None in
  match o with Some (a, _) -> a | None -> k
let opaque_arm (b : bool) (k : string) (q : string * string) =
  let p = match b with true -> (k, ext "a") | false -> q in
  match p with (a, _) -> a
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val arms : bool @ 'm0 -> (string @ 'm2 -> string @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm1
    val arms_other : bool @ 'm0 -> (string @ 'm2 -> string @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and external <= 'm3 and gc <= 'm1
    val options : bool @ 'm0 -> (string @ 'm2 -> string @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm1
    val opaque_arm : bool @ 'm0 -> (string @ 'm2 -> (string * string @ 'm4 -> string @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and external <= 'm5 and gc <= 'm3 and gc <= 'm1
    |}]
;;

let%expect_test "used whole, components join" =
  check
    (ext
     ^ {|
let ident x = x
let passed (k : string) = match ident (k, ext "a") with (a, _) -> a
let boxed (k : string) = match (Box.make (k, ext "a")).* with (a, _) -> a
let annotated (k : string) = let p = ((k, ext "a") @ _) in match p with (a, _) -> a
let mutable_ (k : string) = let mut p = (k, ext "a") in match p with (a, _) -> a
let in_ptr (k : string) = let p = Ptr.make (k, ext "a") in match p.* with (a, _) -> a
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val passed : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and 'm0 <= global and external <= 'm1
    val boxed : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val annotated : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val mutable_ : string @ 'm0 -> string @ 'm1 with release(locality('m0)) <= locality('m1) and nonportable external <= 'm1
    val in_ptr : string @ 'm0 -> string @ 'm1 with 'm0 <= global and nonportable external <= 'm1
    |}]
;;

let%expect_test "field modalities still apply to a component" =
  check
    (ext
     ^ {|
type g = { x : string @@ gc; y : string }
let ok (k : string) = let r = { x = k; y = ext "y" } in r.x
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type g = { x : string @@ gc; y : string }
    val ok : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= gc
    |}];
  check
    (ext
     ^ {|
type g = { x : string @@ gc; y : string }
let bad (k : string) = { x = ext "x"; y = k }
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:5:29-36), but must be at most gc, because it is held by a field or argument with a modality (at t.stag:5:29-36) |}]
;;

let%expect_test "locality: a local component next to a global one" =
  check
    {|
let f (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  match p with (_, g) -> g
let field (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  match p with (l, g) -> Int.add l.* g.*
|};
  [%expect
    {|
    val f : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    val field : int @ _ -> int @ _
    |}];
  check
    {|
let f (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  match p with (l, _) -> l
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:11-32), then it is part of the value (at t.stag:3:11-32), then p is used (at t.stag:4:8-9), then l is used (at t.stag:4:25-26), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:2-4:26) |}];
  check {|
let f (n : int) = let p = ((stack_ (Ptr.make n)), Ptr.make n) in p
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:27-48), then it is part of the value (at t.stag:2:27-48), then it is held (at t.stag:2:27-48), then p is used (at t.stag:2:65-66), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:18-66) |}];
  check
    {|
let f (n : int) = exclave_ (let p = ((stack_ (Ptr.make n)), Ptr.make n) in p)
let g (n : int) = let p = stack_ (Ptr.make n, Ptr.make n) in match p with (_, b) -> b
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:46-56), then p is used (at t.stag:3:67-68), then b is used (at t.stag:3:84-85), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:18-85) |}]
;;

let%expect_test "[stack_ e1, e2] allocates only [e1] locally, also in a \
                 scrutinee"
  =
  Mode_helpers.allocations
    {|
let pair (k : int) =
  let p = (stack_ (Ptr.make 1), Ptr.make k) in
  match (k, p) with (_, (_, b)) -> b
let direct (k : int) =
  match (k, (stack_ (Ptr.make 1), Ptr.make k)) with (_, (_, b)) -> b
let wrapped (k : int) =
  let p = (stack_ (Ptr.make 1), Ptr.make k) in
  match Some p with Some (_, b) -> b | None -> Ptr.make 0
|};
  [%expect
    {|
    val pair : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    val direct : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    val wrapped : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    t.stag:3:18-30 Pointer in pair: region
    t.stag:3:32-42 Pointer in pair: heap
    t.stag:6:20-32 Pointer in direct: region
    t.stag:6:34-44 Pointer in direct: heap
    t.stag:8:18-30 Pointer in wrapped: region
    t.stag:8:32-42 Pointer in wrapped: heap
    t.stag:9:47-57 Pointer in wrapped: heap, caller's region if res is local
    |}];
  check
    {|
let first (k : int) =
  let p = (stack_ (Ptr.make 1), Ptr.make k) in
  match (k, p) with (_, (a, _)) -> a
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:11-30), then it is part of the value (at t.stag:3:11-30), then p is used (at t.stag:4:12-13), then a is used (at t.stag:4:35-36), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:2-4:36) |}]
;;

let%expect_test "closures capture the whole" =
  check
    (ext
     ^ {|
let captured (k : string) = let p = (k, ext "a") in fun u -> match p with (a, _) -> a
let inside (k : string) = let p = (k, ext "a") in (fun u -> match p with (a, _) -> a) ()
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val captured : string @ 'm0 -> ('a @ _ -> string @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val inside : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}];
  check
    {|
let f (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  let g u = match p with (_, b) -> b in
  g () [@nontail]
|};
  [%expect
    {| val f : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0 |}]
;;

let%expect_test "stage: a quote embeds only the component it uses" =
  check
    {|
let f (n : int) =
  let p = (Ptr.make n, "s") in
  match p with (_, s) -> .< s >.
|};
  [%expect
    {| val f : int @ _ -> <string>{..'a} @ 'm0 with nonportable <= 'm0 |}];
  check
    {|
let f (n : int) =
  let p = (Ptr.make n, "s") in
  match p with (q, _) -> .< q >.
|};
  [%expect
    {| The quote can't embed q, as it isn't portable. Write [forget q in ...] to make q a free variable of the quote |}];
  check {|
let f (n : int) = let p = (Ptr.make n, "s") in .< p >.
|};
  [%expect
    {| The quote can't embed p, as it isn't portable. Write [forget p in ...] to make p a free variable of the quote |}];
  check
    {|
type r = { cell : ptr[int]; name : string }
let f (n : int) = let r = { cell = Ptr.make n; name = "s" } in let s = r.name in .< s >.
|};
  [%expect
    {|
    type r = { cell : ptr[int]; name : string }
    val f : int @ _ -> <string>{..'a} @ 'm0 with nonportable <= 'm0
    |}]
;;

let%expect_test "where parts are allocated" =
  Mode_helpers.allocations
    {|
let f (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  match p with (l, g) -> l.* <- 1; g
let g (n : int) =
  let p = (Ptr.make n, Ptr.make n) in
  match p with (l, g) -> l.* <- 1; g
|};
  [%expect
    {|
    val f : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    val g : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    t.stag:3:19-31 Pointer in f: region
    t.stag:3:34-44 Pointer in f: heap
    t.stag:6:11-21 Pointer in g: region
    t.stag:6:23-33 Pointer in g: heap
    |}]
;;

let%expect_test "a component through a variable bound in an outer region" =
  check
    {|
let f (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  region_ (match p with (_, g) -> g)
let h (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  exclave_ (match p with (_, g) -> g)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:6:11-32), then it is part of the value (at t.stag:6:11-32), then it is held (at t.stag:6:11-32), then p is used in an [exclave_] (at t.stag:7:18-19), but must be at most regional, because the [exclave_] has ended its region (at t.stag:7:18-19) |}];
  check
    {|
let h (n : int) =
  let p = ((stack_ (Ptr.make n)), Ptr.make n) in
  exclave_ (match p with (l, _) -> l)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:11-32), then it is part of the value (at t.stag:3:11-32), then it is held (at t.stag:3:11-32), then p is used in an [exclave_] (at t.stag:4:18-19), but must be at most regional, because the [exclave_] has ended its region (at t.stag:4:18-19) |}]
;;

let%expect_test "a component read in a closure, of a let not generalized" =
  (* The pair's modes are of the let's rank, not its body's: else the closure,
     generalized, would take the constraint on [b] with it, and the [region_]
     would hold the box it returns. *)
  print_s
    [%sexp
      (Region_check.run
         {|
let mk2 (a : int) (b : int) = Box.make (Int.add a b)
let f (x : int) =
  region_ (
    let p = ((stack_ (Box.make 1)), mk2 1 x) in
    let g (y : int) = match p with (_, b) -> b in
    g x)
let use (u : unit) = match f 1 with *n -> n
|}
         ~entry:"use"
       : Region_check.Outcome.t)];
  [%expect {| (Returned (value 2) (on_heap 1) (in_regions 2)) |}]
;;

(* What [stack_] allocates of a part is local, and the rest of it as it would
   be, whether or not what uses the value wants its components' modes. *)
let%expect_test "[stack_] places each part alike, however the value is used" =
  Mode_helpers.allocations
    {|
let ident x = x
let shaped (n : int) (g : int -> int) =
  let p = stack_ (Ptr.make n, Box.make g) in
  match p with (a, b) -> Int.add a.* (b.* n)
let whole (n : int) (g : int -> int) =
  let p = ident (stack_ (Ptr.make n, Box.make g)) in
  match p with (a, b) -> Int.add a.* (b.* n)
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val shaped : int @ 'm0 -> ((int -> int) @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val whole : int @ 'm0 -> ((int -> int) @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    t.stag:4:2-5:44 Partial_application in shaped: heap, caller's region if res is local
    t.stag:4:18-28 Pointer in shaped: region
    t.stag:4:30-40 Box in shaped: region
    t.stag:7:2-8:44 Partial_application in whole: heap, caller's region if res is local
    t.stag:7:25-35 Pointer in whole: region
    t.stag:7:37-47 Box in whole: region
    |}]
;;
