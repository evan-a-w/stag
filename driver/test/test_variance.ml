open! Import
open Source_helpers
open Mode_helpers

let check source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

let ext = {|
let ext (x : string) = let y @ external = x in y
|}

let%expect_test "an argument's modes are contravariant" =
  (* A function taking local arguments may be used where one taking global ones
     is expected, but not the reverse. *)
  check
    {|
let apply (f : string -> int) = f "a"
let local_ok = apply (fun (s @ local) -> String.length s)
let annotated : string -> int = fun (s @ local) -> String.length s
|};
  [%expect
    {|
    val apply : (string -> int) @ _ -> int @ _
    val local_ok : int
    val annotated : string -> int
    |}];
  check
    {|
let apply (f : string @ local -> int) = f "a"
let global_only = apply (fun (s : string) -> let g @ global = s in String.length g)
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:2:24-29), then the function types are unified, then s is passed to the function (at t.stag:3:24-83), then s is used (at t.stag:3:62-63), then it is bound by an annotated let (at t.stag:3:53-59), but must be at most global, because it is annotated global (at t.stag:3:53-59) |}]
;;

let%expect_test "a result's modes are covariant" =
  (* A function returning gc strings may be used where one returning external
     ones is expected, but not the reverse. *)
  check
    (ext
     ^ {|
let take (f : unit -> string @ external) = f ()
let gc_ok = take (fun (u : unit) -> "a")
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val take : (unit -> string @ external) @ _ -> string @ 'm0 with nonportable external <= 'm0
    val gc_ok : string @ external
    |}];
  check
    (ext
     ^ {|
let take (f : unit -> string) = f ()
let external_bad = take (fun (u : unit) -> ext "a")
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then a function is used at a type with other modes, then the function returns it (at t.stag:5:43-50), but must be at most gc, because its type is written without a mode (at t.stag:4:22-28), then the function types are unified |}]
;;

let%expect_test "nested arrows compose: an argument's argument is covariant" =
  check
    {|
let twice (k : (string @ local -> int) -> int) = k (fun (s @ local) -> String.length s)
let ok = twice (fun (g : string -> int) -> g "a")
|};
  [%expect
    {|
    val twice : ((string @ local -> int) -> int) @ _ -> int @ _
    val ok : int
    |}];
  check
    {|
let twice (k : (string -> int) -> int) = k (fun (s : string) -> String.length s)
let bad = twice (fun (g : string @ local -> int) -> g "a")
|};
  [%expect
    {|
    This expression has type string @ local -> int but an expression was expected of type string -> int
    Their modes differ: This is local, because it is annotated local (at t.stag:3:35-40), then a function is used at a type with other modes, but must be at most global, because its type is written without a mode (at t.stag:2:16-22)
    |}]
;;

let%expect_test "arms and elements join, in either order" =
  (* Each arm is used at the match's type, made of new modes: a function taking
     only global strings, whichever arm comes first. *)
  check
    {|
let pick (b : bool) =
  match b with
  | true -> (fun (s @ local) -> String.length s)
  | false -> (fun (s : string) -> let g @ global = s in String.length g)
let pick2 (b : bool) =
  match b with
  | true -> (fun (s : string) -> let g @ global = s in String.length g)
  | false -> (fun (s @ local) -> String.length s)
let both = [ (fun (s @ local) -> String.length s); (fun (s : string) -> let g @ global = s in String.length g) ]
let both2 = [ (fun (s : string) -> let g @ global = s in String.length g); (fun (s @ local) -> String.length s) ]
|};
  [%expect {| |}];
  (* Neither accepts a local string. *)
  check
    {|
let pick (b : bool) =
  match b with
  | true -> (fun (s @ local) -> String.length s)
  | false -> (fun (s : string) -> let g @ global = s in String.length g)
let use (s @ local) = pick true s
|};
  [%expect {| |}];
  check
    {|
let pick (b : bool) =
  match b with
  | true -> (fun (s : string) -> let g @ global = s in String.length g)
  | false -> (fun (s @ local) -> String.length s)
let use (s @ local) = pick true s
|};
  [%expect {| |}]
;;

let%expect_test "_ can't be constrained or declared" =
  check {|let f : string @ _ -> string @ 'm with _ <= 'm = fun s -> s|};
  [%expect {| |}];
  check {|type t = { f : string @ _ -> string }|};
  [%expect {| |}]
;;

let%expect_test "an immutable box is covariant" =
  (* A box of gc strings may be used as a box of external ones. *)
  check
    (ext
     ^ {|
let read (b : box[string -> int]) = b.* "a"
let boxed = Box.make (fun (s @ local) -> String.length s)
let used = read boxed
let get (b : box[string]) = b.*
let matched (b : box['a]) = match b with *x -> x
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val read : box[string -> int] @ _ -> int @ _
    val boxed : box[string @ '_m0 -> int @ '_m1] with nonportable gc local <= '_m0 and '_m1 <= gc global
    val used : int
    val get : box[string] @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val matched : box['a] @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}];
  (* Reading a box is reading a part: no more than the box's mode. *)
  check
    {|
let bad (s @ local) = let b = Box.make s in let r @ global = b.* in r
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:2:13-18), then s is passed to the function (at t.stag:2:22-69), then s is used (at t.stag:2:39-40), then it is passed to a function (at t.stag:2:30-40), then a function is used at a type with other modes, then Box.make holds it, then a function is used at a type with other modes, then the function returns it (at t.stag:2:30-40), then b is used (at t.stag:2:61-62), then it is dereferenced (at t.stag:2:61-64), then it is bound by an annotated let (at t.stag:2:52-58), but must be at most global, because it is annotated global (at t.stag:2:52-58) |}]
;;

let%expect_test "a box can't be stored to" =
  check {|
let set (b : box[int]) = b.* <- 1
|};
  [%expect {| A box is immutable: only a pointer ([ptr]) can be stored to |}];
  check {|
type r = { x : int }
let set (b : box[r]) = b.*.x <- 1
|};
  [%expect {| A box is immutable: only a pointer ([ptr]) can be stored to |}]
;;

let%expect_test "a pointer is invariant" =
  (* Unlike a box, a pointer's cell is read and written: what it holds can't be
     widened, as another alias may store into it. *)
  check
    (ext
     ^ {|
let p : ptr[string] = Ptr.make "a"
let widen (q : ptr[string @@ external]) = q.* <- ext "b"
let bad = widen p
|}
    );
  [%expect
    {|
    This expression has type ptr[string] but an expression was expected of type ptr[string @@ external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}];
  (* Nor narrowed: a reader expecting gc values would see the external ones
     stored by the alias. *)
  check
    (ext
     ^ {|
let q : ptr[string @@ external] = Ptr.make (ext "a")
let read (p : ptr[string]) = p.*
let bad = read q
|}
    );
  [%expect
    {|
    This expression has type ptr[string @@ external] but an expression was expected of type ptr[string]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}]
;;

let%expect_test "a list is covariant: its cells are boxes" =
  check
    (ext
     ^ {|
let count (fs : list[string -> int]) = fs
let fs : list[string -> int] = [fun (s @ local) -> String.length s]
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val count : list[string -> int] @ 'm0 -> list[string -> int] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val fs : list[string -> int]
    |}]
;;

let%expect_test "a mode parameter's variance is inferred from where it is" =
  check
    (ext
     ^ {|
type co['p : mode] = { v : string @@ 'p }
type nested['p : mode] = { n : co['p] }
let nested_ok (x : nested[gc]) : nested[external] = x
let co_ok (x : co[gc]) : co[external] = x
let co_bad (x : co[external]) : co[gc] = x
|}
    );
  [%expect
    {|
    This expression has type co[external] but an expression was expected of type co[gc]
    Their modes differ: This is external, because the types are unified, then a type is used at one with another mode, but must be at most gc, because the types are unified
    |}];
  check
    {|
type cell['p : mode] = { c : ptr[string @@ 'p] }
let cell_bad (x : cell[gc]) : cell[external] = x
|};
  [%expect
    {|
    This expression has type cell[gc] but an expression was expected of type cell[external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type contra['p : mode] = { f : co['p] -> unit }
let contra_ok (x : contra[external]) : contra[gc] = x
let contra_bad (x : contra[gc]) : contra[external] = x
|};
  [%expect
    {|
    This expression has type contra[gc] but an expression was expected of type contra[external]
    Their modes differ: This is external, because the types are unified, then a type is used at one with another mode, but must be at most gc, because the types are unified
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type contra['p : mode] = { f : co['p] -> unit }
type flipped['p : mode] = { g : contra['p] -> unit }
let flipped_ok (x : flipped[gc]) : flipped[external] = x
let flipped_bad (x : flipped[external]) : flipped[gc] = x
|};
  [%expect
    {|
    This expression has type flipped[external] but an expression was expected of type flipped[gc]
    Their modes differ: This is external, because the types are unified, then a type is used at one with another mode, but must be at most gc, because the types are unified
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type recursive['p : mode] = Leaf | Node of co['p] * box[recursive['p]]
let rec_ok (x : recursive[gc]) : recursive[external] = x
let rec_bad (x : recursive[external]) : recursive[gc] = x
|};
  [%expect
    {|
    This expression has type recursive[external] but an expression was expected of type recursive[gc]
    Their modes differ: This is external, because the types are unified, then a type is used at one with another mode, but must be at most gc, because the types are unified
    |}]
;;

let%expect_test "the aliasing reproducer is still rejected" =
  check
    (ext
     ^ {|
let p : ptr[string] = Ptr.make "a"
let put (q : ptr[string] @ external) (e : string @ external) = q.* <- e
let bad = put p (ext "c")
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:5:51-59), then e is passed to the function (at t.stag:5:63-71), then e is used (at t.stag:5:70-71), but must be at most gc, because it is stored into a cell, which holds only what fits its modality (at t.stag:5:70-71) |}]
;;

let%expect_test "printed types round trip" =
  round_trip
    (ext
     ^ {|
let apply (f : string -> int) = f "a"
let twice (k : (string @ local -> int) -> int) = k (fun (s @ local) -> String.length s)
let read (b : box[string]) = b.*
type co['p : mode] = { v : string @@ 'p }
let unwrap (x : co['p]) = x.v
let ident x = x
let compose f g x = f (g x)
|}
    )
    ~show:`Signature;
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val apply : (string -> int) @ _ -> int @ _
    val twice : ((string @ local -> int) -> int) @ _ -> int @ _
    val read : box[string] @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    type co['m0 : mode] = { v : string @@ 'm0 }
    val unwrap : co['m1] @ 'm0 -> string @ 'm2 with release(locality('m0)) <= locality('m2) and 'm1 <= 'm2
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val compose : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm7 and 'm0 <= 'm3 and 'm2 <= 'm9 and release(locality('m2)) <= locality('m9) and 'm2 <= regional and 'm4 <= 'm7 and portable <= 'm5 and 'm6 <= 'm1 and 'm6 <= regional and stage('m8) <= stage('m5) and provenance('m8) <= provenance('m5) and meet(locality('m8), regional) <= locality('m5) and gc <= 'm7 and gc <= 'm3
    |}]
;;

let%expect_test "a cyclic alias's parameter is invariant when reached both ways" =
  (* [f['a]] reaches its own expansion contravariantly (in [box[f['a]] ->]) and
     ['a] covariantly, so ['a] is invariant in [r], and a local-accepting
     function in [r1] can't be passed off as a global-accepting one. *)
  check
    {|
type f['a] = box[f['a]] -> 'a
type r['a] = { x : f['a] }
let cell : ptr[ptr[int]] = Ptr.make (Ptr.make 0)
let pf : f[ptr[int] @ local -> unit] =
  fun (b : box[f[ptr[int] @ local -> unit]]) ->
    let k = b.* b in
    let u = k (stack_ (Ptr.make 1)) in
    k
let r1 : r[ptr[int] @ local -> unit] = { x = pf }
let r2 : r[ptr[int] -> unit] = r1
|};
  [%expect {|
    This expression has type ptr[int] @ local -> unit but an expression was expected of type ptr[int] -> unit
    Their modes differ: This is local, because it is annotated local (at t.stag:10:22-27), then the function types are unified, but must be at most global, because its type is written without a mode (at t.stag:11:11-19)
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type f['p : mode] = box[f['p]] -> co['p]
type r['p : mode] = { x : f['p] }
let widen (r : r[gc]) : r[external] = r
|};
  [%expect {|
    This expression has type r[gc] but an expression was expected of type r[external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type f['p : mode] = box[f['p]] -> co['p]
type r['p : mode] = { x : f['p] }
let narrow (r : r[external]) : r[gc] = r
|};
  [%expect {|
    This expression has type r[external] but an expression was expected of type r[gc]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}]
;;


let%expect_test "scratch" =
  check {|
let pick (b : bool) =
  match b with
  | true -> (fun (s @ local) -> String.length s)
  | false -> (fun (s : string) -> String.length s)
|};
  check {|
let f = fun (s @ local) -> String.length s
let pick (b : bool) =
  match b with
  | true -> f
  | false -> (fun (s : string) -> let g @ global = s in String.length g)
|};
  check {|
let g = (fun (s : string) -> let g @ global = s in String.length g)
let pick (b : bool) =
  match b with
  | true -> (fun (s @ local) -> String.length s)
  | false -> g
|};
  [%expect {| |}]
