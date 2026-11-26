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
    {| This is regional, because it is annotated local (at t.stag:2:24-29), then s is passed to the function (at t.stag:3:24-83), then s is used (at t.stag:3:62-63), then it is bound by an annotated let (at t.stag:3:53-59), but must be at most global, because it is annotated global (at t.stag:3:53-59) |}]
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
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it, by a tail call (at t.stag:5:43-50), but must be at most gc, because its type is written without a mode (at t.stag:4:22-28) |}]
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
    Their modes differ: This is local, because it is annotated local (at t.stag:3:35-40), but must be at most global, because its type is written without a mode (at t.stag:2:16-22)
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
  [%expect
    {|
    val pick : bool @ _ -> (string @ 'm0 -> int @ _) @ _ with 'm0 <= global
    val pick2 : bool @ _ -> (string @ 'm0 -> int @ _) @ _ with 'm0 <= global
    val both : list[string @ '_m0 -> int @ '_m1] with '_m0 <= global
    val both2 : list[string @ '_m0 -> int @ '_m1] with '_m0 <= global
    |}];
  (* Neither accepts a local string. *)
  check
    {|
let pick (b : bool) =
  match b with
  | true -> (fun (s @ local) -> String.length s)
  | false -> (fun (s : string) -> let g @ global = s in String.length g)
let use (s @ local) = pick true s
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:6:13-18), then s is passed to the function (at t.stag:6:22-33), then s is used (at t.stag:6:32-33), then its type turned out to be string, which has pointers, then it is passed to a function (at t.stag:6:22-33), but must be at most global, because it is annotated global (at t.stag:5:42-48), then it is bound by an annotated let (at t.stag:5:42-48), then s is used (at t.stag:5:51-52), then s is passed to the function (at t.stag:5:13-72) |}];
  check
    {|
let pick (b : bool) =
  match b with
  | true -> (fun (s : string) -> let g @ global = s in String.length g)
  | false -> (fun (s @ local) -> String.length s)
let use (s @ local) = pick true s
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:6:13-18), then s is passed to the function (at t.stag:6:22-33), then s is used (at t.stag:6:32-33), then its type turned out to be string, which has pointers, then it is passed to a function (at t.stag:6:22-33), but must be at most global, because it is annotated global (at t.stag:4:41-47), then it is bound by an annotated let (at t.stag:4:41-47), then s is used (at t.stag:4:50-51), then s is passed to the function (at t.stag:4:12-71) |}]
;;

let%expect_test "_ can't be constrained or declared" =
  check {|let f : string @ _ -> string @ 'm with _ <= 'm = fun s -> s|};
  [%expect
    {| The anonymous mode _ can't be constrained: name a mode variable like 'm instead |}];
  check {|type t = { f : string @ _ -> string }|};
  [%expect
    {| The anonymous mode _ is not allowed in a declaration: its arrows have fixed modes, written as atoms like [local], or a mode parameter declared ['p : mode] |}]
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
    val boxed : box[string @ '_m0 -> int @ '_m1] with nonportable gc <= '_m0 and '_m1 <= global gc
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
    {| This is regional, because it is annotated local (at t.stag:2:13-18), then s is passed to the function (at t.stag:2:22-69), then s is used (at t.stag:2:39-40), then it is passed to a function (at t.stag:2:30-40), but must be at most global, because it is annotated global (at t.stag:2:52-58), then it is bound by an annotated let (at t.stag:2:52-58), then it is dereferenced (at t.stag:2:61-64), then b is used (at t.stag:2:61-62), then the function returns it (at t.stag:2:30-40), then Box.make holds it |}]
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
    Their modes differ: The modes external and gc must be equal, because the types are unified
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
    Their modes differ: The modes gc and external must be equal, because the types are unified
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
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
    |}];
  check
    {|
type cell['p : mode] = { c : ptr[string @@ 'p] }
let cell_bad (x : cell[gc]) : cell[external] = x
|};
  [%expect
    {|
    This expression has type cell[gc] but an expression was expected of type cell[external]
    Their modes differ: The modes external and gc must be equal, because the types are unified
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
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
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
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
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
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
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
    val compose : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm7 and 'm0 <= 'm3 and 'm2 <= 'm9 and 'm4 <= 'm7 and portable <= 'm5 and 'm6 <= 'm1 and 'm6 <= regional and stage('m8) <= stage('m5) and provenance('m8) <= provenance('m5) and meet(locality('m8), regional) <= locality('m5) and gc <= 'm7 and gc <= 'm3
    |}]
;;

let%expect_test "fixed modes of nested arrows round trip" =
  (* Variables fixed to different constants are never printed as one. *)
  round_trip
    {|
let a (k : (string @ local -> int) -> int) = 1
let b (k : ((string @ local -> int) -> int) -> int) = 1
let c (k : (string @ local -> string) -> string @ local -> int) = 1
let d (k : (string @ external -> int) -> int) = 1
let e (k : (string @ local external -> int) -> (string @ regional -> int) -> int) = 1
|}
    ~show:`Signature;
  [%expect
    {|
    val a : ((string @ local -> int) -> int) @ _ -> int @ _
    val b : (((string @ local -> int) -> int) -> int) @ _ -> int @ _
    val c : ((string @ local -> string) -> string @ local -> int) @ _ -> int @ _
    val d : ((string @ external -> int) -> int) @ _ -> int @ _
    val e : ((string @ local external -> int) -> (string @ regional -> int) -> int) @ _ -> int @ _
    |}]
;;

let%expect_test "a cyclic alias's parameter is invariant when reached both ways"
  =
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
  [%expect
    {|
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
  [%expect
    {|
    This expression has type r[gc] but an expression was expected of type r[external]
    Their modes differ: The modes external and gc must be equal, because the types are unified
    |}];
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type f['p : mode] = box[f['p]] -> co['p]
type r['p : mode] = { x : f['p] }
let narrow (r : r[external]) : r[gc] = r
|};
  [%expect
    {|
    This expression has type r[external] but an expression was expected of type r[gc]
    Their modes differ: The modes gc and external must be equal, because the types are unified
    |}]
;;

let%expect_test "weak toplevels keep their instances' modes" =
  (* A polymorphic function's instance is a join of new modes, which a weak
     toplevel fixes at the end of its definition: where it would be used, at the
     bound its own type gave it, so a contravariant parameter stays as declared. *)
  check
    {|
type co['p : mode] = { v : string @@ 'p }
type contra['p : mode] = { f : co['p] -> unit }
let diverge (u : unit) = diverge u
let ident x = x
let f1 : contra[external] = diverge ()
let x = ident f1
let g : contra[external] = x
let p = Ptr.make f1
let q : ptr[contra[external]] = p
let c = Ptr.make "a"
|};
  [%expect
    {|
    type co['m0 : mode] = { v : string @@ 'm0 }
    type contra['m0 : mode] = { f : co['m0] -> unit }
    val diverge : unit @ 'm0 -> 'a @ _ with portable <= 'm0
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val f1 : contra[external]
    val x : contra[external]
    val g : contra[external]
    val p : ptr[contra[external]]
    val q : ptr[contra[external]]
    val c : ptr[string]
    |}]
;;

let%expect_test "an arm of unknown type joins too, in either order" =
  check
    {|
let f b x = match b with | true -> (fun (s @ local) -> String.length s) | false -> x
let f2 b x = match b with | true -> x | false -> (fun (s @ local) -> String.length s)
|};
  [%expect
    {|
    val f : bool @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm4 <= 'm7 and 'm6 <= 'm3 and gc <= 'm1
    val f2 : bool @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm4 <= 'm7 and 'm6 <= 'm3 and gc <= 'm1
    |}]
;;

(* Every order of the same arms gives the same signature. *)
let permutations l =
  let rec go = function
    | [] -> [ [] ]
    | l ->
      List.concat_mapi l ~f:(fun i x ->
        let rest = List.filteri l ~f:(fun j _ -> j <> i) in
        List.map (go rest) ~f:(fun p -> x :: p))
  in
  go l
;;

let same_in_every_order ~prefix ~arms ~make =
  let signatures =
    List.map (permutations arms) ~f:(fun arms ->
      match signature (prefix ^ make arms) with
      | Ok signature -> signature
      | Error errors -> String.concat ~sep:"\n" errors)
    |> List.dedup_and_sort ~compare:String.compare
  in
  match signatures with
  | [ signature ] -> print_string signature
  | signatures ->
    print_endline "differs by order:";
    List.iter signatures ~f:print_endline
;;

let local_arm = "(fun (s @ local) -> String.length s)"
let global_arm = "(fun (s : string) -> let g @ global = s in String.length g)"

let%expect_test "arms join in every order, known or not" =
  let match_of arms =
    sprintf
      "let f (n : int) x y = match n with %s\n"
      (String.concat
         ~sep:" "
         (List.mapi arms ~f:(fun i arm ->
            if i = List.length arms - 1
            then sprintf "| _ -> %s" arm
            else sprintf "| %d -> %s" i arm)))
  in
  same_in_every_order ~prefix:"" ~arms:[ local_arm; "x"; "y" ] ~make:match_of;
  [%expect
    {| val f : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> ((string @ 'm7 -> int @ 'm8) @ 'm6 -> (string @ 'm10 -> int @ 'm11) @ 'm9) @ 'm5) @ 'm1 with 'm0 <= 'm5 and 'm0 <= 'm1 and 'm2 <= 'm9 and release(locality('m2)) <= locality('m9) and 'm2 <= 'm5 and 'm4 <= 'm11 and 'm6 <= 'm9 and release(locality('m6)) <= locality('m9) and 'm8 <= 'm11 and 'm10 <= 'm3 and 'm10 <= 'm7 and gc <= 'm5 and gc <= 'm1 |}];
  same_in_every_order
    ~prefix:""
    ~arms:[ local_arm; global_arm; "x" ]
    ~make:(fun arms ->
      match_of arms ^ "let g = f 0 (fun (s : string) -> 1) ()\n");
  [%expect
    {|
    val f : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> ('a @ _ -> (string @ 'm7 -> int @ 'm8) @ 'm6) @ 'm5) @ 'm1 with 'm0 <= 'm5 and 'm0 <= 'm1 and 'm2 <= 'm6 and release(locality('m2)) <= locality('m6) and 'm2 <= 'm5 and 'm4 <= 'm8 and 'm7 <= 'm3 and 'm7 <= global and gc <= 'm5 and gc <= 'm1
    val g : string @ '_m0 -> int @ '_m1 with '_m0 <= global
    |}];
  same_in_every_order
    ~prefix:""
    ~arms:[ local_arm; global_arm; "x" ]
    ~make:(fun arms ->
      sprintf "let l x = [ %s ]\n" (String.concat ~sep:"; " arms));
  [%expect
    {| val l : (string @ 'm1 -> int @ 'm2) @ 'm0 -> list[string @ 'm4 -> int @ 'm5] @ 'm3 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm2 <= 'm5 and 'm4 <= 'm1 and 'm4 <= global and gc <= 'm3 |}]
;;

let%expect_test "a join's type known before, after, or inside a let" =
  (* [x]'s type is only known once it is applied to a string: before the match,
     after it, or inside a let in an arm. Each accepts a local string. *)
  check
    {|
let before (n : int) x = let _ = x "a" in match n with | 0 -> x | _ -> (fun (s @ local) -> String.length s)
let after (n : int) x = let r = match n with | 0 -> x | _ -> (fun (s @ local) -> String.length s) in let _ = x "a" in r
let inside (n : int) x = match n with | 0 -> (let _ = x "a" in x) | _ -> (fun (s @ local) -> String.length s)
let use_before (s @ local) = before 0 (fun (t @ local) -> 1) s
let use_after (s @ local) = after 0 (fun (t @ local) -> 1) s
let use_inside (s @ local) = inside 0 (fun (t @ local) -> 1) s
|};
  [%expect
    {|
    val before : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm4 <= 'm7 and 'm6 <= 'm3 and gc <= 'm1
    val after : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm4 <= 'm7 and 'm6 <= 'm3 and gc <= 'm1
    val inside : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm4 <= 'm7 and 'm6 <= 'm3 and gc <= 'm1
    val use_before : string @ 'm0 -> int @ _ with local <= 'm0
    val use_after : string @ 'm0 -> int @ _ with local <= 'm0
    val use_inside : string @ 'm0 -> int @ _ with local <= 'm0
    |}];
  (* But a join with a global-only arm is global-only, in each. *)
  check
    {|
let after (n : int) x = let r = match n with | 0 -> x | _ -> (fun (s @ local) -> String.length s) in let _ = x "a" in r
let use_after (s @ local) = after 0 (fun (t : string) -> let g @ global = t in 1) s
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:3:19-24), then s is passed to the function (at t.stag:3:28-83), then s is used (at t.stag:3:82-83), then its type turned out to be string, which has pointers, then it is passed to a function (at t.stag:3:28-83), but must be at most global, because it is annotated global (at t.stag:3:65-71), then it is bound by an annotated let (at t.stag:3:65-71), then t is used (at t.stag:3:74-75), then t is passed to the function (at t.stag:3:36-81) |}]
;;

let%expect_test "joins by variance: contravariant and invariant parts" =
  (* An argument joins contravariantly, so its own argument covariantly: arms
     calling [k] with a local string and with a global one join to a function
     whose [k] must take local strings. A [ptr]'s contents are invariant: its
     arms must agree. *)
  check
    {|
let higher (n : int) = match n with | 0 -> (fun (k : string @ local -> int) -> k "a") | _ -> (fun (k : string -> int) -> k "a")
let same_cells (n : int) (p : ptr[string]) (q : ptr[string]) = match n with | 0 -> p | _ -> q
|};
  [%expect
    {|
    val higher : int @ _ -> ((string @ 'm0 -> int @ 'm1) @ _ -> int @ _) @ _ with local nonportable gc <= 'm0 and 'm1 <= global gc
    val same_cells : int @ 'm0 -> (ptr[string] @ 'm2 -> (ptr[string] @ 'm4 -> ptr[string] @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and gc <= 'm3 and gc <= 'm1
    |}];
  check
    {|
let cells (n : int) (p : ptr[string @@ external]) (q : ptr[string]) = match n with | 0 -> p | _ -> q
|};
  [%expect
    {|
    This expression has type ptr[string] but an expression was expected of type ptr[string @@ external]
    Their modes differ: The modes external and gc must be equal, because the types are unified
    |}]
;;

let%expect_test "an unresolved join is generalized as one type variable" =
  (* Nothing told [x] and [y] apart, so the scheme has one variable for them and
     the result, as in ML; an instance is still a join of its uses. *)
  check
    {|
let pick (n : int) x y = match n with | 0 -> x | _ -> y
let used = pick 0 (fun (s @ local) -> String.length s) (fun (s : string) -> let g @ global = s in String.length g)
let weak = pick 0 (Ptr.make []) (Ptr.make [])
|};
  [%expect
    {|
    val pick : int @ 'm0 -> ('a @ 'm2 -> ('a @ 'm4 -> 'a @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and gc <= 'm3 and gc <= 'm1
    val used : string @ '_m0 -> int @ '_m1 with '_m0 <= global
    val weak : ptr[list['_a]]
    |}]
;;

let%expect_test "every order of a list's elements, known or not" =
  same_in_every_order
    ~prefix:""
    ~arms:[ local_arm; global_arm; "x"; "y" ]
    ~make:(fun arms ->
      sprintf "let l x y = [ %s ]\n" (String.concat ~sep:"; " arms));
  [%expect
    {| val l : (string @ 'm1 -> int @ 'm2) @ 'm0 -> ((string @ 'm5 -> int @ 'm6) @ 'm4 -> list[string @ 'm8 -> int @ 'm9] @ 'm7) @ 'm3 with 'm0 <= 'm7 and release(locality('m0)) <= locality('m7) and 'm0 <= 'm3 and 'm2 <= 'm9 and 'm4 <= 'm7 and release(locality('m4)) <= locality('m7) and 'm6 <= 'm9 and 'm8 <= 'm1 and 'm8 <= 'm5 and 'm8 <= global and gc <= 'm7 and gc <= 'm3 |}]
;;

let%expect_test "a use's arms in either order" =
  let program ~use_first ~arms =
    let match_ =
      sprintf
        "match n with | 0 -> %s | _ -> %s"
        (List.nth_exn arms 0)
        (List.nth_exn arms 1)
    in
    if use_first
    then sprintf "let f (n : int) x = let _ = take_local x in %s\n" match_
    else
      sprintf
        "let f (n : int) x = let r = %s in let _ = take_local x in r\n"
        match_
  in
  List.iter [ false; true ] ~f:(fun use_first ->
    same_in_every_order
      ~prefix:"let take_local (f : string @ local -> int) = 1\n"
      ~arms:[ "x"; "(fun (s : string) -> 1)" ]
      ~make:(fun arms -> program ~use_first ~arms));
  [%expect
    {|
    val take_local : (string @ local -> int) @ _ -> int @ _
    val f : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) @ 'm2 -> (string @ 'm6 -> int @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and local nonportable gc <= 'm3 and 'm4 <= 'm7 and 'm4 <= global gc and 'm6 <= 'm3 and gc <= 'm1
    val take_local : (string @ local -> int) @ _ -> int @ _
    val f : int @ 'm0 -> ((string @ local -> int) @ 'm2 -> (string @ 'm4 -> int @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and 'm4 <= gc and nonportable gc <= 'm5 and gc <= 'm1
    |}]
;;

let%expect_test "a join of cyclic types, in any order" =
  (* The join's copy of [sl] is cyclic, its modes shared around the cycle. *)
  check
    {|
type sl = (string @ local -> int) * box[sl]
type sg = (string -> int) * box[sg]
let j1 (n : int) (a : sl) (b : sg) = match n with | 0 -> a | _ -> b
let j2 (n : int) (a : sl) (b : sg) = match n with | 0 -> b | _ -> a
let j3 (n : int) x (a : sl) (b : sg) = match n with | 0 -> x | 1 -> a | _ -> b
|};
  [%expect
    {|
    type sl = (string @ local -> int) * box[sl]
    type sg = (string -> int) * box[sg]
    val j1 : int @ 'm0 -> (((string @ local -> int) * box['a] as 'a) @ 'm2 -> (((string -> int) * box['b] as 'b) @ 'm4 -> (string @ 'm6 -> int @ 'm7) * box[((string @ 'm6 -> int @ 'm7) * box['c] as 'c)] @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and 'm6 <= global gc and nonportable gc <= 'm7 and gc <= 'm3 and gc <= 'm1
    val j2 : int @ 'm0 -> (((string @ local -> int) * box['a] as 'a) @ 'm2 -> (((string -> int) * box['b] as 'b) @ 'm4 -> (string @ 'm6 -> int @ 'm7) * box[((string @ 'm6 -> int @ 'm7) * box['c] as 'c)] @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and 'm6 <= global gc and nonportable gc <= 'm7 and gc <= 'm3 and gc <= 'm1
    val j3 : int @ 'm0 -> ((string @ 'm3 -> int @ 'm4) * box[((string @ 'm3 -> int @ 'm4) * box['a] as 'a)] @ 'm2 -> (((string @ local -> int) * box['b] as 'b) @ 'm6 -> (((string -> int) * box['c] as 'c) @ 'm8 -> (string @ 'm10 -> int @ 'm11) * box[((string @ 'm10 -> int @ 'm11) * box['d] as 'd)] @ 'm9) @ 'm7) @ 'm5) @ 'm1 with 'm0 <= 'm7 and 'm0 <= 'm5 and 'm0 <= 'm1 and 'm2 <= 'm9 and release(locality('m2)) <= locality('m9) and 'm2 <= 'm7 and 'm2 <= 'm5 and 'm4 <= 'm11 and 'm6 <= 'm9 and release(locality('m6)) <= locality('m9) and 'm6 <= 'm7 and 'm8 <= 'm9 and release(locality('m8)) <= locality('m9) and 'm10 <= 'm3 and 'm10 <= global gc and nonportable gc <= 'm11 and gc <= 'm7 and gc <= 'm5 and gc <= 'm1
    |}]
;;

let%expect_test "a join made recursive by its own arms is a recursive type" =
  check
    {|
let f (n : int) x y =
  let _ = match n with | 0 -> x | _ -> y in
  match n with | 0 -> x | _ -> [ y ]
|};
  [%expect
    {| val f : int @ 'm0 -> (list[(list['a] as 'a)] @ 'm2 -> (list[(list['b] as 'b)] @ 'm4 -> list[(list['c] as 'c)] @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and gc <= 'm5 and gc <= 'm3 and gc <= 'm1 |}]
;;

let%expect_test "a join reached through a code context's presence" =
  check {|
let f c = let id y = y in .<Int.add (.~c) (id 1)>.
|};
  [%expect
    {| val f : <int>{..'a} @ 'm0 -> <int>{..'b} @ 'm1 where ..'a <= {..'b} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1 |}]
;;

let%expect_test "many joins take time comparable to unification" =
  let params n = String.concat ~sep:" " (List.init n ~f:(sprintf "x%d")) in
  let arms n arm =
    String.concat (List.init n ~f:(fun i -> sprintf "| %d -> %s\n" i (arm i)))
  in
  let within_bound name source =
    let start = Time_ns.now () in
    let ok = Result.is_ok (checked source) in
    let elapsed = Time_ns.diff (Time_ns.now ()) start in
    print_s
      [%message
        name
          (ok : bool)
          ~fast:(Time_ns.Span.( < ) elapsed (Time_ns.Span.of_int_sec 60) : bool)]
  in
  within_bound
    "800 arms of an instance"
    (sprintf
       "let myid x = x\nlet f (n : int) x = match n with\n%s| _ -> x\n"
       (arms 800 (fun _ -> "myid x")));
  within_bound
    "a list of 100 parameters"
    (sprintf
       "let f %s = [ %s ]\n"
       (params 100)
       (String.concat ~sep:"; " (List.init 100 ~f:(sprintf "x%d"))));
  within_bound
    "100 arms of parameters"
    (sprintf
       "let f (n : int) %s = match n with\n%s| _ -> x0\n"
       (params 100)
       (arms 100 (sprintf "x%d")));
  [%expect
    {|
    ("800 arms of an instance" (ok true) (fast true))
    ("a list of 100 parameters" (ok true) (fast true))
    ("100 arms of parameters" (ok true) (fast true))
    |}]
;;

let%expect_test "joins of variables that stay unknown take linear time" =
  let source n =
    sprintf
      "let myid x = x\nlet f (n : int) x = match n with\n%s| _ -> x\n"
      (String.concat (List.init n ~f:(fun i -> sprintf "| %d -> myid x\n" i)))
  in
  let time n =
    List.init 3 ~f:(fun _ ->
      let start = Time_ns.now () in
      assert (Result.is_ok (checked (source n)));
      Time_ns.diff (Time_ns.now ()) start)
    |> List.min_elt ~compare:Time_ns.Span.compare
    |> Option.value_exn
  in
  let small = time 500 in
  let large = time 2000 in
  print_s
    [%message
      "4 times the arms"
        ~within_6_times_the_time:
          (Time_ns.Span.( < ) large (Time_ns.Span.scale small 6.) : bool)
        ~under_4s:(Time_ns.Span.( < ) large (Time_ns.Span.of_int_sec 4) : bool)];
  [%expect
    {| ("4 times the arms" (within_6_times_the_time true) (under_4s true)) |}]
;;

let%expect_test "a quote's use of a toplevel being checked is by variance" =
  (* [c]'s quote uses [ident] before [ident] is finished (its splice needs [c]),
     or after (no splice): either way, it is used as a finished toplevel is. *)
  let ident_first body =
    sprintf
      "let apply (f : string -> int) = 1\n\
       %s\n\
       let c (u : unit) = .< apply ident >."
      body
  in
  check
    (ident_first
       "let ident (x : string @ local) : int = .~(let _ = c () in .< 1 >.)");
  check (ident_first "let ident (x : string @ local) : int = 1");
  let joined ident =
    sprintf
      "%s\n\
       let c (n : int) = .< fun y -> match n with 0 -> ident y | _ -> (fun (s \
       @ local) -> String.length s) >."
      ident
  in
  check (joined "let ident x = .~(let _ = c 0 in .< x >.)");
  check (joined "let ident x = x");
  [%expect
    {|
    val apply : (string -> int) @ _ -> int @ _
    val ident : string @ 'm0 -> int @ _ with local <= 'm0
    val c : unit @ _ -> <int>{..'a} @ _
    val apply : (string -> int) @ _ -> int @ _
    val ident : string @ 'm0 -> int @ _ with local <= 'm0
    val c : unit @ _ -> <int>{..'a} @ _
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : int @ _ -> <(string @ 'm2 -> int @ 'm3) @ 'm1 -> (string @ 'm5 -> int @ 'm6) @ 'm4>{..'a} @ 'm0 with 'm1 <= 'm4 and release(locality('m1)) <= locality('m4) and 'm3 <= 'm6 and 'm5 <= 'm2 and gc <= 'm0
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : int @ _ -> <(string @ 'm2 -> int @ 'm3) @ 'm1 -> (string @ 'm5 -> int @ 'm6) @ 'm4>{..'a} @ 'm0 with 'm1 <= 'm4 and release(locality('m1)) <= locality('m4) and 'm3 <= 'm6 and 'm5 <= 'm2 and gc <= 'm0
    |}]
;;

let%expect_test "a mode parameter on a declared arrow" =
  (* An argument's is contravariant, a result's covariant. *)
  check
    (ext
     ^ {|
type takes['p : mode] = { f : string @ 'p -> int }
type gives['p : mode] = { g : unit -> string @ 'p }
let takes_ok (x : takes[external]) : takes[gc] = x
let gives_ok (x : gives[gc]) : gives[external] = x
let length = { f = fun (s @ external) -> String.length s }
let used : takes[external] = length
let n = length.f (ext "a")
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type takes['m0 : mode] = { f : string @ 'm0 -> int }
    type gives['m0 : mode] = { g : unit -> string @ 'm0 }
    val takes_ok : takes[external] @ 'm0 -> takes[gc] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val gives_ok : gives[gc] @ 'm0 -> gives[external] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val length : takes[_]
    val used : takes[external]
    val n : int
    |}];
  check
    {|
type takes['p : mode] = { f : string @ 'p -> int }
let takes_bad (x : takes[gc]) : takes[external] = x
|};
  [%expect
    {|
    This expression has type takes[gc] but an expression was expected of type takes[external]
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
    |}];
  check
    {|
type gives['p : mode] = { g : unit -> string @ 'p }
let gives_bad (x : gives[external]) : gives[gc] = x
|};
  [%expect
    {|
    This expression has type gives[external] but an expression was expected of type gives[gc]
    Their modes differ: This is external, but must be at most gc, because a type is used at one with another mode
    |}];
  (* What the field's function is given must fit its argument's mode. *)
  check
    (ext
     ^ {|
type takes['p : mode] = { f : string @ 'p -> int }
let call (x : takes[gc]) = x.f (ext "a")
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:5:31-40), then it is passed to a function (at t.stag:5:27-40), but must be at most gc, because the types are unified |}];
  (* A mode parameter has no locality: alone, the arrow's is the default,
     global. *)
  check
    {|
type takes['p : mode] = { f : string @ 'p -> int }
let call (x : takes[gc]) (s @ local) = x.f s
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:3:30-35), then s is passed to the function (at t.stag:3:39-44), then s is used (at t.stag:3:43-44), then its type turned out to be string, which has pointers, then it is passed to a function (at t.stag:3:39-44), but must be at most global, because it is a mode parameter on an arrow in a declaration, which is global (at t.stag:2:39-41) |}];
  round_trip
    {|
type cmp['k, 'p : mode] = { compare : 'k @ 'p -> 'k @ 'p -> int }
type alias['p : mode] = string @ 'p -> string @ 'p
let apply (c : cmp[string, external]) (f : alias[gc]) = c.compare (f "a") "b"
|}
    ~show:`Signature;
  [%expect
    {|
    type cmp['a, 'm0 : mode] = { compare : 'a @ 'm0 -> 'a @ 'm0 -> int }
    type alias['m0 : mode] = string @ 'm0 -> string @ 'm0
    val apply : cmp[string, external] @ 'm0 -> ((string -> string) @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    |}]
;;

let%expect_test "a locality with a mode parameter" =
  (* The locality is written; the parameter gives the stage and provenance. *)
  round_trip
    (ext
     ^ {|
type takes['p : mode] = { f : string @ local 'p -> int }
type gives['p : mode] = { g : unit -> string @ regional 'p }
let call (x : takes[gc]) (s @ local) = x.f s
let takes_ok (x : takes[external]) : takes[gc] = x
let gives_ok (x : gives[gc]) : gives[external] = x
|}
    )
    ~show:`Signature;
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type takes['m0 : mode] = { f : string @ local 'm0 -> int }
    type gives['m0 : mode] = { g : unit -> string @ regional 'm0 }
    val call : takes[gc] @ 'm0 -> (string @ 'm2 -> int @ _) @ 'm1 with 'm0 <= 'm1 and local <= 'm2 and 'm2 <= gc and gc <= 'm1
    val takes_ok : takes[external] @ 'm0 -> takes[gc] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val gives_ok : gives[gc] @ 'm0 -> gives[external] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}];
  check
    (ext
     ^ {|
type takes['p : mode] = { f : string @ local 'p -> int }
let ext_bad (x : takes[gc]) = x.f (ext "a")
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:5:34-43), then it is passed to a function (at t.stag:5:30-43), but must be at most gc, because the types are unified |}];
  check
    {|
type gives['p : mode] = { g : unit -> string @ regional 'p }
let keep (x : gives[gc]) : string @ global = x.g ()
|};
  [%expect
    {| This is regional, because its type is written regional (at t.stag:2:47-58), then the function returns it, by a tail call (at t.stag:3:45-51), but must be at most global, because it is annotated global (at t.stag:3:36-42) |}];
  (* In an annotation, too: the variable's locality is left free. *)
  round_trip
    {|
let f (g : string @ local 'm -> int) (s : string @ 'm) = (g s, s)
let h (s @ local 'm) : string @ global 'm = "x"
let k (x : string @ 'm) (g : string @ local 'm -> int @ global 'm) = g x
|}
    ~show:`Signature;
  [%expect
    {|
    val f : (string @ local 'm1 -> int) @ 'm0 -> (string @ 'm1 -> int * string @ 'm3) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm3 and release(locality('m1)) <= locality('m3) and gc <= 'm2
    val h : 'a @ 'm0 -> string @ 'm1 with local <= 'm0 and stage('m0) <= stage('m1) and provenance('m0) <= provenance('m1) and 'm1 <= 'm0 and 'm1 <= global
    val k : string @ 'm0 -> ((string @ local 'm0 -> int @ global 'm0) @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    |}];
  List.iter
    ~f:(fun source ->
      check source;
      print_endline "")
    [ "type t['p : mode] = { x : string @@ local 'p }"
    ; "type t['p : mode] = { f : string @ local 'q -> int }"
    ];
  [%expect
    {|
    This mode is invalid: a modality is a mode parameter or modes, not a locality with a mode parameter

    The mode variable 'q is not allowed in a declaration unless it is a parameter declared ['q : mode]: its arrows have fixed modes
    |}]
;;
