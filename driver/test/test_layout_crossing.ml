open! Import
open Mode_helpers

let modes source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

let located source =
  match Mode_helpers.located_errors source with
  | Ok () -> print_endline "ok"
  | Error errors ->
    List.iter errors ~f:(fun (loc, message) ->
      printf "%s: %s\n" (Loc.to_string loc) message)
;;

let polymorphic =
  {|
let ident x = x
let mk x = fun u -> x
let get (b : box['a]) = b.*
let put (x : 'a) (k : 'a @ global -> unit) = k x
let pair (p : 'a * 'a) = p
let g_ident x = ident x
let g_mk x = mk x
let g_get b = get b
let g_put x k = put x k
let g_pair p = pair p
|}
;;

(* A generalized type variable is taken to have pointers, yet every instance at
   [int] crosses at its own uses: the default loses nothing. *)
let uses ty =
  List.map [ ""; "g_" ] ~f:(fun g ->
    [%string
      {|
let %{g}use_ident (x : %{ty} @ local) = let r @ global = %{g}ident x in r
let %{g}use_mk (x : %{ty} @ local) = let f = %{g}mk x in let r @ global = f () in r
let %{g}use_closure (x : %{ty} @ local) = let f @ global = %{g}mk x in f
let %{g}use_get (x : %{ty} @ local) = let b = stack_ (Box.make x) in let r @ global = %{g}get b in r
let %{g}use_put (x : %{ty} @ local) = %{g}put x (fun (y @ global) -> ())
let %{g}use_pair (x : %{ty} @ local) = let p @ global = %{g}pair (x, x) in p
|}])
  |> String.concat
;;

let%expect_test "instances at a pointer-free type cross everywhere" =
  modes (polymorphic ^ uses "int");
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val mk : 'a @ 'm0 -> ('b @ _ -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm0 <= 'm1 and gc <= 'm1
    val get : box['a] @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val put : 'a @ 'm0 -> (('a -> unit) @ _ -> unit @ _) @ 'm1 with 'm0 <= 'm1 and 'm0 <= gc global and gc <= 'm1
    val pair : 'a * 'a @ 'm0 -> 'a * 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val g_ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val g_mk : 'a @ 'm0 -> ('b @ _ -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and stage('m0) <= stage('m1) and provenance('m0) <= provenance('m1) and meet(locality('m0), regional) <= locality('m1) and gc <= 'm1
    val g_get : box['a] @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val g_put : 'a @ 'm0 -> (('a -> unit) @ _ -> unit @ _) @ 'm1 with 'm0 <= 'm1 and 'm0 <= gc global and gc <= 'm1
    val g_pair : 'a * 'a @ 'm0 -> 'a * 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val use_ident : int @ 'm0 -> int @ _ with local <= 'm0
    val use_mk : int @ 'm0 -> int @ _ with local <= 'm0
    val use_closure : int @ 'm0 -> ('a @ _ -> int @ _) @ 'm1 with local <= 'm0 and gc <= 'm1
    val use_get : int @ 'm0 -> int @ _ with local <= 'm0
    val use_put : int @ 'm0 -> unit @ _ with local <= 'm0
    val use_pair : int @ 'm0 -> int * int @ _ with local <= 'm0
    val g_use_ident : int @ 'm0 -> int @ _ with local <= 'm0
    val g_use_mk : int @ 'm0 -> int @ _ with local <= 'm0
    val g_use_closure : int @ 'm0 -> ('a @ _ -> int @ _) @ 'm1 with local <= 'm0 and gc <= 'm1
    val g_use_get : int @ 'm0 -> int @ _ with local <= 'm0
    val g_use_put : int @ 'm0 -> unit @ _ with local <= 'm0
    val g_use_pair : int @ 'm0 -> int * int @ _ with local <= 'm0
    |}]
;;

let%expect_test "instances at types with pointers don't" =
  List.iter [ "string"; "ptr[int]" ] ~f:(fun ty ->
    String.split_lines (uses ty)
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.iter ~f:(fun use ->
      print_endline use;
      match checked (polymorphic ^ use) with
      | Ok _ -> print_endline "  accepted"
      | Error errors ->
        List.iter errors ~f:(fun error ->
          print_endline ("  " ^ String.prefix error 60))));
  [%expect
    {|
    let use_ident (x : string @ local) = let r @ global = ident x in r
      This is local, because it is annotated local (at t.stag:12:2
    let use_mk (x : string @ local) = let f = mk x in let r @ global = f () in r
      This is local, because it is annotated local (at t.stag:12:2
    let use_closure (x : string @ local) = let f @ global = mk x in f
      This is regional, because it is annotated local (at t.stag:1
    let use_get (x : string @ local) = let b = stack_ (Box.make x) in let r @ global = get b in r
      This is local, because it is allocated by [stack_] (at t.sta
    let use_put (x : string @ local) = put x (fun (y @ global) -> ())
      This is regional, because it is annotated local (at t.stag:1
    let use_pair (x : string @ local) = let p @ global = pair (x, x) in p
      This is local, because it is annotated local (at t.stag:12:2
    let g_use_ident (x : string @ local) = let r @ global = g_ident x in r
      This is local, because it is annotated local (at t.stag:12:3
    let g_use_mk (x : string @ local) = let f = g_mk x in let r @ global = f () in r
      This is local, because it is annotated local (at t.stag:12:2
    let g_use_closure (x : string @ local) = let f @ global = g_mk x in f
      This is regional, because it is annotated local (at t.stag:1
    let g_use_get (x : string @ local) = let b = stack_ (Box.make x) in let r @ global = g_get b in r
      This is local, because it is allocated by [stack_] (at t.sta
    let g_use_put (x : string @ local) = g_put x (fun (y @ global) -> ())
      This is regional, because it is annotated local (at t.stag:1
    let g_use_pair (x : string @ local) = let p @ global = g_pair (x, x) in p
      This is local, because it is annotated local (at t.stag:12:2
    let use_ident (x : ptr[int] @ local) = let r @ global = ident x in r
      This is local, because it is annotated local (at t.stag:12:3
    let use_mk (x : ptr[int] @ local) = let f = mk x in let r @ global = f () in r
      This is local, because it is annotated local (at t.stag:12:2
    let use_closure (x : ptr[int] @ local) = let f @ global = mk x in f
      This is regional, because it is annotated local (at t.stag:1
    let use_get (x : ptr[int] @ local) = let b = stack_ (Box.make x) in let r @ global = get b in r
      This is local, because it is allocated by [stack_] (at t.sta
    let use_put (x : ptr[int] @ local) = put x (fun (y @ global) -> ())
      This is regional, because it is annotated local (at t.stag:1
    let use_pair (x : ptr[int] @ local) = let p @ global = pair (x, x) in p
      This is local, because it is annotated local (at t.stag:12:2
    let g_use_ident (x : ptr[int] @ local) = let r @ global = g_ident x in r
      This is local, because it is annotated local (at t.stag:12:3
    let g_use_mk (x : ptr[int] @ local) = let f = g_mk x in let r @ global = f () in r
      This is local, because it is annotated local (at t.stag:12:2
    let g_use_closure (x : ptr[int] @ local) = let f @ global = g_mk x in f
      This is regional, because it is annotated local (at t.stag:1
    let g_use_get (x : ptr[int] @ local) = let b = stack_ (Box.make x) in let r @ global = g_get b in r
      This is local, because it is allocated by [stack_] (at t.sta
    let g_use_put (x : ptr[int] @ local) = g_put x (fun (y @ global) -> ())
      This is regional, because it is annotated local (at t.stag:1
    let g_use_pair (x : ptr[int] @ local) = let p @ global = g_pair (x, x) in p
      This is local, because it is annotated local (at t.stag:12:3
    |}]
;;

let%expect_test "crossing doesn't depend on whether the type is known first" =
  List.iter [ "int"; "string" ] ~f:(fun ty ->
    modes
      [%string
        {|
let known_first (x @ local) = let t : %{ty} = x in let r @ global = x in r
let known_after (x @ local) = let r @ global = x in let t : %{ty} = x in r
let known_in_let (x @ local) = let f u = let r @ global = x in r in let t : %{ty} = x in f ()
|}]);
  [%expect
    {|
    val known_first : int @ 'm0 -> int @ _ with local <= 'm0
    val known_after : int @ 'm0 -> int @ _ with local <= 'm0
    val known_in_let : int @ 'm0 -> int @ _ with local <= 'm0
    This is regional, because it is annotated local (at t.stag:2:21-26), then x is passed to the function (at t.stag:2:30-75), then x is used (at t.stag:2:69-70), then it is bound by an annotated let (at t.stag:2:60-66), but must be at most global, because it is annotated global (at t.stag:2:60-66)
    This is regional, because it is annotated local (at t.stag:3:21-26), then x is passed to the function (at t.stag:3:30-75), then x is used (at t.stag:3:47-48), then its type turned out to be string, which has pointers, but must be at most global, because it is annotated global (at t.stag:3:38-44), then it is bound by an annotated let (at t.stag:3:38-44)
    This is regional, because it is annotated local (at t.stag:4:22-27), then x is passed to the function (at t.stag:4:31-94), then x is used (at t.stag:4:58-59), then its type turned out to be string, which has pointers, but must be at most global, because it is annotated global (at t.stag:4:49-55), then it is bound by an annotated let (at t.stag:4:49-55)
    |}]
;;

let%expect_test "a type variable annotated pointer-free" =
  modes
    {|
let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
let local_int (x : int @ local) = keep x
|};
  round_trip
    {|
let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
|}
    ~show:`Copies;
  modes
    {|
let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
let s = keep "s"
|};
  (* Without it, the argument must itself be global. *)
  modes {|
let keep : 'a @ 'm -> 'a @ global = fun x -> x
|};
  [%expect
    {|
    val keep : 'a @ _ -> 'a with 'a : pointer_free
    val local_int : int @ 'm0 -> int @ _ with local <= 'm0
    let keep_copy : 'a @ _ -> 'a with 'a : pointer_free = keep
    This type has pointers, because its type turned out to be string, which has pointers (at t.stag:3:13-16), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:2:11-56)
    val keep : 'a @ 'm0 -> 'a with 'm0 <= gc global
    |}]
;;

(* Whether a quote may embed a local is decided once its layout is known,
   whatever the order things are checked in. *)
let%expect_test "embedding doesn't depend on whether the type is known first" =
  modes
    {|
let t1 f = let r = f 1 in let k = Int.add r 0 in .<r>.
let t2 f = let r = f 1 in let q = .<r>. in let k = Int.add r 0 in q
let t2_in_let f = let r = f 1 in let g u = .<r>. in let k = Int.add r 0 in g ()
|};
  [%expect
    {|
    val t1 : (int @ _ -> int @ _) @ _ -> <int>{..'a} @ 'm0 with gc <= 'm0
    val t2 : (int @ _ -> int @ _) @ _ -> <int>{..'a} @ 'm0 with gc <= 'm0
    val t2_in_let : (int @ _ -> int @ _) @ _ -> <int>{..'a} @ 'm0 with gc <= 'm0
    |}];
  (* Of a type with pointers, it must be portable however the function is used:
     a parameter's results may not be. *)
  List.iter
    [ {|let t3 f = let r = f 1 in .<r>.|}
    ; {|let t3 f = let r = f 1 in let q = .<r>. in let k = String.length r in q|}
    ; {|let t3 f = let r = f 1 in let k = String.length r in .<r>.|}
    ]
    ~f:modes;
  [%expect
    {|
    The quote can't embed r, as it may not be portable. Bind it as (r @ portable) to embed it, or write [forget r in ...] to make r a free variable of the quote
    The quote can't embed r, as it may not be portable. Bind it as (r @ portable) to embed it, or write [forget r in ...] to make r a free variable of the quote
    The quote can't embed r, as it may not be portable. Bind it as (r @ portable) to embed it, or write [forget r in ...] to make r a free variable of the quote
    |}]
;;

let%expect_test "embedding decides a layout known in the code a splice runs" =
  modes
    {|
let d = .~(let k = id (fun p -> let y = (p, 1) in .< y >.) in print_endline "ran d"; k 0)
let e = .~(let k = id (fun p -> let y = (p, 1) in .< y >.) in print_endline "ran e"; k "s")
|};
  [%expect
    {|
    The quote can't embed y, as it may not be portable. Bind it as (y @ portable) to embed it, or write [forget y in ...] to make y a free variable of the quote
    The quote can't embed y, as it may not be portable. Bind it as (y @ portable) to embed it, or write [forget y in ...] to make y a free variable of the quote
    |}]
;;

(* A failed embedding requires nothing more, so uses of what it is in find no
   other errors. *)
let%expect_test "a failed embedding adds no constraint" =
  modes {|
let t f = let r = f 1 in .<r>.
let u = t (Int.add 1)
|};
  [%expect
    {| The quote can't embed r, as it may not be portable. Bind it as (r @ portable) to embed it, or write [forget r in ...] to make r a free variable of the quote |}]
;;

(* A type no type reaches is decided as its scope ends: as having pointers. *)
let%expect_test "an unreachable variable's layout is decided" =
  modes
    {|
let loop u = loop u
let f (u : unit) = let x @ local = loop () in let r @ global = x in 0
let v = let x @ local = loop () in let r @ global = x in Ptr.make 0
let g (u : unit) = let h w = (let x @ local = loop () in let r @ global = x in 0) in h ()
|};
  [%expect
    {|
    This is local, because it is annotated local (at t.stag:3:27-32), then x is used (at t.stag:3:63-64), but must be at most global, because it is annotated global (at t.stag:3:54-60), then it is bound by an annotated let (at t.stag:3:54-60)
    This is local, because it is annotated local (at t.stag:4:16-21), then x is used (at t.stag:4:52-53), but must be at most global, because it is annotated global (at t.stag:4:43-49), then it is bound by an annotated let (at t.stag:4:43-49)
    This is local, because it is annotated local (at t.stag:5:38-43), then x is used (at t.stag:5:74-75), but must be at most global, because it is annotated global (at t.stag:5:65-71), then it is bound by an annotated let (at t.stag:5:65-71)
    |}]
;;

(* A failed crossing is reported once: the variable doesn't decide its layout
   again as it is generalized. *)
let%expect_test "a crossing that fails as a type is found is reported once" =
  modes
    {|
let known_after (x @ local) = let r @ global = x in let t : string = x in r
let in_record (x @ local) = let r @ global = x in let t : {`a : string} = {`a = x} in r
|};
  [%expect
    {|
    This is regional, because it is annotated local (at t.stag:2:21-26), then x is passed to the function (at t.stag:2:30-75), then x is used (at t.stag:2:47-48), then its type turned out to be string, which has pointers, but must be at most global, because it is annotated global (at t.stag:2:38-44), then it is bound by an annotated let (at t.stag:2:38-44)
    This is regional, because it is annotated local (at t.stag:3:19-24), then x is passed to the function (at t.stag:3:28-87), then x is used (at t.stag:3:45-46), then its type turned out to be string, which has pointers, but must be at most global, because it is annotated global (at t.stag:3:36-42), then it is bound by an annotated let (at t.stag:3:36-42)
    |}]
;;

(* An entry of a poly record whose presence isn't known has pointers only once
   it is present. *)
let%expect_test "an unknown presence guards" =
  modes
    {|
let loop u = loop u
let present c = let r = loop () in let q = .<let y = "s" in .~(c : <int>{..'r})>. in let s @ global = (let l @ local = (r : {..'r}) in l) in let k : {`y : string} = r in 0
let present_first c = let r = loop () in let q = .<let y = "s" in .~(c : <int>{..'r})>. in let k : {`y : string} = r in let s @ global = (let l @ local = (r : {..'r}) in l) in s
|};
  modes
    {|
let loop u = loop u
let pointer_free c = let r = loop () in let q = .<let y = 1 in .~(c : <int>{..'r})>. in let s @ global = (let l @ local = (r : {..'r}) in l) in let k : {`y : int} = r in 0
let absent c = let r = loop () in let q = .<let y = "s" in .~(c : <int>{..'r})>. in let s @ global = (let l @ local = (r : {..'r}) in l) in let k : {} = r in 0
|};
  [%expect
    {|
    This is local, because it is annotated local (at t.stag:3:111-116), then l is used (at t.stag:3:135-136), then a field of the row turned out to be present, but must be at most global, because it is annotated global (at t.stag:3:93-99), then it is bound by an annotated let (at t.stag:3:93-99)
    This is local, because it is annotated local (at t.stag:4:146-151), then l is used (at t.stag:4:170-171), then it is bound by an annotated let (at t.stag:4:128-134), but must be at most global, because it is annotated global (at t.stag:4:128-134)
    val loop : 'a @ 'm0 -> 'b @ _ with portable <= 'm0
    val pointer_free : <int>{y : immut int} @ _ -> int @ _
    val absent : <int>{} @ _ -> int @ _
    |}]
;;

(* Each generalized let passes what it doesn't decide to the one around it. *)
let%expect_test "nested lets decide their layouts where their types are known" =
  let nested ty =
    let lets =
      List.init 20 ~f:(fun i ->
        [%string "let g%{i#Int} u = (let r @ global = x in 0) in"])
      |> String.concat ~sep:"\n  "
    in
    [%string {|
let f (x @ local) =
  %{lets}
  let t : %{ty} = x in 0
|}]
  in
  modes (nested "int");
  modes (nested "string");
  [%expect
    {|
    val f : int @ 'm0 -> int @ _ with local <= 'm0
    This is regional, because it is annotated local (at t.stag:2:11-16), then x is passed to the function (at t.stag:3:2-23:25), then x is used (at t.stag:3:31-32), then its type turned out to be string, which has pointers, but must be at most global, because it is annotated global (at t.stag:3:22-28), then it is bound by an annotated let (at t.stag:3:22-28)
    |}]
;;

let%expect_test "a type variable annotated pointer-free, in nested lets" =
  modes
    {|
let f (x : 'a @ local) =
  let g u = (let keep : 'b @ 'm -> 'b @ global with 'b : pointer_free = fun y -> y in keep x) in
  let h (z : 'a @ local) = let r @ global = z in 0 in
  0
let ok = f 1
|};
  modes
    {|
let f (x : 'a @ local) =
  let g u = (let keep : 'b @ 'm -> 'b @ global with 'b : pointer_free = fun y -> y in keep x) in
  0
let not_ok = f "s"
|};
  [%expect
    {|
    val f : 'a @ 'm0 -> int @ _ with 'a : pointer_free and local <= 'm0
    val ok : int
    This type has pointers, because its type turned out to be string, which has pointers (at t.stag:5:15-18), but must be pointer-free, because it is annotated 'b : pointer_free (at t.stag:3:24-69)
    |}]
;;

(* A constraint guarded by a weak variable's layout prints [unless] it is
   pointer-free; the round trip instantiates the variable as [int] (never added)
   and [string] (added). *)
let%expect_test "a weak layout's guarded constraints print" =
  let round_trip source = round_trip source ~show:`Signature in
  round_trip
    {|
let loop u = loop u
let p = Ptr.make (loop ())
let g (x @ local) = p.* <- x
|};
  [%expect
    {|
    val loop : 'a @ 'm0 -> 'b @ _ with portable <= 'm0
    val p : ptr['_a]
    val g : '_a @ 'm0 -> unit @ _ with local <= 'm0 and 'm0 <= global unless pointer_free('_a)
    |}];
  round_trip
    {|
let loop u = loop u
let r = loop ()
let f y (x @ local) = let q = Ptr.make r in let k = fun (z : 'b) -> q.* <- x in k y
let f2 = f 1
|};
  [%expect
    {|
    val loop : 'a @ 'm0 -> 'b @ _ with portable <= 'm0
    val r : '_a
    val f : 'a @ 'm0 -> ('_b @ 'm2 -> unit @ _) @ 'm1 with 'm0 <= 'm1 and local <= 'm2 and gc <= 'm1 and 'm2 <= global unless pointer_free('_b)
    val f2 : '_a @ '_m0 -> unit @ '_m1 with local <= '_m0 and '_m0 <= global unless pointer_free('_a)
    |}];
  (* Once the layout has pointers, the constraint is added. *)
  round_trip
    {|
let loop u = loop u
let p = Ptr.make (loop ())
let g (x @ 'm) = p.* <- x
let s = p.* <- "s"
|};
  [%expect
    {|
    val loop : 'a @ 'm0 -> 'b @ _ with portable <= 'm0
    val p : ptr[string]
    val g : string @ 'm0 -> unit @ _ with 'm0 <= gc global
    val s : unit
    |}];
  (* A scheme keeps its guarded constraints, its instances adding them once the
     guard has pointers. *)
  round_trip
    {|
let loop u = loop u
let r = loop ()
let f y x = let q = Ptr.make r in let k = fun (z : 'b) -> q.* <- x in k y
let s = (r : string)
let f3 = f 1
|};
  [%expect
    {|
    val loop : 'a @ 'm0 -> 'b @ _ with portable <= 'm0
    val r : string
    val f : 'a @ 'm0 -> (string @ 'm2 -> unit @ _) @ 'm1 with 'm0 <= 'm1 and 'm2 <= global and gc <= 'm1
    val s : string
    val f3 : string @ '_m0 -> unit @ '_m1 with '_m0 <= global
    |}]
;;

let keep =
  {|let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
let loop u = loop u
|}
;;

(* Where an instance of something annotated pointer-free is used, whenever the
   binding giving it pointers comes. *)
let%expect_test "a pointer-free instance fails where it is used" =
  located (keep ^ {|let s = keep "s"|});
  located (keep ^ {|let r = loop ()
let s = keep r
let t = (r : string)|});
  located
    (keep
     ^ {|let f (u : unit) = let r = loop () in let s = keep r in let t = (r : string) in 0|}
    );
  located (keep ^ {|let h y = keep y
let z = h "s"|});
  located (keep ^ {|let f = keep
let z = f "s"|});
  [%expect
    {|
    t.stag:3:8-12: This type has pointers, because its type turned out to be string, which has pointers (at t.stag:3:13-16), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:1:11-56)
    t.stag:4:8-12: This type has pointers, because its type turned out to be string, which has pointers (at t.stag:5:8-20), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:1:11-56)
    t.stag:3:46-50: This type has pointers, because its type turned out to be string, which has pointers (at t.stag:3:64-76), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:1:11-56)
    t.stag:4:8-9: This type has pointers, because its type turned out to be string, which has pointers (at t.stag:4:10-13), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:1:11-56)
    t.stag:4:8-9: This type has pointers, because its type turned out to be string, which has pointers (at t.stag:4:10-13), but must be pointer-free, because it is annotated 'a : pointer_free (at t.stag:1:11-56)
    |}]
;;

(* The annotation of a let is its binding's mode, whatever is bound. *)
let%expect_test "a value exceeding a let's annotation is one error" =
  located
    {|let f (u : unit) = let s @ global = (let l @ local = "x" in l) in s|};
  located
    {|let loop u = loop u
let f c = let r = loop () in let s @ global = (let l @ local = (r : {..'r}) in l) in let k : {`y : string} = r in s|};
  [%expect
    {|
    t.stag:1:27-33: This is local, because it is annotated local (at t.stag:1:45-50), then l is used (at t.stag:1:60-61), then it is bound by an annotated let (at t.stag:1:27-33), but must be at most global, because it is annotated global (at t.stag:1:27-33)
    t.stag:2:109-110: This is local, because it is annotated local (at t.stag:2:55-60), then l is used (at t.stag:2:79-80), then a field of the row turned out to be present, but must be at most global, because it is annotated global (at t.stag:2:37-43), then it is bound by an annotated let (at t.stag:2:37-43)
    |}]
;;

(* A field has pointers only once it is present and its entry has: neither being
   known first decides it. *)
let%expect_test "a row entry crosses whichever of its presence and type is \
                 known first"
  =
  let program ~entry_first ~absent =
    let entry = {|let z : string = v in |} in
    let presence =
      if absent
      then {|let k : {} = r in |}
      else {|let k : {`a : string} = r in |}
    in
    [%string
      {|let loop u = loop u
let f c (v @ portable) = let r = loop () in let q = .<let y = v in .~(c : <int>{..'r})>. in let s @ global = (let l @ local = (r : {..'r}) in l) in %{if entry_first then entry ^ presence else presence ^ entry}0|}]
  in
  List.iter [ true; false ] ~f:(fun absent ->
    List.iter [ true; false ] ~f:(fun entry_first ->
      printf "absent=%b entry_first=%b: " absent entry_first;
      located (program ~entry_first ~absent)));
  [%expect
    {|
    absent=true entry_first=true: ok
    absent=true entry_first=false: ok
    absent=false entry_first=true: t.stag:2:194-195: This is local, because it is annotated local (at t.stag:2:118-123), then l is used (at t.stag:2:142-143), then a field of the row turned out to be present, but must be at most global, because it is annotated global (at t.stag:2:100-106), then it is bound by an annotated let (at t.stag:2:100-106)
    absent=false entry_first=false: t.stag:2:172-173: This is local, because it is annotated local (at t.stag:2:118-123), then l is used (at t.stag:2:142-143), then a field of the row turned out to be present, but must be at most global, because it is annotated global (at t.stag:2:100-106), then it is bound by an annotated let (at t.stag:2:100-106)
    |}]
;;

let%expect_test "a field turning out present gives its reason" =
  located
    {|let f : {..'r} @ 'm -> {..'r} @ global with ..'r : pointer_free = fun x -> x
let loop u = loop u
let r = loop ()
let a = f r
let b = (r : {`a : string})|};
  [%expect
    {| t.stag:4:8-9: This type has pointers, because a field of the row turned out to be present (at t.stag:5:8-27), but must be pointer-free, because it is annotated ..'r : pointer_free (at t.stag:1:8-63) |}]
;;

(* A type a splice computes is known once the splice has run, whether it is used
   before or after its declaration. *)
let%expect_test "a type computed by a splice crosses as what it computes" =
  let kind =
    {|let kind = Stag.Type_decl.Kind.Alias (Stag.Type_expr.con "int" Nil)
|}
  in
  let decl = "type t = .~kind\n" in
  let local = "let f (x : t @ local) = let r @ global = x in r\n" in
  let weak =
    {|let loop u = loop u
let cell = Ptr.make (loop ())
let g (x @ local) = cell.* <- x
let set = cell.* <- (1 : t)
|}
  in
  List.iter
    [ "used after", kind ^ decl ^ local ^ weak
    ; "used before", kind ^ local ^ weak ^ decl
    ]
    ~f:(fun (name, source) ->
      printf "%s: " name;
      located source);
  [%expect {|
    used after: ok
    used before: ok
    |}]
;;

(* Checking [b] runs the include, to look for [w], and its code uses [c], still
   being checked: abandoned, it is checked again once [c] is ([b] doesn't see
   [w]). What the abandoned check waited on is dropped with it: storing a string
   into [cell] is one error, of the check that finished. *)
let%expect_test "a splice's abandoned check leaves no layout checks behind" =
  let source ~store =
    [%string
      {|let loop u = loop u
let cell = Ptr.make (loop ())
let value name = Cons (Box.make ((Stag.Toplevel.let_ name Nil (Stag.Expr.int 1), Nil)))
let c = b
include .~(let g (x @ local) = cell.* <- x in let _ = c in value "w")
let b = w
%{store}|}]
  in
  List.iter
    [ "let set = cell.* <- \"s\"\n"; "let set = cell.* <- 1\n"; "" ]
    ~f:(fun store ->
      print_endline (String.strip store);
      located (source ~store));
  [%expect
    {|
    let set = cell.* <- "s"
    t.stag:6:8-9: Unbound value w
    t.stag:7:20-23: This is regional, because it is annotated local (at t.stag:5:22-27), then x is passed to the function (at t.stag:5:10-69), then x is used (at t.stag:5:41-42), then its type turned out to be string, which has pointers, but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:5:41-42)
    let set = cell.* <- 1
    t.stag:6:8-9: Unbound value w

    t.stag:6:8-9: Unbound value w
    |}]
;;

(* While a splice computes a type's definition, code its expression uses may use
   the type: whether it has pointers is then not known, until the definition is.
   [g] is checked before or after that, and its constraint is the same. *)
let%expect_test "a type used while its splice computes it" =
  let prelude ~def =
    [%string
      {|let loop u = loop u
let pointer_free = Stag.Type_decl.Kind.Variant (Cons (Box.make (("A", Nil), Nil)))
let with_pointers = Stag.Type_decl.Kind.Variant (Cons (Box.make (("A", Cons (Box.make (Stag.Type_expr.con "string" Nil, Nil))), Nil)))
let of_param = Stag.Type_decl.Kind.Variant (Cons (Box.make (("A", Cons (Box.make (Stag.Type_expr.Var "a", Nil))), Nil)))
let cell = Ptr.make (loop ())
%{def}|}]
  in
  let g = "let g (x @ local) = cell.* <- x\n" in
  let stores ~kind ~ty =
    [%string
      "type t['a] = .~(let _ = h in %{kind})\n\
       let h (u : unit) = cell.* <- (loop () : %{ty})\n"]
  in
  let local ~kind =
    [%string
      "let f (x : t[int] @ local) = let r @ global = x in r\n\
       type t['a] = .~(let _ = f in %{kind})\n"]
  in
  List.iter
    [ "pointer-free", stores ~kind:"pointer_free" ~ty:"t[string]"
    ; "with pointers", stores ~kind:"with_pointers" ~ty:"t[int]"
    ; "of a pointer-free argument", stores ~kind:"of_param" ~ty:"t[int]"
    ; "of an argument with pointers", stores ~kind:"of_param" ~ty:"t[string]"
    ]
    ~f:(fun (name, def) ->
      List.iter [ "before"; "after" ] ~f:(fun order ->
        printf "%s, g %s: " name order;
        located
          (match order with
           | "before" -> prelude ~def:(g ^ def)
           | _ -> prelude ~def:(def ^ g))));
  List.iter [ "pointer_free"; "with_pointers"; "of_param" ] ~f:(fun kind ->
    printf "crossing in its splice, %s: " kind;
    located (prelude ~def:(local ~kind)));
  [%expect
    {|
    pointer-free, g before: ok
    pointer-free, g after: ok
    with pointers, g before: t.stag:7:5-6: This is regional, because it is annotated local (at t.stag:6:11-16), then x is passed to the function (at t.stag:6:20-31), then x is used (at t.stag:6:30-31), then its type turned out to be t, whose definition has pointers (at t.stag:7:5-6), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:6:30-31), then its type turned out to be t, whose definition has pointers (at t.stag:7:5-6)
    with pointers, g after: t.stag:8:20-31: This is regional, because it is annotated local (at t.stag:8:11-16), then x is passed to the function (at t.stag:8:20-31), then x is used (at t.stag:8:30-31), then its type turned out to be t[int], which has pointers, but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:8:30-31)
    of a pointer-free argument, g before: ok
    of a pointer-free argument, g after: ok
    of an argument with pointers, g before: t.stag:7:5-6: This is regional, because it is annotated local (at t.stag:6:11-16), then x is passed to the function (at t.stag:6:20-31), then x is used (at t.stag:6:30-31), then its type turned out to be t, which has pointers when an argument that has them is (at t.stag:7:5-6), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:6:30-31), then its type turned out to be t, which has pointers when an argument that has them is (at t.stag:7:5-6)
    of an argument with pointers, g after: t.stag:8:20-31: This is regional, because it is annotated local (at t.stag:8:11-16), then x is passed to the function (at t.stag:8:20-31), then x is used (at t.stag:8:30-31), then its type turned out to be t[string], which has pointers, but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:8:30-31)
    crossing in its splice, pointer_free: ok
    crossing in its splice, with_pointers: t.stag:7:5-6: This is regional, because it is annotated local (at t.stag:6:20-25), then x is passed to the function (at t.stag:6:29-52), then x is used (at t.stag:6:46-47), then its type turned out to be t, whose definition has pointers (at t.stag:7:5-6), but must be at most global, because it is annotated global (at t.stag:6:37-43), then it is bound by an annotated let (at t.stag:6:37-43)
    crossing in its splice, of_param: ok
    |}]
;;
