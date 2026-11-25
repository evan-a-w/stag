open! Import
open Mode_helpers

let read path = In_channel.read_all path

(* The library's sources as modules of the program, so their allocations are
   recorded too. *)
let library =
  sprintf
    "module Map = struct\n%s\nend\nmodule Lru = struct\n%s\nend\n"
    (read "../../core/map.stag")
    (read "../../core/lru.stag")
;;

let show source =
  match signature (library ^ source) with
  | Ok text -> print_string text
  | Error errors -> List.iter errors ~f:print_endline
;;

let%expect_test "signatures" =
  show "";
  [%expect
    {|
    module Map : sig
      type t['a, 'b, 'm0 : mode] = Empty | Node of box[node['a, 'b, 'm0]]
      type node['a, 'b, 'm0 : mode] = { left : t['a, 'b, 'm0]; key : 'a @@ gc; value : 'b @@ 'm0; right : t['a, 'b, 'm0]; height : int }
      val empty : t['a, 'b, _]
      val is_empty : t['a, 'b, _] @ _ -> bool @ _
      val height : t['a, 'b, _] @ _ -> int @ _
      module Internal : sig
        val max : int @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
        val create : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> ('b @ 'm5 -> (t['a, 'b, 'm1] @ 'm7 -> t['a, 'b, 'm9] @ 'm8) @ 'm6) @ 'm4) @ 'm2 with 'm0 <= 'm8 and release(locality('m0)) <= locality('m8) and 'm0 <= 'm6 and 'm0 <= 'm4 and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm9 and 'm3 <= 'm8 and release(locality('m3)) <= locality('m8) and 'm3 <= 'm6 and 'm3 <= 'm4 and 'm3 <= gc and 'm5 <= 'm1 and 'm5 <= 'm8 and release(locality('m5)) <= locality('m8) and 'm5 <= 'm6 and 'm7 <= 'm8 and release(locality('m7)) <= locality('m8) and gc <= 'm8 and gc <= 'm6 and gc <= 'm4 and gc <= 'm2
        val bal : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> ('b @ 'm5 -> (t['a, 'b, 'm1] @ 'm7 -> t['a, 'b, 'm9] @ 'm8) @ 'm6) @ 'm4) @ 'm2 with 'm0 <= 'm8 and release(locality('m0)) <= locality('m8) and 'm0 <= 'm6 and 'm0 <= 'm4 and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm9 and 'm1 <= 'm8 and 'm3 <= 'm8 and release(locality('m3)) <= locality('m8) and 'm3 <= 'm6 and 'm3 <= 'm4 and 'm3 <= gc and 'm5 <= 'm1 and release(locality('m5)) <= locality('m8) and 'm5 <= 'm6 and 'm7 <= 'm8 and release(locality('m7)) <= locality('m8) and gc <= 'm8 and gc <= 'm6 and gc <= 'm4 and gc <= 'm2
        val onto : t['a, 'b, 'm1] @ 'm0 -> (list['a * 'b] @ 'm3 -> list['a * 'b] @ 'm3) @ 'm2 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and stage('m0) <= stage('m3) and release(locality('m0)) <= locality('m3) and 'm0 <= 'm2 and 'm1 <= 'm3 and gc <= 'm3 and release(locality('m3)) <= locality('m3) and gc <= 'm2
        val remove_min : t['a, 'b, 'm1] @ 'm0 -> t['a, 'b, 'm1] @ 'm2 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm2 and gc <= 'm2 and release(locality('m2)) <= locality('m2)
        val min_key : t['a, 'b, _] @ 'm0 -> option['a] @ 'm1 with portable <= 'm0 and stage('m0) <= stage('m1) and meet(provenance('m0), gc) <= provenance('m1) and release(locality('m0)) <= locality('m1)
        val min_value : t['a, 'b, 'm1] @ 'm0 -> option['b] @ 'm2 with portable <= 'm0 and stage('m0) <= stage('m2) and release(locality('m0)) <= locality('m2) and 'm1 <= 'm2
        val merge : t['a, 'b, 'm1] @ 'm0 -> (t['a, 'b, 'm1] @ 'm3 -> t['a, 'b, 'm1] @ 'm4) @ 'm2 with 'm0 <= 'm4 and release(locality('m0)) <= locality('m4) and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm4 and stage('m3) <= stage('m1) and 'm3 <= 'm4 and release(locality('m3)) <= locality('m4) and gc <= 'm4 and gc <= 'm2
      end
      val singleton : 'a @ 'm0 -> ('b @ 'm2 -> t['a, 'b, 'm4] @ 'm3) @ 'm1 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and 'm0 <= gc and 'm2 <= 'm4 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and portable <= 'm4 and gc <= 'm3 and gc <= 'm1
      val add : ('a @ 'm1 -> ('a @ 'm2 -> int @ _) @ _) @ 'm0 -> (t['a, 'b, 'm5] @ 'm4 -> ('a @ 'm7 -> ('b @ 'm9 -> t['a, 'b, 'm5] @ 'm10) @ 'm8) @ 'm6) @ 'm3 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm8 and 'm0 <= 'm6 and 'm0 <= 'm3 and portable <= 'm4 and stage('m4) <= stage('m2) and meet(provenance('m4), gc) <= provenance('m2) and meet(locality('m4), regional) <= locality('m2) and release(locality('m4)) <= locality('m4) and 'm4 <= 'm10 and 'm4 <= 'm8 and 'm4 <= 'm6 and 'm5 <= 'm10 and portable <= 'm7 and stage('m7) <= stage('m1) and provenance('m7) <= provenance('m1) and meet(locality('m7), regional) <= locality('m1) and release(locality('m7)) <= locality('m7) and 'm7 <= 'm10 and 'm7 <= 'm8 and 'm7 <= gc and portable <= 'm9 and 'm9 <= 'm5 and release(locality('m9)) <= locality('m9) and 'm9 <= 'm10 and gc <= 'm10 and release(locality('m10)) <= locality('m10) and gc <= 'm8 and gc <= 'm6 and gc <= 'm3
      val find : ('a @ 'm1 -> ('b @ 'm2 -> int @ _) @ _) @ 'm0 -> (t['b, 'c, 'm5] @ 'm4 -> ('a @ 'm7 -> option['c] @ 'm8) @ 'm6) @ 'm3 with portable <= 'm0 and 'm0 <= 'm6 and 'm0 <= 'm3 and portable <= 'm4 and stage('m4) <= stage('m2) and meet(provenance('m4), gc) <= provenance('m2) and meet(locality('m4), regional) <= locality('m2) and stage('m4) <= stage('m8) and release(locality('m4)) <= locality('m8) and 'm4 <= 'm6 and 'm5 <= 'm8 and portable <= 'm7 and stage('m7) <= stage('m1) and provenance('m7) <= provenance('m1) and meet(locality('m7), regional) <= locality('m1) and gc <= 'm6 and gc <= 'm3
      val mem : ('a @ 'm1 -> ('b @ 'm2 -> int @ _) @ _) @ 'm0 -> (t['b, 'c, _] @ 'm4 -> ('a @ 'm6 -> bool @ _) @ 'm5) @ 'm3 with 'm0 <= 'm5 and 'm0 <= 'm3 and portable <= 'm1 and portable <= 'm2 and stage('m4) <= stage('m2) and meet(provenance('m4), gc) <= provenance('m2) and meet(locality('m4), regional) <= locality('m2) and 'm4 <= 'm5 and stage('m6) <= stage('m1) and provenance('m6) <= provenance('m1) and meet(locality('m6), regional) <= locality('m1) and gc <= 'm5 and gc <= 'm3
      val min_binding : t['a, 'b, 'm1] @ 'm0 -> option['a * 'b] @ 'm2 with portable <= 'm0 and stage('m0) <= stage('m2) and meet(provenance('m0), gc) <= provenance('m2) and release(locality('m0)) <= locality('m2) and 'm1 <= 'm2
      val remove : ('a @ 'm1 -> ('b @ 'm2 -> int @ _) @ _) @ 'm0 -> (t['b, 'c, 'm5] @ 'm4 -> ('a @ 'm7 -> t['b, 'c, 'm5] @ 'm8) @ 'm6) @ 'm3 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm6 and 'm0 <= 'm3 and portable <= 'm4 and stage('m4) <= stage('m2) and meet(provenance('m4), gc) <= provenance('m2) and meet(locality('m4), regional) <= locality('m2) and release(locality('m4)) <= locality('m4) and stage('m4) <= stage('m5) and 'm4 <= 'm8 and 'm4 <= 'm6 and 'm5 <= 'm8 and portable <= 'm7 and stage('m7) <= stage('m1) and provenance('m7) <= provenance('m1) and meet(locality('m7), regional) <= locality('m1) and release(locality('m7)) <= locality('m7) and gc <= 'm8 and release(locality('m8)) <= locality('m8) and gc <= 'm6 and gc <= 'm3
      val fold : ('a @ 'm1 -> ('b @ 'm2 -> ('c @ 'm3 -> 'c @ 'm4) @ _) @ _) @ 'm0 -> (t['a, 'b, 'm7] @ 'm6 -> ('c @ 'm9 -> 'c @ 'm10) @ 'm8) @ 'm5 with portable <= 'm0 and 'm0 <= 'm8 and 'm0 <= 'm5 and 'm4 <= 'm9 and portable <= 'm6 and stage('m6) <= stage('m1) and meet(provenance('m6), gc) <= provenance('m1) and meet(locality('m6), regional) <= locality('m1) and meet(locality('m6), regional) <= locality('m2) and stage('m6) <= stage('m10) and 'm6 <= 'm8 and 'm7 <= 'm2 and 'm9 <= 'm10 and 'm10 <= 'm3 and 'm10 <= global and gc <= 'm8 and gc <= 'm5
      val iter : ('a @ 'm1 -> ('b @ 'm2 -> 'c @ _) @ _) @ 'm0 -> (t['a, 'b, 'm5] @ 'm4 -> unit @ _) @ 'm3 with portable <= 'm0 and 'm0 <= 'm3 and portable <= 'm4 and stage('m4) <= stage('m1) and meet(provenance('m4), gc) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and meet(locality('m4), regional) <= locality('m2) and 'm5 <= 'm2 and gc <= 'm3
      val cardinal : t['a, 'b, _] @ _ -> int @ _
      val to_list : t['a, 'b, 'm1] @ 'm0 -> list['a * 'b] @ 'm2 with stage('m0) <= stage('m2) and release(locality('m0)) <= locality('m2) and 'm1 <= 'm2 and gc <= 'm2
      val of_list : ('a @ 'm1 -> ('a @ 'm2 -> int @ _) @ _) @ 'm0 -> (list['a * 'b] @ 'm4 -> t['a, 'b, 'm6] @ 'm5) @ 'm3 with stage('m0) <= stage('m5) and 'm0 <= 'm3 and portable <= 'm1 and gc <= 'm2 and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and meet(locality('m4), regional) <= locality('m2) and 'm4 <= 'm6 and release(locality('m4)) <= locality('m5) and 'm4 <= gc and portable <= 'm6 and stage('m6) <= stage('m2) and 'm6 <= 'm5 and gc <= 'm5 and gc <= 'm3
    end
    module Lru : sig
      type link['a, 'b, 'm0 : mode] = Nil | Entry of ptr[entry['a, 'b, 'm0] @@ 'm0]
      type entry['a, 'b, 'm0 : mode] = { key : 'a @@ gc; value : 'b; prev : link['a, 'b, 'm0]; next : link['a, 'b, 'm0] }
      type t['a, 'b, 'm0 : mode] = { capacity : int; compare : 'a -> 'a -> int; index : ptr[Map.t['a, ptr[entry['a, 'b, 'm0] @@ 'm0], 'm0] @@ 'm0]; first : ptr[link['a, 'b, 'm0] @@ 'm0]; last : ptr[link['a, 'b, 'm0] @@ 'm0]; size : ptr[int] }
      val create : int @ 'm0 -> (('a -> 'a -> int) @ 'm2 -> t['a, 'b, _] @ 'm3) @ 'm1 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and nonportable gc <= 'm3 and gc <= 'm1
      val length : t['a, 'b, _] @ _ -> int @ _
      module Internal : sig
        val unlink : t['a, 'b, 'm1] @ 'm0 -> (ptr[entry['a, 'b, 'm1] @@ 'm3] @ _ -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and gc <= 'm2
        val push_front : t['a, 'b, 'm1] @ 'm0 -> (ptr[entry['a, 'b, 'm1] @@ 'm1] @ 'm3 -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= global and gc <= 'm2
        val use : t['a, 'b, 'm1] @ 'm0 -> (ptr[entry['a, 'b, 'm1] @@ 'm1] @ 'm3 -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= global and gc <= 'm2
        val evict_last : t['a, 'b, 'm0] @ _ -> unit @ _ with gc <= 'm0
      end
      val find : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> option['b] @ 'm4) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm4 and 'm3 <= gc global and gc <= 'm2
      val add : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> ('b @ 'm5 -> unit @ _) @ 'm4) @ 'm2 with 'm0 <= 'm4 and 'm0 <= 'm2 and nonportable gc <= 'm1 and 'm3 <= 'm4 and 'm3 <= gc global and 'm5 <= 'm1 and 'm5 <= global and gc <= 'm4 and gc <= 'm2
      val to_list : t['a, 'b, 'm0] @ _ -> list['a * 'b] @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    end
    |}]
;;

(* The signature of [source], using [Core.Map] and [Core.Lru], or its errors. *)
let check source =
  match signature source with
  | Ok text -> print_string text
  | Error errors -> List.iter errors ~f:print_endline
;;

(* Where the allocations of the library's toplevels named in [owners] are made. *)
let library_placements owners =
  match placements library with
  | Error errors -> List.iter errors ~f:print_endline
  | Ok (_, lines) ->
    List.iter lines ~f:(fun (loc, line) ->
      if List.exists owners ~f:(fun owner ->
           String.is_substring line ~substring:(sprintf " in %s:" owner))
      then printf "%s %s\n" (Loc.to_string loc) line)
;;

let ext = {|
let ext (s : string) = let e @ external = s in e
|}

let%expect_test "one map's code for C strings and GC strings" =
  check
    (ext
     ^ {|
let gcs = Core.Map.of_list Core.Int.compare [(1, "a"); (2, "b")]
let exts : Core.Map.t[int, string, external] =
  Core.Map.add Core.Int.compare Core.Map.empty 1 (ext "c")
let found (u : unit) = Core.Map.find Core.Int.compare exts 1
let widened : Core.Map.t[int, string, external] = gcs
let both = Core.Map.add Core.Int.compare widened 3 (ext "d")
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val gcs : Core.Map.t[int, string, portable]
    val exts : Core.Map.t[int, string, external] @ external
    val found : unit @ _ -> option[string] @ 'm0 with nonportable external <= 'm0
    val widened : Core.Map.t[int, string, external]
    val both : Core.Map.t[int, string, external] @ external
    |}];
  (* A map of GC strings is a map of possibly external strings (it is
     covariant), not the reverse; what it holds is read as external. *)
  List.iter
    ~f:(fun source -> check (ext ^ source))
    [ {|
let exts : Core.Map.t[int, string, external] =
  Core.Map.add Core.Int.compare Core.Map.empty 1 (ext "c")
let narrowed : Core.Map.t[int, string, gc] = exts
|}
    ; {|
let gcs : Core.Map.t[int, string, gc] = Core.Map.add Core.Int.compare Core.Map.empty 1 (ext "c")
|}
    ; {|
let exts : Core.Map.t[int, string, external] =
  Core.Map.add Core.Int.compare Core.Map.empty 1 (ext "c")
let kept : ptr[string] = Ptr.make ""
let read (u : unit) =
  match Core.Map.find Core.Int.compare exts 1 with
  | Some s -> kept.* <- s
  | None -> ()
|}
    ];
  [%expect
    {|
    This expression has type Core.Map.t[int, string, external] but an expression was expected of type Core.Map.t[int, string, gc]
    Their modes differ: This is external, because the types are unified, then a type is used at one with another mode, but must be at most gc, because the types are unified
    This is external, because it is annotated external (at t.stag:2:31-39), then e is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then a function is used at a type with other modes, then the function returns it (at t.stag:4:87-96), then it is passed to a function (at t.stag:4:40-96), but must be at most gc, because the types are unified, then a type is used at one with another mode, then it is held by a field or argument with a modality (at <core>/map.stag:135:49-50), then v is used (at <core>/map.stag:135:49-50), then v is passed to the function (at <core>/map.stag:129:2-139:66), then a function is used at a type with other modes
    This is external, because the types are unified, then a type is used at one with another mode, then the types are unified, then it is bound to a part (at <core>/map.stag:144:39-40), then v is used (at <core>/map.stag:147:19-20), then the function returns it (at <core>/map.stag:142:2-151:33), then a function is used at a type with other modes, then the function returns it (at t.stag:8:8-45), then s is used (at t.stag:9:24-25), but must be at most gc, because it is stored into a cell, which holds only what fits its modality (at t.stag:9:24-25)
    |}]
;;

let%expect_test "a map made where its user wants it" =
  (* [add] and its helpers return what they build, under [exclave_] where a
     recursive call builds part of it: its nodes follow its result. *)
  library_placements [ "Map.Internal.create"; "Map.add" ];
  [%expect
    {|
    t.stag:42:4-50:11 Partial_application in Map.Internal.create: heap, caller's region if res is local
    t.stag:42:4-50:11 Partial_application in Map.Internal.create: heap, caller's region if res.res is local
    t.stag:42:4-50:11 Partial_application in Map.Internal.create: heap, caller's region if res.res.res is local
    t.stag:43:6-50:11 Box in Map.Internal.create: heap, caller's region if res.res.res.res is local
    t.stag:130:2-140:66 Partial_application in Map.add: heap, caller's region if res is local
    t.stag:130:2-140:66 Partial_application in Map.add: heap, caller's region if res.res is local
    t.stag:130:2-140:66 Partial_application in Map.add: heap, caller's region if res.res.res is local
    t.stag:136:11-75 Box in Map.add: heap, caller's region if res.res.res.res is local
    |}];
  check
    {|
let on_heap = Core.Map.of_list Core.Int.compare [(1, 10); (2, 20)]
let in_region (u : unit) =
  let m @ local = Core.Map.of_list Core.Int.compare [(1, 10); (2, 20)] in
  Core.Map.cardinal m [@nontail]
let scoped (u : unit) =
  region_ (let m = Core.Map.add Core.Int.compare on_heap 3 30 in Core.Map.cardinal m)
let found (u : unit) =
  let m @ local = Core.Map.add Core.Int.compare on_heap 3 30 in
  match Core.Map.find Core.Int.compare m 3 [@nontail] with
  | Some v -> v
  | None -> 0
|};
  [%expect
    {|
    val on_heap : Core.Map.t[int, int, portable]
    val in_region : unit @ _ -> int @ _
    val scoped : unit @ _ -> int @ _
    val found : unit @ _ -> int @ _
    |}];
  List.iter
    ~f:check
    [ {|
let leaks (u : unit) = let m @ local = Core.Map.of_list Core.Int.compare [(1, "a")] in m
|}
    ; {|
let leaks (u : unit) = region_ (let m @ local = Core.Map.of_list Core.Int.compare [(1, "a")] in m)
|}
    ; {|
let kept : ptr[Core.Map.t[int, string, gc]] = Ptr.make Core.Map.empty
let stores (u : unit) = let m @ local = Core.Map.of_list Core.Int.compare [(1, "a")] in kept.* <- m
|}
    ];
  [%expect
    {|
    This is local, because it is annotated local (at t.stag:2:31-36), then m is used (at t.stag:2:87-88), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:23-88)
    This is local, because it is annotated local (at t.stag:2:40-45), then m is used (at t.stag:2:96-97), then the [region_] returns it (at t.stag:2:31-98), then the [region_] gives it (at t.stag:2:23-98), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:23-98)
    This is local, because it is annotated local (at t.stag:2:40-45), then m is used (at t.stag:2:96-97), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:31-98)
    This is local, because it is annotated local (at t.stag:3:32-37), then m is used (at t.stag:3:98-99), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:98-99)
    |}]
;;

let%expect_test "folding with local closures" =
  check
    {|
let m = Core.Map.of_list Core.Int.compare [(1, 10); (2, 20); (3, 30)]
let total (u : unit) =
  let sum = stack_ (Ptr.make 0) in
  Core.Map.iter (fun _ v -> sum.* <- Int.add sum.* v) m;
  sum.*
let keys (u : unit) = Core.Map.fold (fun k _ acc -> Cons (Box.make (k, acc))) m []
|};
  [%expect
    {|
    val m : Core.Map.t[int, int, portable]
    val total : unit @ _ -> int @ _
    val keys : unit @ _ -> list[int] @ 'm0 with gc <= 'm0
    |}];
  Source_helpers.run
    {|
let m = Core.Map.of_list Core.Int.compare [(1, 10); (2, 20); (3, 30)]
let total =
  let sum = stack_ (Ptr.make 0) in
  Core.Map.iter (fun _ v -> sum.* <- Int.add sum.* v) m;
  sum.*
|};
  [%expect
    {|
    val m : Core.Map.t[int, int, portable]
    val total : int
    -- evaluated at compile time
    -- evaluated
    (m
     (Core.Map.Node
      (Box
       (Core.Map.node
        (left
         (Core.Map.Node
          (Box
           (Core.Map.node (left (Core.Map.Empty)) (key 1) (value 10)
            (right (Core.Map.Empty)) (height 1)))))
        (key 2) (value 20)
        (right
         (Core.Map.Node
          (Box
           (Core.Map.node (left (Core.Map.Empty)) (key 3) (value 30)
            (right (Core.Map.Empty)) (height 1)))))
        (height 2)))))
    (total 60)
    |}]
;;

let%expect_test "a keyword table built at compile time" =
  Source_helpers.run
    {|
let keywords = .~(
  let table @ portable = Core.Map.of_list Core.String.compare [("let", 1); ("in", 2); ("match", 3)] in
  .< table >.)
let is_keyword (s : string) = Core.Map.mem Core.String.compare keywords s
let checks = (is_keyword "match", is_keyword "x")
|};
  [%expect
    {|
    val keywords : Core.Map.t[string, int, portable]
    val is_keyword : string -> bool
    val checks : bool * bool
    -- evaluated at compile time
    -- evaluated
    (keywords
     (Core.Map.Node
      (Box
       (Core.Map.node
        (left
         (Core.Map.Node
          (Box
           (Core.Map.node (left (Core.Map.Empty)) (key (String in)) (value 2)
            (right (Core.Map.Empty)) (height 1)))))
        (key (String let)) (value 1)
        (right
         (Core.Map.Node
          (Box
           (Core.Map.node (left (Core.Map.Empty)) (key (String match)) (value 3)
            (right (Core.Map.Empty)) (height 1)))))
        (height 2)))))
    (is_keyword <fun>)
    (checks (Tuple true false))
    |}]
;;

let%expect_test "an LRU cache of C strings" =
  check
    (ext
     ^ {|
let exts : Core.Lru.t[string, string, external] = Core.Lru.create 2 Core.String.compare
let fill (u : unit) = Core.Lru.add exts "a" (ext "x")
let gcs : Core.Lru.t[string, string, gc] = Core.Lru.create 2 Core.String.compare
let fill_gc (u : unit) = Core.Lru.add gcs "a" "x"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val exts : Core.Lru.t[string, string, external]
    val fill : unit @ _ -> unit @ _
    val gcs : Core.Lru.t[string, string, gc]
    val fill_gc : unit @ _ -> unit @ _
    |}];
  (* A cache is mutable, so invariant: a cache of GC strings is not one of
     external strings (storing a C string through the latter would break the
     former), nor the reverse. *)
  List.iter
    ~f:(fun source -> check (ext ^ source))
    [ {|
let gcs : Core.Lru.t[string, string, gc] = Core.Lru.create 2 Core.String.compare
let as_ext : Core.Lru.t[string, string, external] = gcs
|}
    ; {|
let gcs : Core.Lru.t[string, string, gc] = Core.Lru.create 2 Core.String.compare
let fill (u : unit) = Core.Lru.add gcs "a" (ext "x")
|}
    ; {|
let exts : Core.Lru.t[string, string, external] = Core.Lru.create 2 Core.String.compare
let kept : ptr[string] = Ptr.make ""
let read (u : unit) =
  match Core.Lru.find exts "a" with
  | Some s -> kept.* <- s
  | None -> ()
|}
    ];
  [%expect
    {|
    This expression has type Core.Lru.t[string, string, gc] but an expression was expected of type Core.Lru.t[string, string, external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    This is external, because it is annotated external (at t.stag:2:31-39), then e is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then a function is used at a type with other modes, then the function returns it (at t.stag:5:43-52), then it is passed to a function (at t.stag:5:22-52), but must be at most gc, because the types are unified, then Ptr.make holds it, then a function is used at a type with other modes, then it is passed to a function (at <core>/lru.stag:93:15-68), then v is used (at <core>/lru.stag:93:42-43), then v is passed to the function (at <core>/lru.stag:88:2-99:17), then a function is used at a type with other modes
    This is external, because the types are unified, then it is read from a pointer's cell, which may hold it (at <core>/lru.stag:83:9-15), then it is dereferenced (at <core>/lru.stag:83:9-15), then its field is read (at <core>/lru.stag:83:9-21), then the function returns it (at <core>/lru.stag:79:2-83:21), then a function is used at a type with other modes, then the function returns it (at t.stag:7:8-30), then s is used (at t.stag:8:24-25), but must be at most gc, because it is stored into a cell, which holds only what fits its modality (at t.stag:8:24-25)
    |}]
;;

let%expect_test "an LRU cache in a region" =
  library_placements [ "Lru.create"; "Lru.add" ];
  [%expect
    {|
    t.stag:242:30-249:1 Partial_application in Lru.create: heap, caller's region if res is local
    t.stag:245:10-28 Pointer in Lru.create: heap, caller's region if res.res is local
    t.stag:246:10-22 Pointer in Lru.create: heap, caller's region if res.res is local
    t.stag:247:9-21 Pointer in Lru.create: heap, caller's region if res.res is local
    t.stag:248:9-19 Pointer in Lru.create: heap, caller's region if res.res is local
    t.stag:299:2-310:17 Partial_application in Lru.add: heap, caller's region if res is local
    t.stag:299:2-310:17 Partial_application in Lru.add: heap, caller's region if res.res is local
    t.stag:304:15-68 Pointer in Lru.add: heap
    |}];
  check
    {|
let scoped (u : unit) =
  region_ (
    let c @ local = Core.Lru.create 2 Core.Int.compare in
    Core.Lru.add c 1 "a";
    Core.Lru.add c 2 "b";
    Core.Lru.length c)
let leaks (u : unit) = region_ (let c @ local = Core.Lru.create 2 Core.Int.compare in c)
|};
  [%expect
    {|
    This is local, because it is annotated local (at t.stag:8:40-45), then c is used (at t.stag:8:86-87), then the [region_] returns it (at t.stag:8:31-88), then the [region_] gives it (at t.stag:8:23-88), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:8:23-88)
    This is local, because it is annotated local (at t.stag:8:40-45), then c is used (at t.stag:8:86-87), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:8:31-88)
    |}]
;;
