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
  [%expect {|
    module Map : sig
      type t['a, 'b, 'm0 : mode] = Empty | Node of box[node['a, 'b, 'm0]]
      type node['a, 'b, 'm0 : mode] = { left : t['a, 'b, 'm0]; key : 'a; value : 'b @@ 'm0; right : t['a, 'b, 'm0]; height : int }
      val empty : t['a, 'b, _]
      val is_empty : t['a, 'b, _] @ _ -> bool @ _
      val height : t['a, 'b, _] @ _ -> int @ _
      module Internal : sig
        val max : int @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
        val create : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> ('b @ 'm5 -> (t['a, 'b, 'm1] @ 'm7 -> t['a, 'b, 'm9] @ 'm8) @ 'm6) @ 'm4) @ 'm2 with 'm0 <= 'm8 and release(locality('m0)) <= locality('m8) and 'm0 <= 'm6 and 'm0 <= 'm4 and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm9 and 'm3 <= 'm8 and release(locality('m3)) <= locality('m8) and 'm3 <= 'm6 and 'm3 <= 'm4 and 'm5 <= 'm1 and 'm5 <= 'm8 and release(locality('m5)) <= locality('m8) and 'm5 <= 'm6 and 'm7 <= 'm8 and release(locality('m7)) <= locality('m8) and gc <= 'm8 and gc <= 'm6 and gc <= 'm4 and gc <= 'm2
        val bal : t['a, 'b, 'm1] @ 'm0 -> ('a @ 'm3 -> ('b @ 'm5 -> (t['a, 'b, 'm1] @ 'm7 -> t['a, 'b, 'm9] @ 'm8) @ 'm6) @ 'm4) @ 'm2 with 'm0 <= 'm8 and release(locality('m0)) <= locality('m8) and 'm0 <= 'm6 and 'm0 <= 'm4 and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm9 and 'm1 <= 'm8 and 'm3 <= 'm8 and release(locality('m3)) <= locality('m8) and 'm3 <= 'm6 and 'm3 <= 'm4 and 'm5 <= 'm1 and release(locality('m5)) <= locality('m8) and 'm5 <= 'm6 and 'm7 <= 'm8 and release(locality('m7)) <= locality('m8) and gc <= 'm8 and gc <= 'm6 and gc <= 'm4 and gc <= 'm2
        val onto : t['a, 'b, 'm1] @ 'm0 -> (list['a * 'b] @ 'm3 -> list['a * 'b] @ 'm3) @ 'm2 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm3 and 'm0 <= 'm2 and 'm1 <= 'm3 and gc <= 'm3 and release(locality('m3)) <= locality('m3) and gc <= 'm2
        val remove_min : t['a, 'b, 'm1] @ 'm0 -> t['a, 'b, 'm1] @ 'm2 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm2 and gc <= 'm2 and release(locality('m2)) <= locality('m2)
        val merge : t['a, 'b, 'm1] @ 'm0 -> (t['a, 'b, 'm1] @ 'm3 -> t['a, 'b, 'm1] @ 'm4) @ 'm2 with 'm0 <= 'm4 and release(locality('m0)) <= locality('m4) and 'm0 <= 'm2 and portable <= 'm1 and 'm1 <= 'm4 and 'm3 <= 'm1 and release(locality('m3)) <= locality('m4) and gc <= 'm4 and gc <= 'm2
      end
      val singleton : 'a @ 'm0 -> ('b @ 'm2 -> t['a, 'b, 'm4] @ 'm3) @ 'm1 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and 'm2 <= 'm4 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and portable <= 'm4 and gc <= 'm3 and gc <= 'm1
      val add : ('a @ 'm1 -> ('a @ 'm2 -> int @ 'm3) @ _) @ 'm0 -> (t['a, 'b, 'm6] @ 'm5 -> ('a @ 'm8 -> ('b @ 'm10 -> t['a, 'b, 'm6] @ 'm11) @ 'm9) @ 'm7) @ 'm4 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm9 and 'm0 <= 'm7 and 'm0 <= 'm4 and stage('m3) <= stage('m11) and portable <= 'm5 and stage('m5) <= stage('m2) and provenance('m5) <= provenance('m2) and meet(locality('m5), regional) <= locality('m2) and release(locality('m5)) <= locality('m5) and 'm5 <= 'm11 and 'm5 <= 'm9 and 'm5 <= 'm7 and 'm6 <= 'm11 and portable <= 'm8 and stage('m8) <= stage('m1) and provenance('m8) <= provenance('m1) and meet(locality('m8), regional) <= locality('m1) and release(locality('m8)) <= locality('m8) and 'm8 <= 'm11 and 'm8 <= 'm9 and portable <= 'm10 and 'm10 <= 'm6 and release(locality('m10)) <= locality('m10) and 'm10 <= 'm11 and gc <= 'm11 and release(locality('m11)) <= locality('m11) and gc <= 'm9 and gc <= 'm7 and gc <= 'm4
      val find : ('a @ 'm1 -> ('b @ 'm2 -> int @ 'm3) @ _) @ 'm0 -> (t['b, 'c, 'm6] @ 'm5 -> ('a @ 'm8 -> option['c] @ 'm9) @ 'm7) @ 'm4 with portable <= 'm0 and 'm0 <= 'm7 and 'm0 <= 'm4 and stage('m3) <= stage('m9) and portable <= 'm5 and stage('m5) <= stage('m2) and provenance('m5) <= provenance('m2) and meet(locality('m5), regional) <= locality('m2) and stage('m5) <= stage('m9) and release(locality('m5)) <= locality('m9) and 'm5 <= 'm7 and 'm6 <= 'm9 and portable <= 'm8 and stage('m8) <= stage('m1) and provenance('m8) <= provenance('m1) and meet(locality('m8), regional) <= locality('m1) and gc <= 'm7 and gc <= 'm4
      val mem : ('a @ 'm1 -> ('b @ 'm2 -> int @ _) @ _) @ 'm0 -> (t['b, 'c, _] @ 'm4 -> ('a @ 'm6 -> bool @ _) @ 'm5) @ 'm3 with 'm0 <= 'm5 and 'm0 <= 'm3 and portable <= 'm1 and portable <= 'm2 and stage('m4) <= stage('m2) and provenance('m4) <= provenance('m2) and meet(locality('m4), regional) <= locality('m2) and 'm4 <= 'm5 and stage('m6) <= stage('m1) and provenance('m6) <= provenance('m1) and meet(locality('m6), regional) <= locality('m1) and gc <= 'm5 and gc <= 'm3
      val min_binding : t['a, 'b, 'm1] @ 'm0 -> option['a * 'b] @ 'm2 with portable <= 'm0 and 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm1 <= 'm2
      val remove : ('a @ 'm1 -> ('b @ 'm2 -> int @ 'm3) @ _) @ 'm0 -> (t['b, 'c, 'm6] @ 'm5 -> ('a @ 'm8 -> t['b, 'c, 'm6] @ 'm9) @ 'm7) @ 'm4 with portable <= 'm0 and release(locality('m0)) <= locality('m0) and 'm0 <= 'm7 and 'm0 <= 'm4 and stage('m3) <= stage('m9) and portable <= 'm5 and stage('m5) <= stage('m2) and provenance('m5) <= provenance('m2) and meet(locality('m5), regional) <= locality('m2) and release(locality('m5)) <= locality('m5) and 'm5 <= 'm6 and 'm5 <= 'm9 and 'm5 <= 'm7 and 'm6 <= 'm9 and portable <= 'm8 and stage('m8) <= stage('m1) and provenance('m8) <= provenance('m1) and meet(locality('m8), regional) <= locality('m1) and release(locality('m8)) <= locality('m8) and gc <= 'm9 and release(locality('m9)) <= locality('m9) and gc <= 'm7 and gc <= 'm4
      val fold : ('a @ 'm1 -> ('b @ 'm2 -> ('c @ 'm3 -> 'c @ 'm4) @ _) @ _) @ 'm0 -> (t['a, 'b, 'm7] @ 'm6 -> ('c @ 'm9 -> 'c @ 'm10) @ 'm8) @ 'm5 with portable <= 'm0 and 'm0 <= 'm8 and 'm0 <= 'm5 and 'm4 <= 'm9 and portable <= 'm6 and stage('m6) <= stage('m1) and provenance('m6) <= provenance('m1) and meet(locality('m6), regional) <= locality('m1) and meet(locality('m6), regional) <= locality('m2) and stage('m6) <= stage('m10) and 'm6 <= 'm8 and 'm7 <= 'm2 and 'm9 <= 'm10 and 'm10 <= 'm3 and 'm10 <= global and gc <= 'm8 and gc <= 'm5
      val iter : ('a @ 'm1 -> ('b @ 'm2 -> 'c @ _) @ _) @ 'm0 -> (t['a, 'b, 'm5] @ 'm4 -> unit @ _) @ 'm3 with portable <= 'm0 and 'm0 <= 'm3 and portable <= 'm4 and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and meet(locality('m4), regional) <= locality('m2) and 'm5 <= 'm2 and gc <= 'm3
      val cardinal : t['a, 'b, _] @ _ -> int @ _
      val to_list : t['a, 'b, 'm1] @ 'm0 -> list['a * 'b] @ 'm2 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2) and 'm1 <= 'm2 and gc <= 'm2
      val of_list : ('a @ 'm1 -> ('a @ 'm2 -> int @ 'm3) @ _) @ 'm0 -> (list['a * 'b] @ 'm5 -> t['a, 'b, 'm7] @ 'm6) @ 'm4 with stage('m0) <= stage('m6) and 'm0 <= 'm4 and portable <= 'm1 and gc <= 'm2 and stage('m3) <= stage('m2) and stage('m3) <= stage('m6) and stage('m5) <= stage('m1) and provenance('m5) <= provenance('m1) and meet(locality('m5), regional) <= locality('m1) and meet(locality('m5), regional) <= locality('m2) and 'm5 <= 'm7 and release(locality('m5)) <= locality('m6) and portable <= 'm7 and 'm7 <= 'm2 and 'm7 <= 'm6 and gc <= 'm6 and gc <= 'm4
    end
    module Lru : sig
      type link['a, 'b, 'm0 : mode] = Nil | Entry of ptr[entry['a, 'b, 'm0] @@ 'm0]
      type entry['a, 'b, 'm0 : mode] = { key : 'a; value : 'b; prev : link['a, 'b, 'm0]; next : link['a, 'b, 'm0] }
      type t['a, 'b, 'm0 : mode] = { capacity : int; compare : 'a -> 'a -> int; index : ptr[Map.t['a, ptr[entry['a, 'b, 'm0] @@ 'm0], gc]]; first : ptr[link['a, 'b, 'm0]]; last : ptr[link['a, 'b, 'm0]]; size : ptr[int] }
      val create : int @ 'm0 -> (('a -> 'a -> int) @ 'm2 -> t['a, 'b, _] @ 'm3) @ 'm1 with 'm0 <= 'm3 and release(locality('m0)) <= locality('m3) and 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and nonportable gc <= 'm3 and gc <= 'm1
      val length : t['a, 'b, _] @ _ -> int @ _
      module Internal : sig
        val unlink : t['a, 'b, 'm1] @ 'm0 -> (ptr[entry['a, 'b, 'm1] @@ 'm3] @ _ -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= gc and gc <= 'm2
        val push_front : t['a, 'b, 'm1] @ 'm0 -> (ptr[entry['a, 'b, 'm1] @@ 'm1] @ 'm3 -> unit @ _) @ 'm2 with 'm0 <= 'm2 and nonportable gc <= 'm1 and 'm3 <= gc global and gc <= 'm2
        val use : t['a, 'b, gc] @ 'm0 -> (ptr[entry['a, 'b, gc]] @ 'm2 -> unit @ _) @ 'm1 with 'm0 <= 'm1 and 'm2 <= gc global and gc <= 'm1
        val evict_last : t['a, 'b, 'm0] @ _ -> unit @ _ with portable <= 'm0 and 'm0 <= gc
      end
      val find : t['a, 'b, gc] @ 'm0 -> ('a @ 'm2 -> option['b] @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= gc global and nonportable gc <= 'm3 and gc <= 'm1
      val add : t['a, 'b, gc] @ 'm0 -> ('a @ 'm2 -> ('b @ 'm4 -> unit @ _) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm3 and 'm2 <= gc global and 'm4 <= gc global and gc <= 'm3 and gc <= 'm1
      val to_list : t['a, 'b, 'm0] @ _ -> list['a * 'b] @ 'm1 with 'm0 <= 'm1 and nonportable gc <= 'm1
    end
    |}]
;;
