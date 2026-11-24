open! Import
open Source_helpers
open Mode_helpers

let round_trip source = round_trip source ~show:`Signature

let check source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

let ext = {|
let ext (x : string) = let y @ external = x in y
|}

let%expect_test "field modalities, one axis at a time" =
  (* Locality: what a [@@ global] field holds outlives the record. *)
  check
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let g (r @ local) = Ptr.make r.g
|};
  [%expect
    {|
    type r = { g : ptr[int] @@ global; l : ptr[int] }
    val g : r @ 'm0 -> ptr[ptr[int] @@ 'm2] @ 'm1 with local <= 'm0 and 'm0 <= 'm2 and portable <= 'm2 and nonportable gc <= 'm1
    |}];
  check
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let l (r @ local) = Ptr.make r.l
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:3:11-16), then r is passed to the function (at t.stag:3:20-32), then r is used (at t.stag:3:29-30), then its field is read (at t.stag:3:29-32), then it is passed to a function (at t.stag:3:20-32), but must be at most global, because Ptr.make holds it, then the function types are unified |}];
  (* Stage: what a [@@ portable] field holds is portable, whatever else the
     record holds. *)
  check
    {|
type r = { p : string -> string @@ portable; n : string -> string }
let make u = { p = (fun x -> x); n = (let n = Int.to_string 3 in fun x -> String.append x n) }
let p u = let f @ portable = (make ()).p in f
|};
  [%expect
    {|
    type r = { p : (string -> string) @@ portable; n : string -> string }
    val make : 'a -> r @ 'm0 with nonportable gc <= 'm0
    val p : 'a -> (string @ nonportable gc global -> string @ nonportable gc global) @ 'm0 with gc <= 'm0
    |}];
  check
    {|
type r = { p : string -> string @@ portable; n : string -> string }
let make u = { p = (fun x -> x); n = (let n = Int.to_string 3 in fun x -> String.append x n) }
let n u = let f @ portable = (make ()).n in f
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:46-61), then n is bound (at t.stag:3:37-92), then the function returns it (at t.stag:3:13-94), then the function types are unified, then the function returns it (at t.stag:4:29-38), then its field is read (at t.stag:4:29-40), then it is bound by an annotated let (at t.stag:4:18-26), but must be at most portable, because it is annotated portable (at t.stag:4:18-26) |}];
  check
    {|
type r = { p : string -> string @@ portable }
let bad u = { p = (let n = Int.to_string 3 in fun x -> String.append x n) }
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:27-42), then n is bound (at t.stag:3:18-73), but must be at most portable, because it is held by a field or argument with a modality (at t.stag:3:18-73) |}];
  (* Provenance: a [@@ gc] field is gc even when the record holds external
     values. *)
  check
    (ext
     ^ {|
type r = { owned : string @@ gc; borrowed : string }
let make u = { owned = "a"; borrowed = ext "b" }
let owned u = let s @ gc = (make ()).owned in s
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type r = { owned : string @@ gc; borrowed : string }
    val make : 'a -> r @ external
    val owned : 'a -> string @ 'm0 with gc <= 'm0
    |}];
  check
    (ext
     ^ {|
type r = { owned : string @@ gc; borrowed : string }
let make u = { owned = "a"; borrowed = ext "b" }
let borrowed u = let s @ gc = (make ()).borrowed in s
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:5:39-46), then the function returns it (at t.stag:5:13-48), then the function types are unified, then the function returns it (at t.stag:6:30-39), then its field is read (at t.stag:6:30-48), then it is bound by an annotated let (at t.stag:6:25-27), but must be at most gc, because it is annotated gc (at t.stag:6:25-27) |}];
  check
    (ext
     ^ {|
type r = { owned : string @@ gc }
let bad u = { owned = ext "a" }
|});
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:5:22-29), but must be at most gc, because it is held by a field or argument with a modality (at t.stag:5:22-29) |}];
  (* Several axes at once, and as constructor arguments. *)
  check
    (ext
     ^ {|
type t = A of string @@ global portable gc | B of string
let a (x @ local) = match x with A s -> let s2 @ gc = s in Ptr.make s2 | B _ -> Ptr.make "b"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type t = A of string @@ portable gc global | B of string
    val a : t @ local -> ptr[string @@ 'm1] @ 'm0 with gc <= 'm1 and nonportable gc <= 'm0
    |}]
;;

let%expect_test "meet on values, exact through storage" =
  (* Read from a record, a field is at most the record's mode and its modality:
     a static record's field is static. *)
  check
    {|
type r = { s : string @@ portable }
let r = { s = "a" }
let read u = let s @ static = r.s in s
|};
  [%expect
    {|
    type r = { s : string @@ portable }
    val r : r
    val read : 'a -> string
    |}];
  (* Read through a pointer, it is what the cell's [@@] says, whatever the
     pointer's own mode: a cell's aliases may store anything that fits. *)
  check
    {|
let p : ptr[string @@ external] = Ptr.make "a"
let read u = let s @ static = p.* in s
|};
  [%expect
    {| This is external, because it is read from a pointer's cell, which may hold it (at t.stag:3:30-33), then it is dereferenced (at t.stag:3:30-33), then it is bound by an annotated let (at t.stag:3:21-27), but must be at most static, because it is annotated static (at t.stag:3:21-27) |}];
  (* A pointer's own provenance doesn't reach what is read from its cell. *)
  check
    (ext ^ {|
let read (p : ptr[string] @ external) = let s @ gc = p.* in s
|});
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val read : ptr[string] @ external -> string @ 'm0 with nonportable gc <= 'm0
    |}]
;;

let%expect_test "modalities compose along a path through storage" =
  check
    (ext
     ^ {|
type r = { owned : string @@ gc; any : string }
let p : ptr[r @@ external] = Ptr.make { owned = "a"; any = "b" }
let set_any u = p.*.any <- ext "c"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type r = { owned : string @@ gc; any : string }
    val p : ptr[r @@ external]
    val set_any : 'a -> unit
    |}];
  check
    {|
type r = { owned : string @@ gc; any : string }
let p : ptr[r @@ external] = Ptr.make { owned = "a"; any = "b" }
let read_any u = let s @ gc = p.*.any in s
|};
  [%expect
    {| This is external, because it is read from a pointer's cell, which may hold it (at t.stag:4:30-33), then it is dereferenced (at t.stag:4:30-33), then its field is read (at t.stag:4:30-37), then it is bound by an annotated let (at t.stag:4:25-27), but must be at most gc, because it is annotated gc (at t.stag:4:25-27) |}];
  check
    (ext
     ^ {|
type r = { owned : string @@ gc; any : string }
let p : ptr[r @@ external] = Ptr.make { owned = "a"; any = "b" }
let set_owned u = p.*.owned <- ext "c"
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:31-38), but must be at most gc, because it is stored into a cell, which holds only what fits its modality (at t.stag:6:31-38) |}];
  check
    {|
type r = { owned : string @@ gc; any : string }
let p : ptr[r @@ external] = Ptr.make { owned = "a"; any = "b" }
let read_owned u = let s @ gc = p.*.owned in s
|};
  [%expect
    {|
    type r = { owned : string @@ gc; any : string }
    val p : ptr[r @@ external]
    val read_owned : 'a -> string @ 'm0 with nonportable gc <= 'm0
    |}];
  (* Only the last cell's [@@] says what is stored: the earlier ones hold the
     pointer to it. *)
  check
    (ext
     ^ {|
let p : ptr[ptr[string @@ external]] = Ptr.make (Ptr.make "a")
let set u = p.*.* <- ext "b"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val p : ptr[ptr[string @@ external]]
    val set : 'a -> unit
    |}]
;;

let%expect_test "stage modality on [ptr]" =
  check
    {|
let p : ptr[(string -> string) @@ portable] = Ptr.make (fun x -> x)
let read u = let f @ portable = p.* in f
let store u = p.* <- (fun x -> String.append x x)
|};
  [%expect
    {|
    val p : ptr[(string @ '_m0 -> string @ '_m1) @@ portable] with '_m0 <= '_m1 and release(locality('_m0)) <= locality('_m1) and nonportable gc <= '_m1
    val read : 'a -> (string @ '_m1 -> string @ '_m2) @ 'm0 with '_m1 <= '_m2 and release(locality('_m1)) <= locality('_m2) and nonportable gc <= '_m2 and gc <= 'm0
    val store : 'a -> unit
    |}];
  check
    {|
let p : ptr[(string -> string) @@ portable] = Ptr.make (fun x -> x)
let store u = p.* <- (let n = Int.to_string 3 in fun x -> String.append x n)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:30-45), then n is bound (at t.stag:3:21-76), but must be at most portable, because it is stored into a cell, which holds only what fits its modality (at t.stag:3:21-76) |}];
  check
    {|
let p : ptr[string -> string] = Ptr.make (fun x -> x)
let read u = let f @ portable = p.* in f
|};
  [%expect
    {| This is nonportable, because it is read from a pointer's cell, which may hold it (at t.stag:3:32-35), then it is dereferenced (at t.stag:3:32-35), then it is bound by an annotated let (at t.stag:3:21-29), but must be at most portable, because it is annotated portable (at t.stag:3:21-29) |}];
  check {|
let f (p : ptr[int @@ local]) = p
|};
  [%expect
    {| A mode argument is on stage and provenance only ([const], [portable], [nonportable], [static], [gc], [external]) or a mode variable, as in [ptr[string @@ external]]: a pointer's cell is always [global], and a type's mode parameters have no locality |}];
  check {|
let f (p : ptr[int @@ portable external]) = p
|};
  [%expect
    {| val f : ptr[int @@ portable external] @ 'm0 -> ptr[int @@ portable external] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) |}]
;;

let%expect_test "mode parameters" =
  round_trip
    {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
type tagged['p : mode] = { tag : int; payload : string @@ 'p }
let get (c : cell['a, 'p]) = c.v.*
let set (c : cell['a, 'p]) v = c.v.* <- v
let ext_cell : cell[string, external] = { v = Ptr.make "a" }
let portable_cell : cell[string, portable gc] = { v = Ptr.make "a" }
let gc_cell : cell[string, gc] = { v = Ptr.make "a" }
let tag (t : tagged['p]) = t.tag
let payload (t : tagged['p]) = t.payload
let static_tagged : tagged[static] = { tag = 1; payload = "a" }
|};
  [%expect
    {|
    type cell['a, 'm0 : mode] = { v : ptr['a @@ 'm0] }
    type tagged['m0 : mode] = { tag : int; payload : string @@ 'm0 }
    val get : cell['a, 'm0] -> 'a @ 'm1 with 'm0 <= 'm1
    val set : cell['a, 'm1] @ 'm0 -> ('a @ 'm3 -> unit) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= global and gc <= 'm2
    val ext_cell : cell[string, external]
    val portable_cell : cell[string, portable]
    val gc_cell : cell[string, gc]
    val tag : tagged['m0] -> int
    val payload : tagged['m1] @ 'm0 -> string @ 'm2 with release(locality('m0)) <= locality('m2) and 'm1 <= 'm2
    val static_tagged : tagged[static]
    |}]
;;

let%expect_test "mode parameters: what they allow" =
  check
    (ext
     ^ {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let c : cell[string, external] = { v = Ptr.make "a" }
let ok u = c.v.* <- ext "b"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type cell['a, 'm0 : mode] = { v : ptr['a @@ 'm0] }
    val c : cell[string, external]
    val ok : 'a -> unit
    |}];
  check
    (ext
     ^ {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let c : cell[string, gc] = { v = Ptr.make "a" }
let bad u = c.v.* <- ext "b"
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:21-28), then it is stored into a cell, which holds only what fits its modality (at t.stag:6:21-28), but must be at most gc, because the types are unified |}];
  check
    {|
type tagged['p : mode] = { tag : int; payload : string @@ 'p }
let t : tagged[external] = { tag = 1; payload = "a" }
let bad u = let s @ gc = t.payload in s
|};
  [%expect
    {| This is external, because the types are unified, then its field is read (at t.stag:4:25-34), then it is bound by an annotated let (at t.stag:4:20-22), but must be at most gc, because it is annotated gc (at t.stag:4:20-22) |}];
  (* Mode arguments are invariant. *)
  check
    {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let f (c : cell[string, gc]) : cell[string, external] = c
|};
  [%expect
    {|
    This expression has type cell[string, gc] but an expression was expected of type cell[string, external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because the types are unified
    |}]
;;

let%expect_test "mode parameters: errors" =
  check
    {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let f (c : cell[int, string]) = c
|};
  [%expect {| The parameter p of type cell expects a mode |}];
  check
    {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let f (c : cell[gc, int]) = c
|};
  [%expect {| Unbound type gc |}];
  check {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'a] }
|};
  [%expect
    {| The variable a is used as more than one of a type, a row and a mode |}];
  check {|
type cell['a, 'p : mode] = { v : 'p }
|};
  [%expect
    {| The variable p is used as more than one of a type, a row and a mode |}];
  check {|
type cell['a] = { v : ptr['a @@ 'q] }
|};
  [%expect
    {| The mode variable 'q is not allowed in a declaration unless it is a parameter declared ['q : mode]: its arrows have fixed modes |}];
  check
    {|
type cell['p : mode] = { v : ptr[int @@ 'p] }
let f (c : cell[local]) = c
|};
  [%expect
    {| A mode argument is on stage and provenance only ([const], [portable], [nonportable], [static], [gc], [external]) or a mode variable, as in [ptr[string @@ external]]: a pointer's cell is always [global], and a type's mode parameters have no locality |}];
  (* Mode parameters have no locality, also where one only meets a field. *)
  check
    {|
type tagged['p : mode] = { payload : string @@ 'p }
let f (t : tagged[global]) = t
|};
  [%expect
    {| A mode argument is on stage and provenance only ([const], [portable], [nonportable], [static], [gc], [external]) or a mode variable, as in [ptr[string @@ external]]: a pointer's cell is always [global], and a type's mode parameters have no locality |}];
  (* A name is a mode variable or a type variable, not both. *)
  check
    {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
let f (c : cell['a, 'a]) = c
|};
  [%expect
    {| The variable a is used as more than one of a type, a row and a mode |}];
  check {|
let f (p : ptr[string @@ 'a]) (x : 'a) = p
|};
  [%expect
    {| The variable a is used as more than one of a type, a row and a mode |}];
  check {|
let f (x : 'a) (y @ 'a) = x
|};
  [%expect
    {| The variable a is used as more than one of a type, a row and a mode |}];
  (* An unknown mode argument is named in errors, not left out as the default. *)
  check {|
let f (p : ptr[string @@ 'm]) : int = p
|};
  [%expect
    {| This expression has type ptr[string @@ 'm0] but an expression was expected of type int |}]
;;

let%expect_test "a modality's locality is only [global]" =
  (* [regional] is relative to a region: a block returned to the caller's region
     with [exclave_] would keep it regional there, though its parts were
     allocated in the callee's. *)
  check
    {|
type r = { f : ptr[int] @@ regional }
let mk (x : ptr[int] @ local) = let r = { f = x } in exclave_ r
let h (u : unit) =
  let s = stack_ (Ptr.make 1) in
  let x = (mk s).f in
  exclave_ x
|};
  [%expect
    {| A modality's locality can only be [global]: [regional] and [local] are relative to a region, and a part read from a block returned to the caller's region (with [exclave_]) would outlive the region it was allocated in |}];
  check {|
type t = A of string @@ local
|};
  [%expect
    {| A modality's locality can only be [global]: [regional] and [local] are relative to a region, and a part read from a block returned to the caller's region (with [exclave_]) would outlive the region it was allocated in |}]
;;

let%expect_test "record update keeps the mode parameters of what it copies" =
  check
    (ext
     ^ {|
type two['p : mode] = { a : string @@ 'p; b : int }
let forge (t : two[external]) : two[static] = { t with b = 1 }
|}
    );
  [%expect
    {|
    This expression has type two[static] but an expression was expected of type two[external]
    Their modes differ: This is external, because the types are unified, but must be at most static, because the types are unified
    |}];
  (* A mode parameter only of updated fields may change. *)
  check
    (ext
     ^ {|
type two['p : mode] = { a : string @@ 'p; b : int }
let launder (t : two[external]) : two[static] = { t with a = "a" }
let infer (t : two['p]) = { t with b = 1 }
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type two['m0 : mode] = { a : string @@ 'm0; b : int }
    val launder : two[external] @ 'm0 -> two[static] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val infer : two['m1] @ 'm0 -> two['m1] @ 'm2 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2)
    |}]
;;

let%expect_test "reading a cell" =
  (* What a [portable] cell holds is portable, so can be embedded in code. *)
  check
    {|
let p : ptr[string @@ portable] = Ptr.make "a"
let read u = let s @ portable = p.* in .< s >.
|};
  [%expect
    {|
    val p : ptr[string @@ portable]
    val read : 'a -> <string>{..'b} @ 'm0 with gc <= 'm0
    |}];
  (* A cell may change between evaluations, so what is read from it is never
     [const], even from a [const] cell. *)
  check
    {|
let p : ptr[string @@ const] = Ptr.make "a"
let read u = let s @ portable = p.* in s
let bad u = let s @ const = p.* in s
|};
  [%expect
    {| This is portable, because it is read from a pointer's cell, which may hold it (at t.stag:4:28-31), then it is dereferenced (at t.stag:4:28-31), then it is bound by an annotated let (at t.stag:4:20-25), but must be at most const, because it is annotated const (at t.stag:4:20-25) |}]
;;

let%expect_test "in quotes, re-checked once erased" =
  check
    (ext
     ^ {|
type cell['a, 'p : mode] = { v : ptr['a @@ 'p] }
type r = { owned : string @@ gc; any : string }
let code = .< fun (c : cell[string, external]) -> c.v.* <- ext "a" >.
let fields = .< fun (r : r) -> let s @ gc = r.owned in s >.
let constrained = .< fun (r : r) -> let s @ gc = r.any in s >.
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type cell['a, 'm0 : mode] = { v : ptr['a @@ 'm0] }
    type r = { owned : string @@ gc; any : string }
    val code : <cell[string, external] -> unit>{..'a}
    val fields : <r @ 'm0 -> string @ 'm1>{..'a} with stage('m0) <= stage('m1) and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val constrained : <r @ 'm0 -> string @ 'm1>{..'a} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= gc and gc <= 'm1
    |}]
;;

(* A mode argument's variable has no locality: relating others through it
   plainly mustn't relate their localities. *)
let%expect_test "printing: through a mode argument's variable" =
  round_trip
    {|
type box['p : mode] = { v : string @@ 'p }
let f x b = let c = match 0 with | 0 -> b | _ -> {v = x} in b.v
|};
  [%expect
    {|
    type box['m0 : mode] = { v : string @@ 'm0 }
    val f : string @ 'm0 -> (box['m3] @ 'm2 -> string @ 'm4) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and stage('m2) <= stage('m4) and release(locality('m2)) <= locality('m4) and portable <= 'm3 and 'm3 <= 'm4 and gc <= 'm1
    |}];
  check
    {|
type box['p : mode] = { v : string @@ 'p }
let f x b = let c = match 0 with | 0 -> b | _ -> {v = x} in b.v
let gb : box[gc] = { v = "a" }
let f_copy : string @ 'm0 -> box['m3] @ 'm2 -> string @ 'm4 = f
let use (x @ local) = let r @ global = f x gb in r
|};
  [%expect
    {|
    type box['m0 : mode] = { v : string @@ 'm0 }
    val f : string @ 'm0 -> (box['m3] @ 'm2 -> string @ 'm4) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and stage('m2) <= stage('m4) and release(locality('m2)) <= locality('m4) and portable <= 'm3 and 'm3 <= 'm4 and gc <= 'm1
    val gb : box[gc]
    val f_copy : string @ 'm0 -> (box['m3] @ 'm2 -> string @ 'm4) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and stage('m2) <= stage('m4) and release(locality('m2)) <= locality('m4) and portable <= 'm3 and 'm3 <= 'm4 and gc <= 'm1
    val use : string @ 'm0 -> string @ 'm1 with local <= 'm0 and 'm0 <= gc and nonportable gc <= 'm1
    |}]
;;

(* A side fixed by a declaration reads back, in an annotation, as the atoms
   written: every axis is. *)
let%expect_test "printing: a declared arrow's fixed sides" =
  round_trip {|
type r = { f : string -> string }
let get_f (r : r) = r.f
|};
  [%expect
    {|
    type r = { f : string -> string }
    val get_f : r @ 'm0 -> (string @ nonportable gc global -> string @ nonportable gc global) @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}];
  check
    {|
type r = { f : string -> string }
let get_f : r -> (string @ nonportable gc global -> string @ nonportable gc global) = fun r -> r.f
let use (x @ local) = get_f {f = fun s -> s} x
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:4:13-18), then x is passed to the function (at t.stag:4:22-46), then x is used (at t.stag:4:45-46), then it is passed to a function (at t.stag:4:22-46), but must be at most global, because it is annotated nonportable gc global (at t.stag:3:27-48), then the function types are unified |}]
;;

let%expect_test "printing: a variable's relation to itself" =
  round_trip
    {|
let diverge (u : unit) = diverge u
let s : 'a @ 'm -> 'a @ 'n with release(locality('m)) <= locality('m) = fun x -> diverge ()
|};
  [%expect
    {|
    val diverge : unit @ 'm0 -> 'a @ global with portable <= 'm0
    val s : 'a @ 'm0 -> 'a with release(locality('m0)) <= locality('m0)
    |}]
;;

let%expect_test "a variable fixed on some axes keeps its name where others are \
                 free"
  =
  (* [const] fixes stage, and so provenance ([static]); as only a mode argument
     it has no locality, so it is fixed and prints as atoms. *)
  round_trip
    {|
let diverge (u : unit) = diverge u
let s : ptr['a @@ 'm1] -> 'a with 'm1 <= const = fun x -> diverge ()
|};
  [%expect
    {|
    val diverge : unit @ 'm0 -> 'a @ global with portable <= 'm0
    val s : ptr['a @@ const static] -> 'a
    |}];
  (* Also an arrow's side, its locality is free: one name in both places. *)
  round_trip
    {|
let diverge (u : unit) = diverge u
let s : 'a @ 'm1 -> ptr['a @@ 'm1] -> 'a with 'm1 <= const = fun x -> diverge ()
|};
  [%expect
    {|
    val diverge : unit @ 'm0 -> 'a @ global with portable <= 'm0
    val s : 'a @ 'm0 -> ptr['a @@ 'm0] -> 'a with 'm0 <= const
    |}]
;;

let%expect_test "a variable only of mode arguments has no locality" =
  check
    {|
type box['p : mode] = { v : string @@ 'p }
let diverge (u : unit) = diverge u
let s : 'a @ 'm0 -> box['m3] @ 'm2 -> 'a @ 'm4
  with release(locality('m0)) <= locality('m3) and 'm3 <= 'm4 = fun x -> diverge ()
|};
  [%expect
    {| This mode is invalid: 'm3 is only a mode argument, which has no locality |}];
  check
    {|
type box['p : mode] = { v : string @@ 'p }
let s : box['m3] -> unit with 'm3 <= local = fun x -> ()
|};
  [%expect
    {| This mode is invalid: 'm3 is only a mode argument, which has no locality |}];
  (* A plain relation relates the axes it has: stage and provenance. *)
  round_trip
    {|
type box['p : mode] = { v : string @@ 'p }
let diverge (u : unit) = diverge u
let s : box['m3] @ 'm2 -> 'a @ 'm0 -> 'a @ 'm4
  with 'm0 <= 'm3 and 'm3 <= 'm4 and locality('m0) <= locality('m4) = fun x -> diverge ()
|};
  [%expect
    {|
    type box['m0 : mode] = { v : string @@ 'm0 }
    val diverge : unit @ 'm0 -> 'a @ global with portable <= 'm0
    val s : box['m0] -> 'a @ 'm1 -> 'a @ 'm2 with 'm0 <= 'm2 and 'm1 <= 'm0 and 'm1 <= 'm2
    |}]
;;
