open! Import
open Source_helpers

let signature source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    Ok
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    Error (List.map errors ~f:Typecheck_error.message)
;;

let check source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

(* Annotates a copy of each value with its printed type, which must read back as
   the same type. *)
let round_trip source =
  let printed = signature source |> Result.ok |> Option.value_exn in
  let copies =
    String.split_lines printed
    |> List.filter_map ~f:(fun line ->
      String.chop_prefix line ~prefix:"val "
      |> Option.map ~f:(fun rest ->
        let name, ty = String.lsplit2_exn rest ~on:':' in
        let name = String.strip name in
        sprintf "let %s_copy :%s = %s" name ty name))
  in
  print_string printed;
  match signature (String.concat ~sep:"\n" (source :: copies)) with
  | Error errors -> List.iter errors ~f:print_endline
  | Ok signature' ->
    let lines = String.split_lines signature' in
    let find name =
      List.find_map lines ~f:(fun line ->
        String.chop_prefix line ~prefix:("val " ^ name ^ " :"))
    in
    List.iter copies ~f:(fun copy ->
      let name = String.chop_prefix_exn copy ~prefix:"let " in
      let name = String.prefix name (String.index_exn name ' ') in
      let original = String.chop_suffix_exn name ~suffix:"_copy" in
      if [%equal: string option] (find name) (find original)
      then ()
      else
        print_s
          [%message
            "differs"
              (name : string)
              (find original : string option)
              (find name : string option)])
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
    val g : r @ 'm0 -> ptr[ptr[int] @@ 'm1] @ nonportable with 'm0 <= 'm1
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
    val make : 'a -> r @ nonportable
    val p : 'a -> (string -> string) @ 'm0 with gc <= 'm0
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    type r = { owned : string @@ gc; borrowed : string }
    val make : 'a -> r @ 'm0 with external <= 'm0
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    type t = A of string @@ portable gc global | B of string
    val a : t @ local -> ptr[string @@ 'm0] @ nonportable with gc <= 'm0
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    val read : ptr[string] @ 'm0 -> string @ nonportable with portable <= 'm0 and external <= 'm0
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
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
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:31-38), but must be at most gc, because it is stored into a pointer's cell, which holds only what fits its [@@] ([ptr[t]]: nonportable gc), and the modalities of the fields stored into (at t.stag:6:31-38) |}];
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
    val read_owned : 'a -> string @ nonportable
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
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
    {| This is nonportable, because it reads through a pointer (at t.stag:3:32-35), then it is bound by an annotated let (at t.stag:3:21-29), but must be at most portable, because it is annotated portable (at t.stag:3:21-29) |}];
  check
    {|
let p : ptr[(string -> string) @@ portable] = Ptr.make (fun x -> x)
let store u = p.* <- (let n = Int.to_string 3 in fun x -> String.append x n)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:30-45), then n is bound (at t.stag:3:21-76), but must be at most portable, because it is stored into a pointer's cell, which holds only what fits its [@@] ([ptr[t]]: nonportable gc), and the modalities of the fields stored into (at t.stag:3:21-76) |}];
  check
    {|
let p : ptr[string -> string] = Ptr.make (fun x -> x)
let read u = let f @ portable = p.* in f
|};
  [%expect
    {| This is nonportable, because it reads through a pointer (at t.stag:3:32-35), then it is bound by an annotated let (at t.stag:3:21-29), but must be at most portable, because it is annotated portable (at t.stag:3:21-29) |}];
  check {|
let f (p : ptr[int @@ local]) = p
|};
  [%expect
    {| The modality of a pointer's type argument is on stage and provenance ([const], [portable], [nonportable], [static], [gc], [external]) or a mode variable, as in [ptr[string @@ external]]: a pointer's cell is always [global] |}];
  check {|
let f (p : ptr[int @@ portable external]) = p
|};
  [%expect
    {| val f : ptr[int @@ portable external] @ 'm0 -> ptr[int @@ portable external] @ 'm1 with 'm0 <= 'm1 |}]
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
    val get : cell['a, 'm0] -> 'a @ 'm1 with 'm0 <= 'm1 and nonportable <= 'm1
    val set : cell['a, 'm1] @ 'm0 -> ('a @ 'm3 -> unit) @ 'm2 with 'm0 <= 'm2 and 'm3 <= 'm1 and gc <= 'm2
    val ext_cell : cell[string, external]
    val portable_cell : cell[string, portable]
    val gc_cell : cell[string, gc]
    val tag : tagged['m0] -> int
    val payload : tagged['m0] -> string @ 'm1 with 'm0 <= 'm1
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
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
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:21-28), then it is stored into a pointer's cell, which holds only what fits its [@@] ([ptr[t]]: nonportable gc), and the modalities of the fields stored into (at t.stag:6:21-28), but must be at most gc, because the types are unified |}];
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
    {|
    type cell['m0 : mode] = { v : ptr[int @@ 'm0] }
    val f : cell[local] @ 'm0 -> cell[local] @ 'm1 with 'm0 <= 'm1
    |}]
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
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    type cell['a, 'm0 : mode] = { v : ptr['a @@ 'm0] }
    type r = { owned : string @@ gc; any : string }
    val code : <cell[string, external] -> unit>{..'a}
    val fields : <r @ 'm0 -> string @ 'm1>{..'a} with 'm0 <= 'm1 and gc <= 'm1
    val constrained : <r @ 'm0 -> string @ 'm1>{..'a} with 'm0 <= 'm1 and 'm0 <= gc and gc <= 'm1
    |}]
;;
