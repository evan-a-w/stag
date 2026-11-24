open! Import

let show source =
  match Mode_helpers.signature source with
  | Ok s -> print_string s
  | Error errors -> List.iter errors ~f:print_endline
;;

let%expect_test "prims only read their arguments, so take any" =
  show
    {|
let ext (x : string) = let y @ external = x in y
let len (s : string @ local) = String.length s
let len_ext u = String.length (ext "a")
let sub (s : string @ local) = String.sub s 0 1
let print (s : string @ local) = print_endline s
let concat (l : list[string] @ local) = String.concat ", " l
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val len : string @ local -> int
    val len_ext : 'a -> int
    val sub : string @ local -> string @ 'm0 with nonportable gc <= 'm0
    val print : string @ local -> unit
    val concat : list[string] @ local -> string @ 'm0 with nonportable gc <= 'm0
    |}]
;;

let%expect_test "a prim's result is a new value" =
  show {|
let fresh (s : string @ local) = String.append s s

|};
  [%expect
    {| val fresh : string @ local -> string @ 'm0 with nonportable gc <= 'm0 |}]
;;

let%expect_test "a partial application holds the arguments so far" =
  show
    {|
let app = String.append
let part (s : string) = String.append s
let part_local (s : string @ local) = String.append s
let int_part (n : int @ local) = Int.add n
|};
  [%expect
    {|
    val app : string @ 'm0 -> (string -> string @ nonportable gc global) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val part : string @ 'm0 -> (string -> string @ nonportable gc global) @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val part_local : string @ 'm0 -> (string -> string @ nonportable gc global) @ 'm1 with local <= 'm0 and 'm0 <= 'm1 and gc <= 'm1
    val int_part : int @ local -> (int -> int @ nonportable gc global) @ 'm0 with gc <= 'm0
    |}];
  show {|
let returned (s : string @ local) = String.append s
|};
  [%expect
    {| val returned : string @ 'm0 -> (string -> string @ nonportable gc global) @ 'm1 with local <= 'm0 and 'm0 <= 'm1 and gc <= 'm1 |}];
  show
    {|
let stored = Ptr.make (String.append "a")
let store (s : string @ local) = stored.* <- String.append s
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:3:24-29), then s is passed to the function (at t.stag:3:33-60), then s is used (at t.stag:3:59-60), then it is passed to a function (at t.stag:3:45-60), then the function types are unified, then a partial application holds it, then the function types are unified, then the function returns it (at t.stag:3:45-60), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:45-60) |}]
;;

let%expect_test "core's functions take local arguments" =
  show
    {|
let is_empty (s : string @ local) = Core.String.is_empty s
let equal (s : string @ local) (t : string @ local) = Core.String.equal s t
let upper (s : string @ local) = Core.String.uppercase s
let iter (s : string @ local) = Core.String.iter (fun c -> print_char c) s
let count (s : string @ local) = Core.String.fold_left (fun n c -> Int.add n 1) 0 s
let lengths (l : list[string] @ local) = Core.List.map Core.String.length l
let set (n : ptr[int] @ local) = Core.List.iter (fun x -> n.* <- x) [1; 2]
|};
  [%expect
    {|
    val is_empty : string @ local -> bool
    val equal : string @ 'm0 -> (string @ local -> bool) @ 'm1 with local <= 'm0 and 'm0 <= 'm1 and gc <= 'm1
    val upper : string @ local -> string @ 'm0 with nonportable gc <= 'm0
    val iter : string @ local -> unit
    val count : string @ local -> int
    val lengths : list[string] @ local -> list[int] @ 'm0 with nonportable gc <= 'm0
    val set : ptr[int] @ local -> unit
    |}]
;;
