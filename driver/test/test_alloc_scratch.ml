open! Import
open Mode_helpers

let%expect_test "scratch" =
  allocations {|
let mk x = Box.make x
let mk2 (x : int) = Box.make x
let mk3 (x : int) = fun (y : int) -> Int.add x y
let mk4 (x : int) = let b = Box.make x in let u = Box.make b in b
let mk5 (x : int) = exclave_ Box.make x
let use_local (u : unit) = let b @ local = mk2 1 in 0
let leak (x @ local) = Box.make x
let build (n : int) = match n with 0 -> Nil | _ -> exclave_ Cons (Box.make (n, build (Int.sub n 1)))
let st (u : unit) = let b = stack_ (Box.make 1) in 0
|};
  [%expect {||}]
;;
