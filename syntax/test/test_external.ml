open! Import
open Syntax

let file = "t.stag"

let prog source =
  match Parser.program ~file source with
  | Ok program ->
    print_s (Round_trip.strip_locs [%sexp (program : Program.t)]);
    Round_trip.program program
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let print ?(width = 80) source =
  match Parser.program ~file source with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok program ->
    Round_trip.program program;
    print_string (Printer.program ~width program)
;;

let%expect_test "externals" =
  prog {|external labs : int -> int = "labs"|};
  prog {|external f : int @ portable -> unit -> int @ portable = "f"|};
  prog {|external g : string = "g"|};
  [%expect
    {|
    ((External (name labs) (ty (Arrow (Con int ()) (Con int ()))) (symbol labs)))
    ((External (name f)
      (ty
       (Arrow (Moded (Con int ()) (Atoms (Portable)))
        (Arrow (Con unit ()) (Moded (Con int ()) (Atoms (Portable))))))
      (symbol f)))
    ((External (name g) (ty (Con string ())) (symbol g)))
    |}]
;;

let%expect_test "malformed externals" =
  prog {|external Labs : int -> int = "labs"|};
  prog {|external labs int -> int = "labs"|};
  prog {|external labs : int -> int "labs"|};
  prog {|external labs : int -> int = labs|};
  prog {|external labs : int -> int = "labs" let x = 1|};
  prog {|let external = 1|};
  [%expect
    {|
    t.stag:1:9-13: expected the external's name, found `Labs`
    t.stag:1:14-17: expected `:`, found `int`
    t.stag:1:27-33: expected `=`, found `"labs"`
    t.stag:1:29-33: expected the external's C symbol, a string, found `labs`
    ((External (name labs) (ty (Arrow (Con int ()) (Con int ()))) (symbol labs))
     (Let x () () (Lit (Int 1))))
    t.stag:1:4-12: expected a name to bind, found `external`
    |}]
;;

let%expect_test "printing externals" =
  print
    {|
external labs : int -> int = "labs"
external strtol : string -> int -> int -> int = "strtol"
module M = struct external getpid : unit -> int = "getpid" end
|};
  print
    ~width:30
    {|external a_long_name : string -> int -> int -> bool -> char = "a_long_symbol_name"|};
  [%expect
    {|
    external labs : int -> int = "labs"
    external strtol : string -> int -> int -> int = "strtol"

    module M = struct
      external getpid : unit -> int = "getpid"
    end
    external a_long_name : string ->
        int ->
        int ->
        bool ->
        char =
      "a_long_symbol_name"
    |}]
;;

(* [external] is also a mode: after a mode it is a declaration only when it
   starts one. *)
let%expect_test "the external mode next to an external" =
  print
    {|
external f : int -> int @ external = "f"
external g : int -> int @ gc = "g"
type t = int -> int @ external
external local : unit -> int = "h"
let x : int @ global external = 1
|};
  prog {|external f : int -> int @ gc external = "f"|};
  [%expect
    {|
    external f : int -> int @ external = "f"
    external g : int -> int @ gc = "g"
    type t = int -> int @ external
    external local : unit -> int = "h"

    let x : int @ global external = 1
    t.stag:1:29-37: `gc` and `external` are both provenance modes
    |}]
;;
