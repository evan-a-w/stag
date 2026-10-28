open! Import

(* Prints [source] formatted, after checking it keeps its syntax and comments
   and is idempotent, at the widths given. *)
let test ?(width = 80) source =
  Round_trip.source ~widths:[ width; 80; 40; 10 ] source;
  match Formatter.format ~width ~file:"t.stag" source with
  | Ok formatted -> print_string formatted
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let%expect_test "comments before toplevels" =
  test
    {|(* header *)

(* second header,
   on two lines *)
let x = 1
(* before y *)
let y = 2


(* after two blank lines *)

let z = 3
(* end of file *)
|};
  [%expect
    {|
    (* header *)

    (* second header,
       on two lines *)
    let x = 1
    (* before y *)
    let y = 2

    (* after two blank lines *)

    let z = 3
    (* end of file *)
    |}]
;;

let%expect_test "only comments" =
  test "(* a *)\n\n(* b *)   \n";
  [%expect
    {|
    (* a *)

    (* b *)
    |}];
  test "(* a *) (* b *)";
  [%expect
    {|
    (* a *)
    (* b *)
    |}]
;;

let%expect_test "blank lines between one line toplevels are kept" =
  test
    {|let a = 1
let b = 2

let c = 3
let f x =
  match x with A -> a | B -> b
let d = 4
|};
  [%expect
    {|
    let a = 1
    let b = 2

    let c = 3

    let f x =
      match x with
      | A -> a
      | B -> b

    let d = 4
    |}]
;;

let%expect_test "trailing comments" =
  test
    {|let x = 1 (* one *)
let y = (2, 3) (* a tuple *) (* and another *)
type t =
  | A (* the a *)
  | B of int (* the b *)
let z = 4
|};
  [%expect
    {|
    let x = 1 (* one *)
    let y = (2, 3) (* a tuple *) (* and another *)

    type t =
      | A (* the a *)
      | B of int (* the b *)

    let z = 4
    |}]
;;

let%expect_test "a trailing comment breaks the line after it" =
  test
    {|let x = f a (* a *)
  b
|};
  [%expect
    {|
    let x =
      f
        a (* a *)
        b
    |}]
;;

let%expect_test "trailing comments stay after separators" =
  test
    {|let f x =
  a x (* after a *)
  ; b x (* after b *)
  ; c
let r = (1 (* one *)
  , 2)
|};
  [%expect
    {|
    let f x =
      a x; (* after a *)
      b x; (* after b *)
      c

    let r =
      (1, (* one *)
       2)
    |}]
;;

let%expect_test "comments in matches" =
  test
    {|let f a b = (* after = *)
  match a with (* after with *)
  (* before A *)
  | A -> b (* trailing A *)
  | B (* in B *) -> (* after -> *) 2
  (* before C *)

  | C -> 3
|};
  [%expect
    {|
    let f a b =
      (* after = *)
      match a with (* after with *)
      (* before A *)
      | A -> b (* trailing A *)
      | B (* in B *) -> (* after -> *) 2
      (* before C *)

      | C -> 3
    |}]
;;

let%expect_test "comments in records" =
  test
    {|let r = { a = 1; (* after a *)
  (* before b *)
  b = (* in b *) 2;
  c = 3 (* after c *) }
type t = {
  x : int; (* the x *)
  (* before y *)
  y : string;
}
let f r = match r with { a = x; (* a *) b = y } -> x
|};
  [%expect
    {|
    let r = {
      a = 1; (* after a *)
      (* before b *)
      b = (* in b *) 2;
      c = 3; (* after c *)
    }

    type t = {
      x : int; (* the x *)
      (* before y *)
      y : string;
    }

    let f r = match r with {a = x; (* a *) b = y} -> x
    |}]
;;

let%expect_test "comments in modules" =
  test
    {|module M = struct (* after struct *)
  (* first *)
  let x = 1
  module N = struct
    let y = 2 (* y *)
    (* last of N *)
  end
  (* last *)
end
module E = struct (* empty *) end
module F = struct
  (* only a comment *)
end
|};
  [%expect
    {|
    module M = struct (* after struct *)
      (* first *)
      let x = 1

      module N = struct
        let y = 2 (* y *)
        (* last of N *)
      end
      (* last *)
    end

    module E = struct (* empty *) end

    module F = struct
      (* only a comment *)
    end
    |}]
;;

let%expect_test "comments in sequences and lets" =
  test
    {|let g x =
  foo x; (* after foo *)
  (* before bar *)
  bar x;
  let y = (* rhs *) x in (* after in *)
  (* body *)
  baz y
|};
  [%expect
    {|
    let g x =
      foo x; (* after foo *)
      (* before bar *)
      bar x;
      let y = (* rhs *) x in (* after in *)
      (* body *)
      baz y
    |}]
;;

let%expect_test "comments inside expressions" =
  test
    {|let x = f (* arg *) a (b (* in parens *)) (* after *)
let y = Int.add (* a *) 1 (* b *) 2
let z = .< (* quoted *) x >. (* code *)
let w = (a, (* second *) b, c (* third *))
let t : (* type *) int = 1
let p (* param *) q = q
|};
  [%expect
    {|
    let x = f (* arg *) a b (* in parens *) (* after *)
    let y = Int.add (* a *) 1 (* b *) 2
    let z = .< (* quoted *) x >. (* code *)
    let w = (a, (* second *) b, c (* third *))
    let t : (* type *) int = 1
    let p (* param *) q = q
    |}]
;;

let%expect_test "comments where the printer drops tokens" =
  test
    {|let x = ((* in redundant parens *) a)
let y = match x with | (* first bar *) A -> a
|};
  [%expect
    {|
    let x = (* in redundant parens *) a
    let y = match x with (* first bar *) A -> a
    |}]
;;

let%expect_test "nested comments and comment-like strings" =
  test
    {x|(* outer (* inner (* innermost *) *) "*)" '"' *)
let s = "(* not a comment *)" (* "a string *) in a comment" *)
let c = '"' (* after a quote char *)
let e = [%expect {| (* in an expect *) |}]
|x};
  [%expect
    {x|
    (* outer (* inner (* innermost *) *) "*)" '"' *)
    let s = "(* not a comment *)" (* "a string *) in a comment" *)
    let c = '"' (* after a quote char *)
    let e = [%expect {| (* in an expect *) |}]
    |x}]
;;

let%expect_test "doc comments" =
  test
    {|(** The module. *)

(** [f x] is [x]. *)
let f x = x

(** A type,
    documented. *)
type t = A (** the a *) | B
|};
  [%expect
    {|
    (** The module. *)

    (** [f x] is [x]. *)
    let f x = x

    (** A type,
        documented. *)
    type t = A (** the a *) | B
    |}]
;;

let%expect_test "multi-line comments keep their text" =
  test
    {|let f x =
      (* indented
         over lines *)
  x
|};
  [%expect
    {|
    let f x =
      (* indented
         over lines *)
      x
    |}]
;;

let%expect_test "narrow widths" =
  test
    ~width:20
    {|let f x = (* f *) g (* g *) x (* x *) (* y *) y
let r = { a = 1; (* a *) b = 2 }
|};
  [%expect
    {|
    let f x =
      (* f *) g
        (* g *) x
        (* x *) (* y *) y

    let r = {
      a = 1;
      (* a *) b = 2;
    }
    |}]
;;

let%expect_test "comments before separators go after them" =
  test
    {|type t['a
(* own line *)
, 'b] = int
let x = (a
(* own line *)
, b)
let y = f (a (* inline *) ; b)
let z =
  let v = 1
  (* own line *)
  in
  v
|};
  [%expect
    {|
    type t['a,
           (* own line *)
           'b] =
      int

    let x =
      (a,
       (* own line *)
       b)
    let y = f (a; (* inline *) b)

    let z =
      let v = 1 in
      (* own line *)
      v
    |}]
;;

let%expect_test
    "comments followed on their last line by more comments then code are inline"
  =
  test
    {|let x = (a, (* one
   two *) (* three
   four *) b)
let y = (a, (* one
   two *) (* three *)
  b)
|};
  [%expect
    {|
    let x =
      (a,
       (* one
    two *) (* three
    four *) b)
    let y =
      (a, (* one
    two *) (* three *)
       b)
    |}]
;;

let%expect_test
    "multi-line comments move with their first line, as far as they can"
  =
  test
    {|let f x =
          (* moved left
             with its second line *)
          x
let g x = (* far right
   then left of the start *) x
let h x =
(* moved right
   with its second line *)
x
|};
  [%expect
    {|
    let f x =
      (* moved left
         with its second line *)
      x
    let g x =
      (* far right
    then left of the start *) x
    let h x =
      (* moved right
         with its second line *)
      x
    |}]
;;
