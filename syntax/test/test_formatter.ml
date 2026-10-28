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
  [%expect {| (* a *) (* b *) |}]
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

let%expect_test "a space after a line is not measured" =
  test
    {|let x =
  iter print_item (map (Int.mul 10) (Cons (1, Cons (2, Cons (3 (* c *), Nil)))))
|};
  [%expect
    {|
    let x =
      iter print_item (map (Int.mul 10) (Cons (1, Cons (2, Cons (3, (* c *) Nil)))))
    |}]
;;

let%expect_test
    "an inline comment whose next token is dropped stays before what follows"
  =
  test
    {|let rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr = ((* in redundant paren *) (xxxxxxxxxxxxxxxxxxxxxx))
let s = .~ (* b *) (q)
let t = (x (* c *)).y
|};
  [%expect
    {|
    let rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr =
      (* in redundant paren *) xxxxxxxxxxxxxxxxxxxxxx
    let s = .~ (* b *) q
    let t = x (* c *) .y
    |}]
;;

let%expect_test "a trailing comment goes after closers the printer adds" =
  test
    {|let f x = match x with a, b -> a, b (* c *)
let g x = match x with a, b -> a, b (* c *)
  | _ -> x
|};
  [%expect
    {|
    let f x = match x with a, b -> (a, b) (* c *)

    let g x =
      match x with
      | a, b -> (a, b) (* c *)
      | _ -> x
    |}]
;;

let%expect_test "own line comments on one line stay on one" =
  test
    {|(* a *) (* b *)
let x = 1
(* c *) (* d
   e *) (* f *)

let y =
  (* g *) (* h *)
  2
|};
  [%expect
    {|
    (* a *) (* b *)
    let x = 1
    (* c *) (* d
       e *) (* f *)

    let y =
      (* g *) (* h *)
      2
    |}]
;;

let%expect_test "no space between an opener and a comment" =
  test
    {|let x = ( (* c *) h, 2)
let y = .~f.{ (* s *) x = y (* t *)}
let z = { (* r *) a = 1 }
|};
  [%expect
    {|
    let x = ((* c *) h, 2)
    let y = .~f.{(* s *) x = y (* t *)}
    let z = {(* r *) a = 1}
    |}]
;;

let%expect_test "comments' CRLFs become LFs, as the code's do" =
  (match
     Formatter.format
       ~file:"t.stag"
       "let x =\r\n  (* a\r\n  b *)\r\n  1\r\nlet y = (* c *) 2\r\n"
   with
   | Ok formatted -> print_endline (String.escaped formatted)
   | Error error -> print_endline (Parser.Error.to_string error));
  [%expect {| let x =\n  (* a\n  b *)\n  1\nlet y = (* c *) 2\n |}]
;;

let%expect_test "comments with lines indented by tabs are not moved" =
  test "let f x =\n        (* a\n\tb\n  \tc *)\n        x\n";
  [%expect
    {|
    let f x =
      (* a
    b
      c *)
      x
    |}]
;;

let%expect_test "long files format quickly" =
  let check source =
    let start = Time_ns.now () in
    let format source =
      Formatter.format ~file:"t.stag" source |> Result.ok |> Option.value_exn
    in
    let formatted = format source in
    let reformatted = format formatted in
    let elapsed = Time_ns.diff (Time_ns.now ()) start in
    let lines = String.split_lines formatted in
    print_s
      [%message
        ""
          ~lines:(List.length lines : int)
          ~idempotent:(String.equal formatted reformatted : bool)
          ~last_lines:(List.drop lines (List.length lines - 3) : string list)];
    if Time_ns.Span.(elapsed > of_sec 10.)
    then print_s [%message "too slow" (elapsed : Time_ns.Span.t)]
  in
  check
    (String.concat
       (List.init 5000 ~f:(fun i ->
          sprintf
            "let f%d x = match x with a, b -> a, b (* c%d *)\n\
             let g%d = (((%d)))\n"
            i
            i
            i
            i)));
  check
    (String.concat
       (List.init 5000 ~f:(fun i ->
          sprintf "let f%d x = Int.add (((x))) %d\n" i i)));
  (* One toplevel, so aligned as a whole. *)
  check
    ("let x = {\n"
     ^ String.concat
         (List.init 10000 ~f:(fun i ->
            sprintf "  `a%d = ((%d)); (* c%d *)\n" i i i))
     ^ "}\n");
  [%expect
    {|
    ((lines 10000) (idempotent true)
     (last_lines
      ("let g4998 = 4998" "let f4999 x = match x with a, b -> (a, b) (* c4999 *)"
       "let g4999 = 4999")))
    ((lines 5000) (idempotent true)
     (last_lines
      ("let f4997 x = Int.add x 4997" "let f4998 x = Int.add x 4998"
       "let f4999 x = Int.add x 4999")))
    ((lines 10002) (idempotent true)
     (last_lines
      ("  `a9998 = 9998; (* c9998 *)" "  `a9999 = 9999; (* c9999 *)" })))
    |}]
;;

let%expect_test "found by the messy random tests" =
  test
    {|type t = A of (* a *)(
(* own line *)
'b)
let x = (
(* b *)( (** c *)

))
type u = (('a (** d *)))

type v[{..'r},
       'a] =
  'a
let y = {`a = -4 (* e *).`f | z}
let w = ({x with `l = y} \ `a
(* f *)  ) \ `a
|};
  [%expect
    {|
    type t =
      | A of
             (* a *)
             (* own line *)
             'b

    let x =
      (
      (* b *) (** c *)

      )

    type u = 'a (** d *)

    type v[{..'r}, 'a] = 'a

    let y = {`a = (-4 (* e *)).`f | z}
    let w = ({x with `l = y} \ `a (* f *)) \ `a
    |}]
;;

let%expect_test "found by sweeping more random seeds" =
  test
    {|let x = {
  `a = 1;
  `b = 1

(* own line *)

(* then before a separator *);
}
let y = {
  `a = 1;
  `b = 1 (* inline *)
  (* before a separator *) ;
}
let z = {`a = (0
(* in redundant parens
   on its own line *) ) | `b}
let v = (0
(* f *) )
|};
  [%expect
    {|
    let x = {
      `a = 1;
      `b = 1;

      (* own line *)

      (* then before a separator *)
    }
    let y = {
      `a = 1;
      `b = 1; (* inline *) (* before a separator *)
    }
    let z = {
      `a = 0 (* in redundant parens
                on its own line *) |
        `b
    }
    let v = 0 (* f *)
    |}]
;;

let%expect_test "inline expansions" =
  test
    {|
include%inline .~(gen (type t))
(* first *)
type t = A
      let x =   1 (* trailing *)
  (* last *)
end%inline
module M = struct
  module%inline N = .~e
      let x = 1


      (* before y *)
      let y = 2
    (* before the end *)
  end%inline
  include%inline .~e   end%inline
end
|};
  [%expect
    {|
    include%inline .~(gen (type t))
      (* first *)
      type t = A

      let x = 1 (* trailing *)
      (* last *)
    end%inline

    module M = struct
      module%inline N = .~e
        let x = 1

        (* before y *)
        let y = 2
        (* before the end *)
      end%inline

      include%inline .~e
      end%inline
    end
    |}]
;;
