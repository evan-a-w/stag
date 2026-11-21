open! Import
open Syntax
open Helpers

let type_decls =
  {|type nat = Z | S of ptr[nat]
type list['a] = Nil | Cons of ptr['a * list['a]]
type option['a] = None | Some of 'a
type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
|}
;;

(* [let f (x : ty) = x] has type [ty -> ty], printed as [ty] is. A context entry
   is marked: an unmarked one's assignment variable is shared by both sides, so
   printed. *)
let%expect_test "printed types parse back" =
  List.map
    [ "{`x : 'a; ..'b} -> 'a"
    ; "nat -> <'a>{..'b} -> <list['a]>{..'c}"
    ; "[`A of int; ..'a] -> ([..'a] -> int) -> int"
    ; "[`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a"
    ; "option[[`A; ..'a]] -> int"
    ; "[`None | `Some of option[int]] -> int"
    ; "{`x : [`A; ..'a]} -> int"
    ; "<{`y : 'a}>{y : immut 'a; ..'b}"
    ; "<'a>{r : mut {`x : 'a; ..'b}; ..'c}"
    ; "<'a * {..'b}>{r : immut {`x : 'a; ..'b}; ..'c}"
    ; "stmt[{`x : int; ..'a}]"
    ; "stmt[{..'a}] -> <unit>{..'a}"
    ; "{`A : int} * [`A of int]"
    ; "{`a : {`b : 'a; ..'b}; ..'c} -> 'a"
    ; "('a * 'b) * 'c"
    ; "'a * ('b -> 'c)"
    ; "() * {} * []"
    ; "<<'a>{..'b}>{..'c}"
    ]
    ~f:(fun ty ->
      Source_helpers.check (type_decls ^ sprintf "let f (x : %s) = x" ty);
      let printed = [%expect.output] |> String.split_lines |> List.last_exn in
      let parens =
        match Parser.type_expr ~file:Source_helpers.file ty with
        | Ok { txt = Arrow _; _ } -> sprintf "(%s)" ty
        | _ -> ty
      in
      let expected = sprintf "val f : %s -> %s" parens ty in
      if String.equal printed expected
      then ty
      else
        Sexp.to_string_hum
          [%message "mismatch" (printed : string) (expected : string)])
  |> List.iter ~f:print_endline;
  [%expect
    {|
    {`x : 'a; ..'b} -> 'a
    nat -> <'a>{..'b} -> <list['a]>{..'c}
    [`A of int; ..'a] -> ([..'a] -> int) -> int
    [`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a
    option[[`A; ..'a]] -> int
    [`None | `Some of option[int]] -> int
    {`x : [`A; ..'a]} -> int
    <{`y : 'a}>{y : immut 'a; ..'b}
    <'a>{r : mut {`x : 'a; ..'b}; ..'c}
    <'a * {..'b}>{r : immut {`x : 'a; ..'b}; ..'c}
    stmt[{`x : int; ..'a}]
    stmt[{..'a}] -> <unit>{..'a}
    {`A : int} * [`A of int]
    {`a : {`b : 'a; ..'b}; ..'c} -> 'a
    ('a * 'b) * 'c
    'a * ('b -> 'c)
    () * {} * []
    <<'a>{..'b}>{..'c}
    |}]
;;

let%expect_test "printed declarations parse back" =
  let source =
    type_decls
    ^ {|type point = {`x : int; `y : int}
type with_x[{..'a}] = {`x : int; ..'a}
type holder = { p : {`x : int; `y : int} }
type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
type tree = [`Leaf of int | `Node of list[tree]]
type pair = [`A of ptr[pair]] * int
type rows[{..'a}] = {`x : ptr[rows[{..'a}]]; ..'a}
type e = |
type abstract
|}
  in
  Source_helpers.check source;
  let printed = [%expect.output] in
  print_string printed;
  print_s [%message (String.equal printed source : bool)];
  [%expect
    {|
    type nat = Z | S of ptr[nat]
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type option['a] = None | Some of 'a
    type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
    type point = {`x : int; `y : int}
    type with_x[{..'a}] = {`x : int; ..'a}
    type holder = { p : {`x : int; `y : int} }
    type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
    type tree = [`Leaf of int | `Node of list[tree]]
    type pair = [`A of ptr[pair]] * int
    type rows[{..'a}] = {`x : ptr[rows[{..'a}]]; ..'a}
    type e = |
    type abstract
    ("String.equal printed source" true)
    |}]
;;

let%expect_test "the staged power example" =
  let nat =
    Decl.variant
      "nat"
      [ "Z", []; "S", [ Ty.con "ptr" ~args:[ Ty (Ty.con "nat") ] ] ]
  in
  let list =
    Decl.variant
      "list"
      ~params:[ Decl.ty "a" ]
      [ "Nil", []
      ; "Cons", [ Ty.var "a"; Ty.con "list" ~args:[ Ty (Ty.var "a") ] ]
      ]
  in
  let power =
    top
      "power"
      [ "n"; "c" ]
      (match_
         (var "n")
         [ Pat.construct "Z", quote (construct "Nil")
         ; ( Pat.construct "S" ~args:[ Pat.deref (Pat.var "m") ]
           , quote
               (construct
                  "Cons"
                  ~args:
                    [ splice (var "c")
                    ; splice (app (var "power") [ var "m"; var "c" ])
                    ]) )
         ])
  in
  let program =
    Source_helpers.parse_exn
      {|
      type nat = Z | S of ptr[nat]
      type list['a] = Nil | Cons of ptr['a * list['a]]

      (* [power n c] is a list of [n] copies of [c]. *)
      let power n c =
        match n with
        | Z -> .< Nil >.
        | S * m -> .< Cons (Ptr.make ( (.~c, .~(power m c)))) >.

      let three = S (Ptr.make (S (Ptr.make (S (Ptr.make Z)))))
      let cube x = .~(power three .<x>.)
      let used = cube 7
      |}
  in
  print_s
    [%message (Program.equal (List.take program 3) [ nat; list; power ] : bool)];
  Option.iter (Source_helpers.typecheck_program program) ~f:(fun t ->
    Source_helpers.print_signature t;
    Source_helpers.eval_and_print_values t);
  [%expect
    {|
    ("Program.equal (List.take program 3) [nat; list; power]" false)
    type nat = Z | S of ptr[nat]
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val power : nat -> <'a>{..'b} -> <list['a]>{..'c} where ..'b <= {..'c}
    val three : nat
    val cube : 'a -> list['a]
    val used : list[int]
    (power <fun>)
    (three (S <ptr>))
    (cube <fun>)
    (used (Cons <ptr>))
    |}]
;;

let%expect_test "polymorphic records, variants and modules" =
  Source_helpers.check
    {|
          let lit = {`x = 1; `y = true}
          let empty = {}
          let get_x r = r.`x
          let extend r = {`z = 1 | r}
          let restrict r = r \ `x
          let update r = {r with `x = true}
          let nested r = r.`a.`b
          let rest r = match r with {`a = a; `b = _; ..others} -> (a, others)
          let open_ r = match r with {`a = a; ..} -> a
          let tags x = match x with `A n -> n | `B -> 0 | `C (a, b) -> add a b
          let sequence x = id x; eq x 1

          type point = {x : int; y : int}
          let origin = {x = 0; y = 0}
          let get_y p = p.y

          module M = struct
            type t = A | B of int * bool
            let make n = B (n, true)
            let first t = match t with A -> 0 | B (n, _) -> n
            module N = struct let deep = first (make 1) end
          end
          let deep = M.N.deep
          let annotated (t : M.t) = t
          let quoted (c : <int>{..'r}) = .< (.~c.{}, x) >.
          |};
  [%expect
    {|
    val lit : {`x : int; `y : bool}
    val empty : {}
    val get_x : {`x : 'a; ..'b} -> 'a
    val extend : {..'a} -> {`z : int; ..'a}
    val restrict : {`x : 'a; ..'b} -> {..'b}
    val update : {`x : 'a; ..'b} -> {`x : bool; ..'b}
    val nested : {`a : {`b : 'a; ..'b}; ..'c} -> 'a
    val rest : {`a : 'a; `b : 'b; ..'c} -> 'a * {..'c}
    val open_ : {`a : 'a; ..'b} -> 'a
    val tags : [`A of int | `B | `C of int * int] -> int
    val sequence : int -> bool
    type point = { x : int; y : int }
    val origin : point
    val get_y : point -> int
    module M : sig
      type t = A | B of int * bool
      val make : int -> t
      val first : t -> int
      module N : sig
        val deep : int
      end
    end
    val deep : int
    val annotated : M.t -> M.t
    val quoted : <int>{} -> <int * 'a>{x : 'a; ..'b}
    |}]
;;
