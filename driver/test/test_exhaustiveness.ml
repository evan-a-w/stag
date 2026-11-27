open! Import
open Source_helpers

(* The warnings and errors of [source], in source order, after the warnings are
   set by the specs [w]. *)
let check ?(w = []) source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let warnings =
    List.fold w ~init:Warning.Settings.default ~f:(fun settings spec ->
      Warning.Settings.apply
        settings
        (Result.ok_or_failwith (Warning.Spec.of_string spec)))
  in
  let typecheck = Typecheck.create ~stamps ~interpreter ~warnings () in
  let warnings, errors =
    match Typecheck.typecheck typecheck ~program:(program source) with
    | Ok checked -> checked.warnings, []
    | Error { errors; partial } -> partial.warnings, errors
  in
  let line kind (error : Typecheck_error.t) =
    error.loc, sprintf "%s: %s" kind (Typecheck_error.message error)
  in
  match
    List.map warnings ~f:(line "Warning") @ List.map errors ~f:(line "Error")
    |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Loc.compare a b)
  with
  | [] -> print_endline "ok"
  | lines ->
    List.iter lines ~f:(fun (loc, line) ->
      print_endline (Loc.to_string loc ^ ": " ^ line))
;;

let%expect_test "nominal variants, nested" =
  check
    {|
type u = X | Y of bool
type t = A | B of u | C of int * u
let f v = match v with A -> 0 | B X -> 1 | C (_, X) -> 2
let g v = match v with A -> 0 | B _ -> 1 | C (0, _) -> 2
let h v = match v with A -> 0 | B (Y true) -> 1 | B X -> 1 | C (_, _) -> 2
let ok v = match v with A | B _ -> 0 | C (_, (X | Y _)) -> 1
|};
  [%expect
    {|
    t.stag:4:10-56: Error: This match is not exhaustive [non-exhaustive]: it does not cover `B (Y _)`
    t.stag:5:10-56: Error: This match is not exhaustive [non-exhaustive]: it does not cover `C (1, _)`
    t.stag:6:10-74: Error: This match is not exhaustive [non-exhaustive]: it does not cover `B (Y false)`
    |}]
;;

let%expect_test "unused arms" =
  check
    {|
type t = A | B
let f v = match v with A -> 0 | B -> 1 | A -> 2
let g v = match v with _ -> 0 | B -> 1
let h v = match v with (A, _) -> 0 | (_, A) -> 1 | (A, A) -> 2 | _ -> 3
let i v = match v with A | B | A -> 0
|};
  [%expect
    {|
    t.stag:3:41-42: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:4:32-33: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:5:51-57: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    |}]
;;

let%expect_test "literals, tuples, records, deref" =
  check
    {|
type r = {x : int; y : bool}
let lit v = match v with 0 -> "zero" | 1 -> "one"
let str v = match v with "a" -> 0
let tup v = match v with (true, _) -> 0 | (_, false) -> 1
let rec_ v = match v with {x = 0; y = _} -> 0 | {x = _; y = true} -> 1
let deref v = match v with *None -> 0
let ok v = match v with (true, _) | (false, _) -> 0
|};
  [%expect
    {|
    t.stag:3:12-49: Error: This match is not exhaustive [non-exhaustive]: it does not cover `2`
    t.stag:4:12-33: Error: This match is not exhaustive [non-exhaustive]: it does not cover `""`
    t.stag:5:12-57: Error: This match is not exhaustive [non-exhaustive]: it does not cover `false, true`
    t.stag:6:13-70: Error: This match is not exhaustive [non-exhaustive]: it does not cover `{x = 1; y = false}`
    t.stag:7:14-37: Error: This match is not exhaustive [non-exhaustive]: it does not cover `*(Some _)`
    |}]
;;

let%expect_test "polymorphic variants: closed rows need every tag" =
  check
    {|
let f v = match v with `A -> 0 | `B n -> n
let g (v : [`A | `B of int]) = match v with `A -> 0
let h (v : [`A | `B of bool]) = match v with `A -> 0 | `B true -> 1
let exhaustive (v : [`A | `B of int]) = match v with `A -> 0 | `B n -> n
let extra_tag (v : [`A | `B of int]) = match v with `A -> 0 | `C -> 1
let redundant (v : [`A | `B of int]) = match v with `A -> 0 | `B _ -> 1 | `A -> 2
let redundant_wild (v : [`A | `B of int]) = match v with `A -> 0 | `B _ -> 1 | _ -> 2
let warn_only (v : [`A | `B of int]) = match v with `A -> 0
[@@warning "+non-exhaustive"]
let suppressed (v : [`A | `B of int]) = match v with `A -> 0
[@@warning "-non-exhaustive"]
let residual_missing (v : [`A | `B of int | `C]) =
  match v with
  | `A -> 0
  | other -> match other with `B n -> n
|};
  [%expect
    {|
    t.stag:3:31-51: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``B _`
    t.stag:4:32-67: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``B false`
    t.stag:6:62-64: Error: The row {`C : unit; ..'_a} is incompatible with the row {}
    The label C is missing.
    t.stag:7:74-76: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:8:79-80: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:9:39-59: Warning: This match is not exhaustive [non-exhaustive]: it does not cover ``B _`
    t.stag:16:13-39: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``C`
    |}];
  check
    {|
let inferred_closed v = match v with `A -> 0 | `B n -> n
let inferred_missing_payload v = match v with `A -> 0 | `B true -> 1
let open_wild v = match v with `A -> 0 | _ -> 1
let open_annotated (v : [`A | `B of int; ..'r]) = match v with `A -> 0 | _ -> 1
let open_closed_by_match (v : [`A | `B of int; ..'r]) = match v with `A -> 0 | `B n -> n
let or_missing (v : [`A | `B of int | `C of bool]) = match v with `A | `B _ -> 0
let or_exhaustive (v : [`A | `B of int]) = match v with `A | `B 0 -> 0 | `B _ -> 1
let or_nested (v : [`A of option[bool] | `B of int]) =
  match v with
  | `A None | `A (Some true) -> 0
  | `B _ -> 1
let nested_closed (v : [`X of [`A | `B of int] | `Y]) =
  match v with
  | `X `A -> 0
  | `Y -> 1
let nested_missing_outer (v : [`X of [`A | `B of int] | `Y]) =
  match v with
  | `X `A -> 0
|};
  [%expect
    {|
    t.stag:3:33-68: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``B false`
    t.stag:7:53-80: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``C _`
    t.stag:10:2-12:13: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``A (Some false)`
    t.stag:14:2-16:11: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``X (`B _)`
    t.stag:18:2-19:14: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``Y`
    |}]
;;

let%expect_test "nonrec declarations with exhaustiveness and warnings" =
  check
    {|
type t = A | B
module M = struct
  type nonrec t = C of t | D
  let f (x : t) = match x with C A -> 0 | D -> 1
end
let nonrec g (v : t) = match v with A -> 0
[@@warning "+non-exhaustive"]
let nonrec h (v : t) = match v with A -> 0 | B -> 1 | A -> 2
[@@warning "-unused-arm"]
let local_nonrec (v : t) =
  let nonrec x = (match v with A -> 0) [@warning "+non-exhaustive"] in
  x
|};
  [%expect
    {|
    t.stag:5:18-48: Error: This match is not exhaustive [non-exhaustive]: it does not cover `C B`
    t.stag:7:23-42: Warning: This match is not exhaustive [non-exhaustive]: it does not cover `B`
    t.stag:12:17-38: Warning: This match is not exhaustive [non-exhaustive]: it does not cover `B`
    |}]
;;

let%expect_test "exception arms" =
  check
    {|
let f g = match g () with v -> v | exception Failure _ -> 0
let h g = match g () with v -> v | exception _ -> 0 | exception Failure _ -> 1
let k g = match g () with v -> v | exception Failure _ -> 0 | exception Failure "x" -> 1
|};
  [%expect
    {|
    t.stag:3:64-73: Warning: This exception case is unused [unused-arm]: the exception cases before it match every exception it does
    t.stag:4:72-83: Warning: This exception case is unused [unused-arm]: the exception cases before it match every exception it does
    |}]
;;

let%expect_test "warning attributes and specs" =
  check
    {|
type t = A | B
let a v = (match v with A -> 0 | B -> 1 | A -> 2) [@warning "-unused-arm"]
let b v = (match v with A -> 0) [@warning "+non-exhaustive"]
let c v = (match v with A -> 0 | B -> 1 | _ -> 2) [@warning "@unused-arm"]
let d v = match v with A -> 0
[@@warning "-non-exhaustive"]
[@@@warning "-all"]
let e v = match v with A -> 0 | _ -> 1 | B -> 2
[@@@warning "+all"]
let f v = match v with A -> 0 | _ -> 1 | B -> 2
module M = struct
  [@@@warning "-unused-arm"]
  let g v = match v with A -> 0 | _ -> 1 | B -> 2
end
let h v = match v with A -> 0 | _ -> 1 | B -> 2
|};
  [%expect
    {|
    t.stag:4:10-31: Warning: This match is not exhaustive [non-exhaustive]: it does not cover `B`
    t.stag:5:42-43: Error: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:11:41-42: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    t.stag:16:41-42: Warning: This case is unused [unused-arm]: the cases before it match every value it does
    |}];
  check
    ~w:[ "-non-exhaustive"; "@unused-arm" ]
    {|
type t = A | B
let a v = match v with A -> 0
let b v = match v with A -> 0 | B -> 1 | A -> 2
|};
  [%expect
    {| t.stag:4:41-42: Error: This case is unused [unused-arm]: the cases before it match every value it does |}]
;;

let%expect_test "bad attributes" =
  let parse source =
    match Parser.program ~file source with
    | Ok _ -> print_endline "ok"
    | Error error -> print_endline (Parser.Error.to_string error)
  in
  parse {|let a v = v [@warning "-nonsense"]|};
  parse {|let b v = v [@frobnicate]|};
  parse {|let c v = v [@warning]|};
  parse {|let d v = v [@@warning "-all"]|};
  parse {|[@@@warning "+all"] let e = 1 [@@warning "@all"]|};
  [%expect
    {|
    t.stag:1:22-33: unknown warning `nonsense`: the warnings are `non-exhaustive`, `unused-arm`, and `all` names every one
    t.stag:1:14-24: unknown attribute `[@frobnicate]`: the attributes are `[@nontail]` and `[@warning "spec"]`
    t.stag:1:14-21: `[@warning]` takes a warning spec, as in `[@warning "-unused-arm"]`
    ok
    ok
    |}]
;;

let%expect_test "module-qualified constructors print relative to the match" =
  check
    {|
module M = struct
  type t = A | B of int
  let inside v = match v with A -> 0
end
let outside v = match v with M.A -> 0
let opt v = match v with Some 0 -> 0 | None -> 1
let lst v = match v with [] -> 0 | [_] -> 1
|};
  [%expect
    {|
    t.stag:4:17-36: Error: This match is not exhaustive [non-exhaustive]: it does not cover `B _`
    t.stag:6:16-37: Error: This match is not exhaustive [non-exhaustive]: it does not cover `M.B _`
    t.stag:7:12-48: Error: This match is not exhaustive [non-exhaustive]: it does not cover `Some 1`
    t.stag:8:12-43: Error: This match is not exhaustive [non-exhaustive]: it does not cover `Cons *(_, Cons _)`
    |}]
;;

let%expect_test "matches in quotes and splices" =
  check
    {|
type t = A | B
let q = .< fun v -> match v with A -> 0 >.
let r = .~(.< fun v -> match v with A -> 0 >.)
let ok = .< fun v -> match v with A -> 0 | B -> 1 >.
|};
  [%expect
    {|
    t.stag:3:20-39: Error: This match is not exhaustive [non-exhaustive]: it does not cover `B`
    t.stag:4:23-42: Error: This match is not exhaustive [non-exhaustive]: it does not cover `B`
    |}]
;;

let%expect_test "shadowed polymorphic variant tags" =
  check
    {|
type t[{..'r}] = [`A of int; ..'r]
let residual (v : t[{`A : bool}]) =
  match v with
  | `A n -> n
  | other -> (match other with `A b -> match b with true -> 1 | false -> 2)
|};
  [%expect {| ok |}];
  check
    {|
type t[{..'r}] = [`A of int; ..'r]
let f (v : t[{`A : bool}]) = match (v, 0) with (`A n, _) -> n
|};
  [%expect
    {| t.stag:3:29-61: Error: This match is not exhaustive [non-exhaustive]: it does not cover `_, _` |}]
;;

let%expect_test "stacked expression warning attributes" =
  check
    {|
type t = A | B
let err v = (match v with A -> 0 | B -> 1 | A -> 2) [@warning "-all"] [@warning "@unused-arm"]
let silent v = (match v with A -> 0 | B -> 1 | A -> 2) [@warning "@unused-arm"] [@warning "-all"]
|};
  [%expect
    {| t.stag:3:44-45: Error: This case is unused [unused-arm]: the cases before it match every value it does |}]
;;

let%expect_test "closed-tail variant with optional tag" =
  check
    {|
let closed_pvar (v : [..'r]) (c : <int>{..'r}) =
  let _ : <int>{a : int} = .< let y = 1 in .~c >. in
  match v with `a n -> n
|};
  [%expect {| ok |}]
;;

let%expect_test "nested error type in scrutinee suppresses coverage" =
  check
    {|
let f _ = match (unbound_xyz, true) with ( *1, true) -> 0 | (1, true) -> 1
|};
  [%expect {| t.stag:2:17-28: Error: Unbound value unbound_xyz |}];
  check
    {|
type t = [`Nil | `Cons of int * ptr[t]]
let f (v : t) = match v with `Nil -> 0
|};
  [%expect
    {| t.stag:3:16-38: Error: This match is not exhaustive [non-exhaustive]: it does not cover ``Cons _` |}]
;;
