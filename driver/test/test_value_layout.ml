open! Import
open Source_helpers

let%expect_test "nominal and structural by-value cycles" =
  check ~prelude:false "type t = T of t";
  check ~prelude:false "type r = { next : (int * r) }";
  check ~prelude:false "type t = [`T of t]";
  check ~prelude:false "type r = {`next : r}";
  check ~prelude:false {|
type a = { b : b }
type b = B of a
|};
  [%expect
    {|
    ((Infinite_type_layout t) @ t.stag:1:5-6)
    ((Infinite_type_layout r) @ t.stag:1:5-6)
    ((Infinite_type_layout t) @ t.stag:1:5-6)
    ((Infinite_type_layout r) @ t.stag:1:5-6)
    ((Infinite_type_layout a) @ t.stag:2:5-6)
    |}]
;;

let%expect_test "nominal parameters follow storage transitively" =
  check
    ~prelude:false
    {|
type t = T of outside[t]
type outside['a] = { inner : inside['a] }
type inside['a] = Inside of (int * 'a)
|};
  check
    ~prelude:false
    {|
type t = T of outer[{`next : t}]
type outer[{..'r}] = Outer of inner[{..'r}]
type inner[{..'r}] = { fields : {..'r} }
|};
  check
    ~prelude:false
    {|
type t = T of outside[t]
type outside['a] = { inner : inside['a] }
type inside['a] = Inside of ('a -> int)
type finite = Finite of outer[outer[int]]
type outer['a] = Outer of 'a
|};
  [%expect
    {|
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    type t = T of outside[t]
    type outside['a] = { inner : inside['a] }
    type inside['a] = Inside of ('a -> int)
    type finite = Finite of outer[outer[int]]
    type outer['a] = Outer of 'a
    |}]
;;

let%expect_test "resolved ptr is a barrier, not a namespaced type called ptr" =
  let source =
    {|
type t = T of wrap[ptr[t]]
type wrap['a] = Wrap of 'a
type node = {`next : ptr[node]}
type link = ptr[link]
let id (x : link) : link = x
|}
  in
  (* Supply the declaration in isolation if the pointer builtin has not been
     wired in yet. Either way the resolved constructor is [ptr]. *)
  let builtin =
    List.exists Stag_typecheck.Type_env.builtins ~f:(fun decl ->
      String.equal decl.Syntax.Type_decl.name.txt "ptr")
  in
  let source = if builtin then source else "type ptr['a]\n" ^ source in
  Option.iter (typecheck ~prelude:false source) ~f:(fun t ->
    let signature =
      List.filter (checked t).signature ~f:(function
        | Type { name = "ptr"; _ } -> false
        | _ -> true)
    in
    print_string (Signature.to_string_hum signature));
  check
    ~prelude:false
    {|
module M = struct type ptr['a] = Pretend_ptr of 'a end
type t = T of M.ptr[t]
|};
  [%expect
    {|
    type t = T of wrap[ptr[t]]
    type wrap['a] = Wrap of 'a
    type node = {`next : ptr[node]}
    type link = ptr[link]
    val id : (ptr['a] as 'a) -> (ptr['b] as 'b)
    ((Infinite_type_layout t) @ t.stag:3:5-6)
    |}]
;;

let%expect_test "inferred cycles use nominal parameter layouts" =
  check
    ~prelude:false
    {|
let close x = match true with true -> x | false -> Wrap (Next x)
type wrap['a] = Wrap of next['a]
type next['a] = Next of 'a
|};
  check
    ~prelude:false
    {|
let tie (x : 'a) : wrap['a] = Wrap (Next 0)
let close x = match true with true -> x | false -> tie x
type wrap['a] = Wrap of next['a]
type next['a] = Next of int
|};
  [%expect
    {|
    ((Unification_error (Occurs_check_failed (ATy (TVar (Unbound 4 1))))) @
     t.stag:2:51-64)
    val tie : 'a -> wrap['a]
    val close : wrap[(wrap['a] as 'a)] -> wrap['a]
    type wrap['a] = Wrap of next['a]
    type next['a] = Next of int
    |}]
;;
