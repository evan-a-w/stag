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

let modes source =
  match signature source with
  | Ok signature -> print_string signature
  | Error errors -> List.iter errors ~f:print_endline
;;

(* Annotates a copy of each value with its printed type, which reads back as the
   same type. *)
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
  let source = String.concat ~sep:"\n" (source :: copies) in
  print_endline (String.concat ~sep:"\n" copies);
  match signature source with
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
      if not ([%equal: string option] (find name) (find original))
      then
        print_s
          [%message
            "differs"
              (name : string)
              (find original : string option)
              (find name : string option)])
;;

let%expect_test "arrow modes are polymorphic" =
  modes
    {|
let ident x = x
let apply f x = f x
let const_ x y = x
let n = ident 3
let code = .<n>.
let m = Int.add 1 2
let code2 = .<m>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val apply : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and 'm2 <= 'm5 and 'm4 <= 'm1 and portable <= 'm5
    val const_ : 'a @ 'm0 -> ('b -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm1 and 'm0 <= 'm2
    val n : int
    val code : <int>{..'a}
    val m : int
    val code2 : <'a>{m : 'a; ..'b}
    |}]
;;

let%expect_test "instances are independent" =
  modes
    {|
let ident x = x
let a = ident 1
let b = ident (Int.add 1 2)
let qa = .<a>.
let qb = .<b>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val a : int
    val b : int
    val qa : <int>{..'a}
    val qb : <'a>{b : 'a; ..'b}
    |}]
;;

let%expect_test "annotations" =
  modes
    {|
let f (x @ portable) = .<x>.
let g : 'a @ 'm -> 'a @ 'm = fun x -> x
let h (x @ 'm) : int @ 'm = x
type t = { k : int @ portable -> int }

|};
  [%expect
    {|
    val f : 'a @ portable -> <'a>{..'b} @ 'm0 with portable <= 'm0
    val g : 'a @ 'm0 -> 'a @ 'm0
    val h : int @ 'm0 -> int @ 'm0
    type t = { k : int @ portable -> int }
    |}]
;;

let%expect_test "parameters used in quotes are free variables" =
  modes {|
let f x = .<x>.
let g x = let h u = x in .<h>.
|};
  [%expect
    {|
    val f : 'a -> <'b>{x : 'b; ..'c}
    val g : 'a @ 'm0 -> <'b>{h : 'b; ..'c} @ 'm1 with 'm0 <= 'm1
    |}]
;;

let%expect_test "an argument annotated portable must be" =
  modes {|
let f (x @ portable) = .<x>.
let bad = f (Int.add 1 2)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:12-25), then it is passed to a function (at t.stag:3:10-25), but must be at most portable, because it is annotated portable (at t.stag:2:11-19), then the function types are unified |}]
;;

let%expect_test "printed constraints are simplified" =
  modes
    {|
let h (x @ 'm) : int @ 'm = x
let chain f g x = f (g x)
let fixed (x @ portable) : int @ portable = x
let both (x @ portable) = x
|};
  [%expect
    {|
    val h : int @ 'm0 -> int @ 'm0
    val chain : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm3 and 'm0 <= 'm7 and portable <= 'm1 and 'm2 <= 'm9 and 'm4 <= 'm7 and 'm6 <= 'm1 and 'm8 <= 'm5 and portable <= 'm9
    val fixed : int @ portable -> int @ portable
    val both : 'a @ portable -> 'a @ 'm0 with portable <= 'm0
    |}]
;;

let%expect_test "printed types read back as themselves" =
  round_trip
    {|
let ident x = x
let apply f x = f x
let const_ x y = x
let chain f g x = f (g x)
let f (x @ portable) = .<x>.
let g x = let h u = x in .<h>.
let h (x @ 'm) : int @ 'm = x
let fixed (x @ portable) : int @ portable = x
|};
  [%expect
    {|
    let ident_copy : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 = ident
    let apply_copy : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and 'm2 <= 'm5 and 'm4 <= 'm1 and portable <= 'm5 = apply
    let const__copy : 'a @ 'm0 -> ('b -> 'a @ 'm2) @ 'm1 with 'm0 <= 'm1 and 'm0 <= 'm2 = const_
    let chain_copy : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> (('c @ 'm5 -> 'a @ 'm6) @ 'm4 -> ('c @ 'm8 -> 'b @ 'm9) @ 'm7) @ 'm3 with 'm0 <= 'm3 and 'm0 <= 'm7 and portable <= 'm1 and 'm2 <= 'm9 and 'm4 <= 'm7 and 'm6 <= 'm1 and 'm8 <= 'm5 and portable <= 'm9 = chain
    let f_copy : 'a @ portable -> <'a>{..'b} @ 'm0 with portable <= 'm0 = f
    let g_copy : 'a @ 'm0 -> <'b>{h : 'b; ..'c} @ 'm1 with 'm0 <= 'm1 = g
    let h_copy : int @ 'm0 -> int @ 'm0 = h
    let fixed_copy : int @ portable -> int @ portable = fixed
    |}]
;;

let%expect_test "annotated constraints constrain" =
  modes
    {|
let loose : 'a @ 'm -> 'a @ 'n = fun x -> x
let tight : 'a @ 'm -> 'a @ 'n with 'n <= 'm = fun x -> x
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let ok = capped 1
|};
  [%expect
    {|
    val loose : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val tight : 'a @ 'm0 -> 'a @ 'm0
    val capped : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and 'm1 <= portable
    val ok : int
    |}];
  modes
    {|
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let bad = capped (Int.add 1 2)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:17-30), then it is passed to a function (at t.stag:3:10-30), but must be at most portable, because it is annotated 'n <= portable (at t.stag:2:37-51), then the function types are unified, then the function returns it (at t.stag:2:63-64), then x is used (at t.stag:2:63-64), then x is passed to the function (at t.stag:2:54-64), then the function types are unified, then the function types are unified |}];
  modes
    {|
let pinned : int @ 'm -> int @ 'n with 'n <= 'm and 'm <= const = fun x -> x
|};
  [%expect {| val pinned : int @ const -> int @ const |}]
;;

let%expect_test "equalities between arrows of different modes are kept" =
  modes
    {|
let f x = x
let user = .< let r = f (Int.add 1 0) in let s = Int.add r 0 in .<r>. >.
let pair c d = .< (.~c, .~d) >.
let both = pair user .< Int.add (f 1) 1 >.
let prov = .< let mut cell = 5 in let f (u : int) = cell in .~both >.
|};
  [%expect
    {|
    val f : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val user : <<'a>{r : 'a; ..'b}>{..'c}
    val pair : <'a>{..'b} @ 'm0 -> (<'c>{..'d} @ 'm2 -> <'a * 'c>{..'e} @ 'm3) @ 'm1 where ..'b <= {..'e}, ..'d <= {..'e} with 'm0 <= 'm1 and 'm0 <= 'm3 and 'm2 <= 'm3
    val both : <<'_a>{r : '_a; ..'_b} * int>{cell?'_c : int; f?'_d : immut int @ '_m0 -> int @ nonportable; ..'_e} where ..'_e <= {..'_f}
    val prov : <<'_a>{r : '_a; ..'_b} * int>{..'_c}
    |}]
;;

let%expect_test "a parameter's mode annotation survives re-inference" =
  modes
    {|
let q = .< fun (x @ portable) -> .<x>. >.
let r = .~q
let s = .~(r 1)
|};
  [%expect
    {|
    val q : <'a @ portable -> <'a>{..'b} @ 'm0>{..'c} with portable <= 'm0
    val r : 'a @ portable -> <'a>{..'b} @ 'm0 with portable <= 'm0
    val s : int
    |}]
;;

let%expect_test "results of function parameters are not assumed portable" =
  modes {|
let t f = let r = f 1 in .<r>.
let u = t (Int.add 1)
|};
  [%expect
    {|
    val t : (int -> 'a @ 'm0) -> <'b>{r : 'b; ..'c} @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    val u : <'_a>{r : '_a; ..'_b}
    |}];
  modes
    {|
let b y = let w = a y in y
let a x = let r = b x in .<r>.
let c = a (Int.add 1 2)
|};
  [%expect
    {|
    val b : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    val a : 'a @ 'm0 -> <'b>{r : 'b; ..'c} @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    val c : <'_a>{r : '_a; ..'_b}
    |}];
  modes
    {|
let a x = let r = b x in .<r>.
let b y = let w = a y in y
let c = a (Int.add 1 2)
|};
  [%expect
    {|
    val a : 'a @ 'm0 -> <'b>{r : 'b; ..'c} @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    val b : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    val c : <'_a>{r : '_a; ..'_b}
    |}]
;;

let%expect_test "mutually recursive toplevels print constraints through each \
                 other"
  =
  round_trip {|
let a x = let r = b x in r
let b y = let w = a y in y
|};
  [%expect
    {|
    let a_copy : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 = a
    let b_copy : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 = b
    |}]
;;

let%expect_test "weak mode variables" =
  modes {|
let k = (fun f -> f) (fun w -> w)
let n = k 1
|};
  [%expect
    {|
    val k : int @ '_m0 -> int @ '_m1 with '_m0 <= '_m1
    val n : int
    |}]
;;

let%expect_test "partial applications' constraints have locations" =
  modes
    {|
let make n = let mut cell = n in let f (u : int) = cell in f
let pair x y = x
let p @ portable = pair (make 1)
|};
  [%expect
    {| This is nonportable, because cell is mutable (at t.stag:2:13-60), then cell is captured by a closure (at t.stag:2:51-55), then f is bound (at t.stag:2:33-60), then the function returns it (at t.stag:2:13-60), then the function types are unified, then the function returns it (at t.stag:4:24-32), then it is passed to a function (at t.stag:4:19-32), then the function types are unified, then a partial application holds it (at t.stag:3:15-16), then the function types are unified, then the function returns it (at t.stag:4:19-32), but must be at most portable, because it is annotated portable (at t.stag:4:8-16), then it is bound by an annotated let (at t.stag:4:8-16) |}]
;;

let%expect_test "annotations on uses bound the variable's mode" =
  modes
    {|
let cap x = (x @ portable)
let cap_with (x : int) = (x : int @ 'm with 'm <= portable)
let same (x @ 'm) = let y = (x @ 'm) in y
|};
  [%expect
    {|
    val cap : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and 'm0 <= portable
    val cap_with : int @ 'm0 -> int @ 'm1 with 'm0 <= 'm1 and 'm0 <= portable
    val same : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    |}];
  modes {|
let bad = let mut c = 1 in (c @ portable)
|};
  [%expect
    {| This is nonportable, because c is mutable (at t.stag:2:27-41), but must be at most portable, because it is annotated portable (at t.stag:2:32-40) |}];
  modes {|
let bad = cap (Int.add 1 2)
let cap x = (x @ portable)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:14-27), then it is passed to a function (at t.stag:2:10-27), but must be at most portable, because it is annotated portable (at t.stag:3:17-25), then x is used (at t.stag:3:12-26), then x is passed to the function (at t.stag:3:12-26), then the function types are unified |}]
;;

let%expect_test "a let mut's mode bounds the values it holds" =
  modes {|
let ok u = let mut x @ portable = 1 in x <- 2; x
|};
  [%expect {| val ok : 'a -> int @ nonportable |}];
  modes {|
let bad u = let mut x @ const = 1 in x <- Int.add 1 2; x
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:42-53), then it is assigned to x (at t.stag:2:37-53), but must be at most const, because it is annotated const (at t.stag:2:24-29) |}];
  modes
    {|
let mut counter @ const = 0
let bump u = counter <- Int.add counter 1
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:24-41), then it is assigned to counter (at t.stag:3:13-41), but must be at most const, because it is annotated const (at t.stag:2:18-23) |}];
  modes {|
let mut counter @ portable = Int.add 1 2
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:29-40), but must be at most portable, because it is annotated portable (at t.stag:2:18-26), then it initializes counter (at t.stag:2:29-40) |}]
;;

let%expect_test "use and let mut annotations survive re-inference" =
  modes
    {|
let q = .< fun x -> (x @ portable) >.
let r = .~q
let q2 = .< fun x -> let mut c @ portable = 1 in c <- x; c >.
let r2 = .~q2
|};
  [%expect
    {|
    val q : <'a @ 'm0 -> 'a @ 'm1>{..'b} with 'm0 <= 'm1 and 'm0 <= portable
    val r : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and 'm0 <= portable
    val q2 : <int @ 'm0 -> int @ nonportable>{..'a} with 'm0 <= portable
    val r2 : int @ 'm0 -> int @ nonportable with 'm0 <= portable
    |}]
;;
