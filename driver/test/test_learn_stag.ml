open! Import
module Claim = Doc_blocks.Claim

(* Checks and runs each stag block of learn_stag.md, printing what it gives, and
   any claim of its comments (see [Doc_blocks.Claim]) the output contradicts. *)

let document = "learn_stag.md"
let document_contents = lazy (In_channel.read_all ("../../" ^ document))

(* Each block is the file [doc.stag], whose lines are the document's. *)
let file = "doc.stag"

let limits =
  { Limits.fuel = Some 10_000_000
  ; max_depth = Some 100_000
  ; time_limit = Some (Time_ns.Span.of_int_sec 10)
  }
;;

module Transcript = struct
  type t =
    { signature : string list
    ; values : string list
    ; output : string
    ; errors : string
    }

  let empty = { signature = []; values = []; output = ""; errors = "" }

  let holds t (claim : Claim.t) =
    let contains text pattern =
      String.is_substring
        (Doc_blocks.normalize text)
        ~substring:(Doc_blocks.normalize pattern)
    in
    match claim with
    | Item item -> List.mem t.signature item ~equal:String.equal
    | Value { name; value } ->
      List.exists t.values ~f:(fun line ->
        let claimed = name ^ " = " ^ value in
        String.equal line claimed
        || String.is_suffix line ~suffix:("." ^ claimed))
    | Prints s -> contains t.output s
    | Error s -> contains t.errors s
  ;;
end

let print_section name text =
  if not (String.is_empty (String.strip text))
  then
    printf
      "-- %s\n%s%s"
      name
      text
      (if String.is_suffix text ~suffix:"\n" then "" else "\n")
;;

let diagnostics_to_string ~contents diagnostics =
  let contents path = Option.some_if (String.equal path file) contents in
  List.map diagnostics ~f:(Diagnostic.to_string_hum ~contents) |> String.concat
;;

(* The value of each toplevel of [signature], in program order. *)
let values driver signature =
  let checked = Driver.checked driver in
  let idents =
    List.concat_map checked.program.sccs ~f:(fun scc -> scc.members)
    |> List.map ~f:(fun (member : Typed.Toplevel.t) ->
      member.ident.name, member.ident)
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  let rec walk prefix (items : Signature.Item.t list) =
    List.concat_map items ~f:(function
      | Value { name; _ } ->
        let name = prefix ^ name in
        (match Map.find idents name with
         | None -> []
         | Some ident ->
           (match Interpreter.find (Driver.interpreter driver) ~ident with
            | Ok value ->
              [ name
                ^ " = "
                ^ Interpreter.Value.to_string_hum ~max_length:300 value
              ]
            | Error _ -> []))
      | Module { name; items = inner } -> walk (prefix ^ name ^ ".") inner
      | Type _ | Exception _ -> [])
  in
  walk "" signature
;;

let failed_tests driver =
  Expect_test_runner.run ~limits driver
  |> List.filter ~f:(Fn.non Expect_test_runner.Test_result.passed)
  |> List.map ~f:(fun result ->
    Sexp.to_string_hum [%sexp (result : Expect_test_runner.Test_result.t)]
    ^ "\n")
  |> String.concat
;;

(* What each stale expansion should be. *)
let stale_expansions driver ~contents =
  List.filter_map (Inline_expansion.stale driver) ~f:(fun expansion ->
    Inline_expansion.edit expansion ~contents)
  |> List.map ~f:(fun (edit : Source_edit.t) ->
    "-- expansion:\n" ^ edit.replacement)
  |> String.concat
;;

let run_program (block : Doc_blocks.Block.t) =
  let output = Buffer.create 64 in
  let file = { Source_file.path = file; contents = block.code } in
  match
    Driver.check_files ~limits ~output:(Buffer.add_string output) [ file ]
  with
  | Error errors ->
    { Transcript.empty with
      errors =
        diagnostics_to_string
          ~contents:block.code
          (List.map errors ~f:Diagnostic.of_driver_error)
    ; output = Buffer.contents output
    }
  | Ok driver ->
    let checked = Driver.checked driver in
    let signature =
      Signature.to_string_hum ~modes:block.modes checked.signature
    in
    let compile_time_output = Buffer.contents output in
    Buffer.clear output;
    let errors =
      match Driver.eval ~limits driver with
      | Ok () ->
        failed_tests driver
        ^ diagnostics_to_string
            ~contents:block.code
            (List.map
               (Inline_expansion.stale driver)
               ~f:Inline_expansion.diagnostic)
        ^ stale_expansions driver ~contents:block.code
      | Error error ->
        diagnostics_to_string
          ~contents:block.code
          [ Diagnostic.of_runtime_error error ]
    in
    print_string signature;
    print_section "compile-time output" compile_time_output;
    { signature = Doc_blocks.signature_items signature
    ; values = values driver checked.signature
    ; output = compile_time_output ^ Buffer.contents output
    ; errors
    }
;;

let rec remove path =
  if Sys_unix.is_directory_exn path
  then (
    Array.iter (Sys_unix.readdir path) ~f:(fun name -> remove (path ^/ name));
    Core_unix.rmdir path)
  else Sys_unix.remove path
;;

let with_dir files ~f =
  let root = Filename_unix.temp_dir "learn_stag" "" in
  List.iter files ~f:(fun (path, contents) ->
    Core_unix.mkdir_p (Filename.dirname (root ^/ path));
    Out_channel.write_all (root ^/ path) ~data:contents);
  Exn.protect ~f:(fun () -> f root) ~finally:(fun () -> remove root)
;;

(* Checks each target, running executables and the expect tests of the others. *)
let run_project (block : Doc_blocks.Block.t) =
  let files = ("stag-project", "") :: Doc_blocks.project_files block.code in
  with_dir files ~f:(fun root ->
    let print_diagnostics diagnostics =
      let contents path = List.Assoc.find files path ~equal:String.equal in
      List.map diagnostics ~f:(Diagnostic.to_string_hum ~contents)
      |> String.concat
    in
    match Stag_build.Project.load ~root () with
    | Error diagnostics ->
      { Transcript.empty with errors = print_diagnostics diagnostics }
    | Ok project ->
      List.fold
        (Stag_build.Project.targets project)
        ~init:Transcript.empty
        ~f:(fun (t : Transcript.t) target ->
          let output = Buffer.create 64 in
          printf "-- target %s\n" (Stag_build.Project.Target.name target);
          match
            Stag_build.Program.check
              ~output:(Buffer.add_string output)
              project
              target
          with
          | Error diagnostics ->
            { t with errors = t.errors ^ print_diagnostics diagnostics }
          | Ok driver ->
            let signature =
              Signature.to_string_hum (Driver.checked driver).signature
            in
            print_string signature;
            let errors =
              match target.stanza.kind with
              | Executable ->
                (match Driver.eval ~limits driver with
                 | Ok () -> ""
                 | Error error ->
                   print_diagnostics [ Diagnostic.of_runtime_error error ])
              | Library | Test -> failed_tests driver
            in
            { signature = t.signature @ Doc_blocks.signature_items signature
            ; values = t.values
            ; output = t.output ^ Buffer.contents output
            ; errors = t.errors ^ errors
            }))
;;

let run (block : Doc_blocks.Block.t) =
  printf "=== %s:%d\n" document block.line;
  let transcript =
    match block.kind with
    | Program | Error -> run_program block
    | Project -> run_project block
  in
  List.iter transcript.values ~f:print_endline;
  print_section "output" transcript.output;
  (match block.kind, String.is_empty transcript.errors with
   | Error, true -> print_endline "!! EXPECTED AN ERROR"
   | (Program | Project), false -> print_endline "!! UNEXPECTED ERROR"
   | _ -> ());
  print_string transcript.errors;
  let claims = Doc_blocks.claims block.code in
  (match block.kind with
   | Error
     when not
            (List.exists claims ~f:(function
              | Error _ -> true
              | Item _ | Value _ | Prints _ -> false)) ->
     print_endline "!! AN ERROR BLOCK MUST CLAIM ITS ERROR"
   | _ -> ());
  List.iter claims ~f:(fun claim ->
    if not (Transcript.holds transcript claim)
    then print_s [%message "!! CLAIM DOES NOT HOLD" (claim : Claim.t)])
;;

let%expect_test "learn_stag.md" =
  List.iter (Doc_blocks.parse (force document_contents)) ~f:run;
  [%expect
    {|
    === learn_stag.md:26
    module Doc : sig
      val answer : int
      val million : int
      val negative : int
      val greeting : string
      val raw : string
      val letter : char
      val yes : bool
      val nothing : unit
      val sum : int
      val diff : int
      val quotient : int
      val smaller : bool
      val joined : string
      val shown : string
      val flipped : bool
      val add : int -> int -> int
      val add5 : int -> int
      val eight : int
      val twice : ('a -> 'a) -> 'a -> 'a
      val square : int -> int
      val sixteen : int
      val shout : string -> string
      val fact : int -> int
      val fact10 : int
      val is_even : int -> bool
      val is_odd : int -> bool
      val even10 : bool
      val hello : unit
    end
    Doc.answer = 42
    Doc.million = 1000000
    Doc.negative = -7
    Doc.greeting = "hello\tworld\n"
    Doc.raw = "no \\escapes here"
    Doc.letter = 'x'
    Doc.yes = true
    Doc.nothing = ()
    Doc.sum = 3
    Doc.diff = 13
    Doc.quotient = 3
    Doc.smaller = true
    Doc.joined = "foobar"
    Doc.shown = "42"
    Doc.flipped = false
    Doc.add = <fun>
    Doc.add5 = <fun>
    Doc.eight = 8
    Doc.twice = <fun>
    Doc.square = <fun>
    Doc.sixteen = 16
    Doc.shout = <fun>
    Doc.fact = <fun>
    Doc.fact10 = 3628800
    Doc.is_even = <fun>
    Doc.is_odd = <fun>
    Doc.even10 = true
    Doc.hello = ()
    -- output
    hello, stag
    === learn_stag.md:82
    module Doc : sig
      type point = { x : int; y : int }
      val origin : point
      val moved : point
      val y : int
      val pair : int * string
      type shape = Circle of int | Rect of int * int | Empty
      val shapes : shape * shape * shape
      type tree['a] = Leaf | Node of 'a * ptr[tree['a]] * ptr[tree['a]]
      val tag : [`Num of int; ..'a]
      val person : {`age : int; `name : string}
      val older : {`age : int; `name : string}
      val with_id : {`age : int; `id : int; `name : string}
      val without_age : {`name : string}
      val get_name : {`name : 'a; ..'b} -> 'a
    end
    Doc.origin = {Doc.x = 0; y = 0}
    Doc.moved = {Doc.x = 0; y = 5}
    Doc.y = 5
    Doc.pair = (1, "one")
    Doc.shapes = (Doc.Circle 2, Doc.Rect (3, 4), Doc.Empty)
    Doc.tag = `Num 7
    Doc.person = {`age = 36; `name = "ada"}
    Doc.older = {`age = 37; `name = "ada"}
    Doc.with_id = {`age = 36; `id = 1; `name = "ada"}
    Doc.without_age = {`name = "ada"}
    Doc.get_name = <fun>
    === learn_stag.md:112
    module Doc : sig
      type shape = Circle of int | Rect of int * int | Empty
      val area : shape -> int
      val areas : int * int
      val describe : int * string -> string
      val d : string
      val size : [`Nothing | `Num of int | `Str of string] -> int
      val name_of : {`name : 'a; ..'b} -> 'a
      exception Not_found of string
      val lookup : string -> int
      val safe : string -> int
      val found : int * int
    end
    Doc.area = <fun>
    Doc.areas = (12, 12)
    Doc.describe = <fun>
    Doc.d = "one, spelled out"
    Doc.size = <fun>
    Doc.name_of = <fun>
    Doc.lookup = <fun>
    Doc.safe = <fun>
    Doc.found = (1, -1)
    === learn_stag.md:153
    module Doc : sig
      val f : int -> string
      val boom : string
    end
    Doc.f = <fun>
    File "doc.stag", line 154, characters 10-34:
    154 | let f n = match n with 0 -> "zero"
                    ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Match failure: no case matches the value 1
    === learn_stag.md:164
    module Doc : sig
      val p : ptr[int]
      val one : int
      val stored : int
      type point = { x : int; y : int }
      val q : ptr[point]
      val copy : point
      val after : point * point
      type tree = Leaf of int | Branch of ptr[tree * tree]
      val rec_sum : tree -> int
      val tree : tree
      val total : int
    end
    Doc.p = Ptr.make 2
    Doc.one = 1
    Doc.stored = 2
    Doc.q = Ptr.make {Doc.x = 10; y = 2}
    Doc.copy = {Doc.x = 1; y = 2}
    Doc.after = ({Doc.x = 10; y = 2}, {Doc.x = 1; y = 2})
    Doc.rec_sum = <fun>
    Doc.tree = Doc.Branch (Ptr.make (Doc.Leaf 1, Doc.Branch (Ptr.make (Doc.Leaf 2, Doc.Leaf 3))))
    Doc.total = 6
    === learn_stag.md:187
    File "doc.stag", line 188, characters 5-8:
    188 | type bad = Bad of int * bad        (* Error: recursive *)
               ^^^
    Error: The type Doc.bad contains itself by value; break the cycle with ptr
    === learn_stag.md:195
    module Doc : sig
      val count_to : int -> int
      val c : int
      val counter : int
      val tick : 'a -> unit
      val ticked : int
    end
    Doc.count_to = <fun>
    Doc.c = 6
    Doc.counter = 2
    Doc.tick = <fun>
    Doc.ticked = 2
    === learn_stag.md:210
    File "doc.stag", line 212, characters 10-16:
    212 | let f u = x <- 2                   (* Error: x is not mutable *)
                    ^^^^^^
    Error: The variable x is not mutable: only one bound by [let mut] can be assigned to
    === learn_stag.md:217
    module Doc : sig
      val xs : list[int]
      val rec_len : list['a] -> int
      val first : list['a] -> option['a]
      val hd : option[int]
      val doubled : list[int]
      val total : int
      val evens : list[int]
      val found : option[int]
      val default : int
      val parsed : option[int]
      val res : result[int, '_a]
      val words : string
      val sexp : string
    end
    Doc.xs = Cons (Ptr.make (1, Cons (Ptr.make (2, Cons (Ptr.make (3, Nil))))))
    Doc.rec_len = <fun>
    Doc.first = <fun>
    Doc.hd = Some 1
    Doc.doubled = Cons (Ptr.make (2, Cons (Ptr.make (4, Cons (Ptr.make (6, Nil))))))
    Doc.total = 6
    Doc.evens = Cons (Ptr.make (2, Cons (Ptr.make (4, Nil))))
    Doc.found = None
    Doc.default = 0
    Doc.parsed = Some 12
    Doc.res = Ok 2
    Doc.words = "a, b"
    Doc.sexp = "(a\"b c\")"
    === learn_stag.md:238
    module Doc : sig
      module Geometry : sig
        type point = { x : int; y : int }
        val origin : point
        module Inner : sig
          val unit_x : point
        end
      end
      val o : Geometry.point
      val ux : int
      val get_x : Geometry.point -> int
      val x : int
      val shadowed : int
      val prim : int
    end
    Doc.Geometry.origin = {Doc.Geometry.x = 0; y = 0}
    Doc.Geometry.Inner.unit_x = {Doc.Geometry.x = 1; y = 0}
    Doc.o = {Doc.Geometry.x = 0; y = 0}
    Doc.ux = 1
    Doc.get_x = <fun>
    Doc.x = 10
    Doc.shadowed = 10
    Doc.prim = 3
    === learn_stag.md:272
    -- target main
    module Math : sig
      module Arith : sig
        val square : int -> int
      end
    end
    module Ffi : sig
      module C : sig
        val labs : int -> int
      end
    end
    module Main : sig
      val main : unit
      val at_compile_time : string
      val print_it : unit
    end
    -- target ffi
    module Ffi : sig
      module C : sig
        val labs : int -> int
      end
    end
    -- target math
    module Math : sig
      module Arith : sig
        val square : int -> int
      end
    end
    -- output
    49
    three
    === learn_stag.md:299
    module Doc : sig
      val one : <int>{..'a}
      val plus_x : <int>{x : int; ..'a}
      val two : <int>{..'a}
      val three : int
      val loud : int
      val f : int -> int
      val f2 : int
      val g : int -> int
      val g2 : int
      val power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
      val cube : int -> int
      val c : int
      val square : int -> int
    end
    -- compile-time output
    compiling
    Doc.one = <code>
    Doc.plus_x = <code>
    Doc.two = <code>
    Doc.three = 3
    Doc.loud = 0
    Doc.f = <fun>
    Doc.f2 = 2
    Doc.g = <fun>
    Doc.g2 = 21
    Doc.power = <fun>
    Doc.cube = <fun>
    Doc.c = 27
    Doc.square = <fun>
    -- output
    compiling
    === learn_stag.md:342
    module Doc : sig
      val n : int
      val uses_n : <int>{..'a}
      val local_const : <string>{..'a}
      val local_int : <int>{..'_a}
      val computed : <'_a>{k : '_a; ..'_b}
      val argument : 'a -> <'b>{x : 'b; ..'c}
      val lift : 'a -> <'a>{..'b}
      val lifted : int
      val incr : <unit>{x : mut int; ..'a}
      val twice_incr : 'a -> int
      val two : int
    end
    Doc.n = 3
    Doc.uses_n = <code>
    Doc.local_const = <code>
    Doc.local_int = <code>
    Doc.computed = <code>
    Doc.argument = <fun>
    Doc.lift = <fun>
    Doc.lifted = 5
    Doc.incr = <code>
    Doc.twice_incr = <fun>
    Doc.two = 2
    === learn_stag.md:374
    File "doc.stag", line 375, characters 20-21:
    375 | let argument x = .< x >.
                              ^
    Error: The quote can't embed x, as it may not be portable. Bind it as (x @ portable) to embed it, or write [forget x in ...] to make x a free variable of the quote
    === learn_stag.md:381
    File "doc.stag", line 382, characters 52-53:
    382 | let shadowed x = let y = 1 in forget y in Int.add x y
                                                              ^
    Error: y is not in scope here: it was forgotten by [forget y in ...]
    === learn_stag.md:389
    File "doc.stag", line 391, characters 31-38:
    391 | let at_compile_time = .~(match counter with 0 -> .< "zero" >. | _ -> .< "more" >.)
                                         ^^^^^^^
    Error: The mutable toplevel Doc.counter is used at compile time: mutable toplevels exist only at runtime
    === learn_stag.md:401
    module Doc : sig
      type color = Red | Green | Blue of int
      val t : Stag.Type.t[color]
      val sexp_of_color : color -> Core.Sexp.t
      val color_of_sexp_exn : Core.Sexp.t -> color
      val color_of_sexp : Core.Sexp.t -> result[color, Core.Sexp.Of_sexp_error.t]
      val s : string
      val back : result[color, Core.Sexp.Of_sexp_error.t]
      val constructors : Stag.Type.t['a] -> int
      val n_colors : int
      val make_answer : int -> list[Stag.Toplevel.t]
      module Answer : sig
        val answer : int
      end
      val answer : int
    end
    Doc.t = <type>
    Doc.sexp_of_color = <fun>
    Doc.color_of_sexp_exn = <fun>
    Doc.color_of_sexp = <fun>
    Doc.s = "(Blue 3)"
    Doc.back = Ok Doc.Red
    Doc.constructors = <fun>
    Doc.n_colors = 3
    Doc.make_answer = <fun>
    Doc.Answer.answer = 42
    Doc.answer = 42
    === learn_stag.md:446
    module Doc : sig
      val sum3 : int -> int -> int -> int
      val s : int
      val read_local : ptr[int] @ local -> int
      val make_local : 'a -> ptr['a] @ local
      val used : int
    end
    Doc.sum3 = <fun>
    Doc.s = 7
    Doc.read_local = <fun>
    Doc.make_local = <fun>
    Doc.used = 5
    === learn_stag.md:465
    File "doc.stag", line 467, characters 13-32:
    467 | let leak x = stack_ (Ptr.make x)
                       ^^^^^^^^^^^^^^^^^^^
    Error: This is local, because it is allocated by [stack_] (at doc.stag:467:13-32), then it is part of the value (at doc.stag:467:13-32), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at doc.stag:467:13-32)
    === learn_stag.md:473
    File "doc.stag", line 477, characters 41-44:
    477 | let f x = let p = stack_ (Ptr.make x) in g p
                                                   ^^^
    Error: This is local, because it is allocated by [stack_] (at doc.stag:477:18-37), then it is part of the value (at doc.stag:477:18-37), then p is used (at doc.stag:477:43-44), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at doc.stag:477:43-44)
    === learn_stag.md:481
    module Doc : sig
      val g : ptr[int] @ local -> int
      val f : int -> int
      val seven : int
    end
    Doc.g = <fun>
    Doc.f = <fun>
    Doc.seven = 7
    === learn_stag.md:495
    module Doc : sig
      val embed : 'a -> <'a>{..'b}
      val first : int * int @ local -> int
      val len : string @ local -> int
      val sum : int
      val same : string -> string
      val capped : 'a -> 'a
      val ok : int
    end
    Doc.embed = <fun>
    Doc.first = <fun>
    Doc.len = <fun>
    Doc.sum = 3
    Doc.same = <fun>
    Doc.capped = <fun>
    Doc.ok = 1
    === learn_stag.md:517
    File "doc.stag", line 519, characters 10-34:
    519 | let bad = capped (Int.to_string 3)
                    ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at doc.stag:519:17-34), then it is passed to a function (at doc.stag:519:10-34), but must be at most portable, because it is annotated 'n <= portable (at doc.stag:518:37-51), then the function types are unified, then the function returns it (at doc.stag:518:63-64), then x is used (at doc.stag:518:63-64), then x is passed to the function (at doc.stag:518:54-64), then the function types are unified
    === learn_stag.md:523
    File "doc.stag", line 530, characters 20-25:
    530 | let bad = cell.* <- exact
                              ^^^^^
    Error: This is external, because it is annotated external (at doc.stag:527:12-20), then Doc.exact is used (at doc.stag:530:20-25), then it is stored into a cell, which holds only what fits its modality (at doc.stag:530:20-25), but must be at most gc, because Doc.cell is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default)
    === learn_stag.md:534
    File "doc.stag", line 536, characters 34-60:
    536 | let store (p : ptr[ptr[int]]) x = p.* <- stack_ (Ptr.make x)
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This is local, because it is allocated by [stack_] (at doc.stag:536:41-60), then it is part of the value (at doc.stag:536:41-60), but must be at most global, because it is stored through a pointer, which may outlive any region (at doc.stag:536:41-60)
    === learn_stag.md:547
    module Doc : sig
      type boxed = { inner : ptr[int] @@ global; other : ptr[int] }
      val get_inner : boxed @ local -> ptr[int]
      val cell : ptr[string @@ external]
      val put : string @ external -> unit
    end
    Doc.get_inner = <fun>
    Doc.cell = Ptr.make "s"
    Doc.put = <fun>
    === learn_stag.md:557
    File "doc.stag", line 559, characters 35-36:
    559 | let put (s @ external) = cell.* <- s
                                             ^
    Error: This is external, because it is annotated external (at doc.stag:559:13-21), then s is passed to the function (at doc.stag:559:25-36), then s is used (at doc.stag:559:35-36), then it is stored into a cell, which holds only what fits its modality (at doc.stag:559:35-36), but must be at most gc, because Doc.cell is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default)
    === learn_stag.md:566
    module Doc : sig
      type cell['a, 'm0 : mode] = { value : 'a @@ 'm0 }
      val gc_cell : cell[string, gc]
      val read_cell : cell['a, 'm0] -> 'a
    end
    Doc.gc_cell = {Doc.value = "x"}
    Doc.read_cell = <fun>
    === learn_stag.md:583
    module Doc : sig
      val id : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
      type r = { x : string @@ gc; y : string }
      val get_x : r @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1) and meet(provenance('m0), gc) <= provenance('m1) and release(locality('m0)) <= locality('m1)
      val embed : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and 'm0 <= portable and gc <= 'm1
      val mk : ptr[int] @ local -> ptr[int @@ 'm1] @ 'm0 with nonportable gc local <= 'm0
      val same : string @ 'm0 -> string @ 'm0
      val static_if : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and provenance('m1) <= static_if_const(stage('m0))
    end
    Doc.id = <fun>
    Doc.get_x = <fun>
    Doc.embed = <fun>
    Doc.mk = <fun>
    Doc.same = <fun>
    Doc.static_if = <fun>
    === learn_stag.md:609
    module Doc : sig
      val labs : int -> int
      val a : int
    end
    Doc.labs = <fun>
    Doc.a = 42
    === learn_stag.md:614
    File "doc.stag", line 615, characters 18-31:
    615 | external strlen : string -> int = "strlen"
                            ^^^^^^^^^^^^^
    Error: The external strlen has no C signature: string would be passed to C as a pointer, which is not supported yet
    |}]
;;

let%expect_test "Doc_blocks" =
  let document =
    {|
# A doc

```ocaml
not stag
```

```stag
let x = 1 (* val x : int = 1 *)
(* (* nested *) prints: hi *)
(* type t = A *) (* exception E *) (* Error: bad *) (* just a comment *)
```

```stag-error modes
let y = (* val y : int -> int *) 2
```

```stag-project
-- a/stag
(library (name a))
-- a/a.stag
let z = 3
```
|}
  in
  let blocks = Doc_blocks.parse document in
  List.iter blocks ~f:(fun block ->
    print_s [%sexp (block.kind : Doc_blocks.Kind.t), (block.modes : bool)];
    print_s [%sexp (Doc_blocks.claims block.code : Claim.t list)]);
  [%expect
    {|
    (Program false)
    ((Item "val x : int") (Value (name x) (value 1)) (Item "type t = A")
     (Item "exception E") (Error bad))
    (Error true)
    ((Item "val y : int -> int"))
    (Project false)
    ()
    |}];
  let block = List.nth_exn blocks 1 in
  (* Code keeps the document's line numbers. *)
  print_s
    [%sexp
      (List.findi (String.split_lines block.code) ~f:(fun _ line ->
         String.is_prefix line ~prefix:"let y")
       |> Option.map ~f:(fun (i, _) -> i + 1)
       : int option)
      , (block.line : int)];
  print_s
    [%sexp
      (Doc_blocks.project_files (List.nth_exn blocks 2).code
       : (string * string) list)];
  [%expect
    {|
    ((15) 14)
    ((a/stag "(library (name a))\n") (a/a.stag "let z = 3\n"))
    |}];
  print_s
    [%sexp
      (Doc_blocks.signature_items
         {|module M : sig
  val f :
    int -> int
  type t = A
end
val g : int
|}
       : string list)];
  [%expect
    {| ("module M : sig" "val f : int -> int" "type t = A" end "val g : int") |}];
  List.iter
    [ "```stag-bogus\n```\n"; "```stag\nlet x = 1\n" ]
    ~f:(fun document ->
      print_s
        [%sexp
          (Or_error.try_with (fun () -> Doc_blocks.parse document)
           : Doc_blocks.Block.t list Or_error.t)]);
  [%expect
    {|
    (Error ("unknown code block" (info stag-bogus) (line 1)))
    (Error "unclosed code block")
    |}]
;;
