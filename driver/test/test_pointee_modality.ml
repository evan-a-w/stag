open! Import
open Source_helpers

let provenance source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

let ext = {|
let ext (x : string) = let y @ external = x in y
|}

let%expect_test "a pointer's mode can't let it be given external values" =
  (* [p] is gc: were [q.* <- e] accepted, [p.*] would read an external value as
     gc. *)
  provenance
    (ext
     ^ {|
let put (q : ptr[string] @ external) (e : string @ external) = q.* <- e
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:4:51-59), then e is passed to the function (at t.stag:4:63-71), then e is used (at t.stag:4:70-71), but must be at most gc, because it is stored through a [ptr[t]], which holds only gc values (a [ptr[t @@ external]] may hold external ones) (at t.stag:4:70-71) |}];
  provenance
    (ext
     ^ {|
let put (q : ptr[string @@ external]) (e : string @ external) = q.* <- e
let p = Ptr.make "a"
let alias u = put p (ext "b")
|}
    );
  [%expect
    {| This expression has type ptr[string] but an expression was expected of type ptr[string @@ external] |}]
;;

let%expect_test "a [ptr[t @@ external]] is given and reads external values" =
  run
    (ext
     ^ {|
let cell = Ptr.make_external "a"
let set u = cell.* <- ext "b"
let get u = cell.*
let read = set (); get ()
|}
    );
  [%expect
    {|
    val ext : string -> string @ external
    val cell : ptr[string @@ external]
    val set : 'a -> unit
    val get : 'a -> string @ external
    val read : string
    -- evaluated at compile time
    -- evaluated
    (ext <fun>)
    (cell (Ptr (String b)))
    (set <fun>)
    (get <fun>)
    (read (String b))
    |}];
  provenance
    (ext
     ^ {|
let cell = Ptr.make_external "a"
let get u = cell.*
let first (p : ptr[string @@ external]) = match p with *s -> s
let gc_only u = let s @ gc = get () in s
|}
    );
  [%expect
    {| This is external, because its cell may be given external values (at t.stag:5:12-18), then it is dereferenced (at t.stag:5:12-18), then the function returns it (at t.stag:5:12-18), then the function types are unified, then the function returns it (at t.stag:7:29-35), then it is bound by an annotated let (at t.stag:7:24-26), but must be at most gc, because it is annotated gc (at t.stag:7:24-26) |}];
  provenance
    (ext
     ^ {|
let first (p : ptr[string @@ external]) = match p with *s -> s
let gc_only (p : ptr[string @@ external]) = let s @ gc = first p in s
|}
    );
  [%expect
    {| This is external, because its cell may be given external values (at t.stag:4:56-57), then s is used (at t.stag:4:61-62), then the function returns it (at t.stag:4:42-62), then the function types are unified, then the function returns it (at t.stag:5:57-64), then it is bound by an annotated let (at t.stag:5:52-54), but must be at most gc, because it is annotated gc (at t.stag:5:52-54) |}]
;;

let%expect_test "stores into a record held by a [ptr[t @@ external]]" =
  provenance
    (ext
     ^ {|
type r = { name : string; n : int }
let cell = Ptr.make_external { name = "a"; n = 1 }
let rename u = cell.*.name <- ext "b"
let renumber u = cell.*.n <- 2
let plain = Ptr.make { name = "a"; n = 1 }
let renumber_plain u = plain.*.n <- 2
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    type r = { name : string; n : int }
    val cell : ptr[r @@ external]
    val rename : 'a -> unit
    val renumber : 'a -> unit
    val plain : ptr[r]
    val renumber_plain : 'a -> unit
    |}];
  provenance
    (ext
     ^ {|
type r = { name : string; n : int }
let plain = Ptr.make { name = "a"; n = 1 }
let rename_plain u = plain.*.name <- ext "b"
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:37-44), but must be at most gc, because it is stored through a [ptr[t]], which holds only gc values (a [ptr[t @@ external]] may hold external ones) (at t.stag:6:37-44) |}]
;;

let%expect_test "[Ptr.make] of an external value is a [ptr[t]]: stores into it \
                 are gc"
  =
  provenance
    (ext
     ^ {|
let c u = Ptr.make (ext "a")
let read u = (c ()).*
let store_gc u = (c ()).* <- "b"
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    val c : 'a -> ptr[string] @ nonportable external
    val read : 'a -> string @ nonportable external
    val store_gc : 'a -> unit
    |}];
  provenance (ext ^ {|
let store_ext u = (Ptr.make (ext "a")).* <- ext "b"
|});
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:4:44-51), but must be at most gc, because it is stored through a [ptr[t]], which holds only gc values (a [ptr[t @@ external]] may hold external ones) (at t.stag:4:44-51) |}];
  provenance
    (ext ^ {|
let typed u : ptr[string @@ external] = Ptr.make (ext "a")
|});
  [%expect
    {| This expression has type ptr[string] but an expression was expected of type ptr[string @@ external] |}]
;;

let%expect_test "stores through an external pointer to a gc cell" =
  (* A [ptr] is always to a cell stag manages, whatever its provenance. *)
  provenance
    {|
let ext (x : ptr[string]) = let y @ external = x in y
let f u = let p = ext (Ptr.make "a") in p.* <- Int.to_string 1
|};
  [%expect
    {|
    val ext : ptr[string] @ 'm0 -> ptr[string] @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    val f : 'a -> unit
    |}]
;;

let%expect_test "the modality is part of the type" =
  provenance {|
let f (p : ptr[string @@ external]) : ptr[string] = p
|};
  [%expect
    {| This expression has type ptr[string @@ external] but an expression was expected of type ptr[string] |}];
  (* Unannotated, a dereference is of a [ptr[t]]. *)
  provenance
    {|
let get p = p.*
let cell = Ptr.make_external "a"
let read u = get cell
|};
  [%expect
    {| This expression has type ptr[string @@ external] but an expression was expected of type ptr['_a] |}]
;;

let%expect_test "polymorphism over the pointee" =
  provenance
    (ext
     ^ {|
let get (p : ptr['a @@ external]) = p.*
let set (p : ptr['a @@ external]) v = p.* <- v
let set_plain p v = p.* <- v
let ints = Ptr.make_external 1
let strings = Ptr.make_external "a"
let use u = set ints 2; set strings (ext "b"); Int.add (get ints) 1
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and external <= 'm1
    val get : ptr['a @@ external] -> 'a @ nonportable external
    val set : ptr['a @@ external] @ 'm0 -> ('a -> unit) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val set_plain : ptr['a] @ 'm0 -> ('a @ 'm2 -> unit) @ 'm1 with 'm0 <= 'm1 and 'm2 <= gc and gc <= 'm1
    val ints : ptr[int @@ external]
    val strings : ptr[string @@ external]
    val use : 'a -> int
    |}]
;;

let%expect_test "printed types read back" =
  let source =
    {|
type r = { name : string; n : int }
let fn = Ptr.make_external (fun (x : int) -> x)
let pair = Ptr.make_external ("a", 1)
let record = Ptr.make_external { name = "a"; n = 1 }
let nested = Ptr.make_external (Ptr.make_external 1)
let in_list = [ Ptr.make_external "a" ]
|}
  in
  provenance source;
  [%expect
    {|
    type r = { name : string; n : int }
    val fn : ptr[(int @ '_m0 -> int @ '_m1) @@ external]
    val pair : ptr[string * int @@ external]
    val record : ptr[r @@ external]
    val nested : ptr[ptr[int @@ external] @@ external]
    val in_list : list[ptr[string @@ external]]
    |}];
  provenance
    {|
type r = { name : string; n : int }
let fn : ptr[(int -> int) @@ external] = Ptr.make_external (fun (x : int) -> x)
let pair : ptr[string * int @@ external] = Ptr.make_external ("a", 1)
let record : ptr[r @@ external] = Ptr.make_external { name = "a"; n = 1 }
let nested : ptr[ptr[int @@ external] @@ external] = Ptr.make_external (Ptr.make_external 1)
let in_list : list[ptr[string @@ external]] = [ Ptr.make_external "a" ]
type holder = { cell : ptr[string @@ external] }
type alias['a] = ptr['a @@ external]
let via_alias : alias[int] = Ptr.make_external 1
|};
  [%expect
    {|
    type r = { name : string; n : int }
    val fn : ptr[(int @ '_m0 -> int @ '_m1) @@ external]
    val pair : ptr[string * int @@ external]
    val record : ptr[r @@ external]
    val nested : ptr[ptr[int @@ external] @@ external]
    val in_list : list[ptr[string @@ external]]
    type holder = { cell : ptr[string @@ external] }
    type alias['a] = ptr['a @@ external]
    val via_alias : ptr[int @@ external]
    |}]
;;

let%expect_test "only [external], only on [ptr]'s argument" =
  provenance {|
let f (p : ptr[string @@ gc]) = p
|};
  [%expect
    {| The only modality of a pointer's type argument is [external], as in [ptr[string @@ external]] |}];
  provenance {|
let f (p : ptr[string @@ 'm]) = p
|};
  [%expect
    {| The only modality of a pointer's type argument is [external], as in [ptr[string @@ external]] |}];
  provenance {|
let f (p : list[string @@ external]) = p
|};
  [%expect
    {| A modality is only written on a pointer's type argument, as in [ptr[string @@ external]] |}];
  provenance {|
type t = { cell : ptr[list[string @@ external]] }
|};
  [%expect
    {| A modality is only written on a pointer's type argument, as in [ptr[string @@ external]] |}]
;;

let%expect_test "locality and stage as for [ptr[t]]" =
  provenance
    {|
let cell = Ptr.make_external (fun (x : int) -> x)
let local_store u = cell.* <- stack_ (fun (x : int) -> Int.add x 1)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:30-67), then it is part of the value (at t.stag:3:30-67), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:30-67) |}];
  provenance {|
let c @ portable = Ptr.make_external 1
|};
  [%expect
    {| This is nonportable, because Ptr.make allocates it at run time, then the function types are unified, then the function returns it (at t.stag:2:19-38), but must be at most portable, because it is annotated portable (at t.stag:2:8-16), then it is bound by an annotated let (at t.stag:2:8-16) |}]
;;

let%expect_test "in quotes" =
  run
    {|
let code = .< (fun (p : ptr[string @@ external]) -> p.*) (Ptr.make_external "a") >.
let spliced = .~code
|};
  [%expect
    {|
    val code : <string>{..'a}
    val spliced : string
    -- evaluated at compile time
    (code
     (Code
      (App
       (Fun (args (p/15))
        (annots
         ((p/15
           ((ty
             ((Con (<ptr> @ t.stag:2:24-27)
               ((Ty
                 ((Modality
                   ((Con (<string> @ t.stag:2:28-34) ()) @ t.stag:2:28-34)
                   ((Atoms (External)) @ t.stag:2:38-46))
                  @ t.stag:2:28-46))))
              @ t.stag:2:24-47))))))
        (body
         (Deref (Var (Bound (Local p/15)) : "ptr[string @@ external]") : string))
        : "ptr[string @@ external] -> string @ external")
       (App
        (Var (Persistent (Toplevel prelude.Ptr.make_external/16)) :
         "string -> ptr[string @@ external]")
        (Lit (String a) : string) : "ptr[string @@ external]")
       : string)))
    -- evaluated
    (code
     (Code
      (App
       (Fun (args (p/15))
        (annots
         ((p/15
           ((ty
             ((Con (<ptr> @ t.stag:2:24-27)
               ((Ty
                 ((Modality
                   ((Con (<string> @ t.stag:2:28-34) ()) @ t.stag:2:28-34)
                   ((Atoms (External)) @ t.stag:2:38-46))
                  @ t.stag:2:28-46))))
              @ t.stag:2:24-47))))))
        (body
         (Deref (Var (Bound (Local p/15)) : "ptr[string @@ external]") : string))
        : "ptr[string @@ external] -> string @ external")
       (App
        (Var (Persistent (Toplevel prelude.Ptr.make_external/16)) :
         "string -> ptr[string @@ external]")
        (Lit (String a) : string) : "ptr[string @@ external]")
       : string)))
    (spliced (String a))
    |}]
;;
