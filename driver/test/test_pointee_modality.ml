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
    {| This is external, because it is annotated external (at t.stag:4:51-59), then e is passed to the function (at t.stag:4:63-71), then e is used (at t.stag:4:70-71), but must be at most gc, because it is stored into a cell, which holds only what fits its modality (at t.stag:4:70-71) |}];
  provenance
    (ext
     ^ {|
let put (q : ptr[string @@ external]) (e : string @ external) = q.* <- e
let p = Ptr.make "a"
let alias u = put p (ext "b")
|}
    );
  [%expect
    {|
    This expression has type ptr[string] but an expression was expected of type ptr[string @@ external]
    Their modes differ: This is external, because the types are unified, but must be at most gc, because p is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default)
    |}]
;;

let%expect_test "a [ptr[t @@ external]] is given and reads external values" =
  run
    (ext
     ^ {|
let cell : ptr[string @@ external] = Ptr.make "a"
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
let first (p : ptr[string @@ external]) = match p with *s -> s
let gc_only (p : ptr[string @@ external]) = let s @ gc = first p in s
|}
    );
  [%expect
    {| This is external, because it is read from a pointer's cell, which may hold it (at t.stag:4:56-57), then s is used (at t.stag:4:61-62), then the function returns it (at t.stag:4:42-62), then the function returns it (at t.stag:5:57-64), then it is bound by an annotated let (at t.stag:5:52-54), but must be at most gc, because it is annotated gc (at t.stag:5:52-54) |}]
;;

let%expect_test "[Ptr.make] infers what its cell holds" =
  (* From the initial value, from stores, and from reads. *)
  provenance
    (ext
     ^ {|
let from_init u = Ptr.make (ext "a")
let from_store u = let p = Ptr.make "a" in p.* <- ext "b"; p
let from_read u = let p = Ptr.make "a" in let s @ gc = p.* in p
let unconstrained u = Ptr.make "a"
let weak = Ptr.make "a"
let weak_ext = Ptr.make (ext "a")
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val from_init : 'a @ _ -> ptr[string @@ 'm1] @ 'm0 with external <= 'm1 and nonportable gc <= 'm0
    val from_store : 'a @ _ -> ptr[string @@ 'm1] @ 'm0 with external <= 'm1 and nonportable gc <= 'm0
    val from_read : 'a @ _ -> ptr[string @@ 'm1] @ 'm0 with 'm1 <= gc and nonportable gc <= 'm0
    val unconstrained : 'a @ _ -> ptr[string @@ _] @ 'm0 with nonportable gc <= 'm0
    val weak : ptr[string]
    val weak_ext : ptr[string @@ external]
    |}];
  (* A toplevel cell not generalized holds gc values unless its definition says
     otherwise: later uses can't change it. *)
  provenance
    (ext ^ {|
let weak = Ptr.make "a"
let later u = weak.* <- ext "b"
|});
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:5:24-31), then it is stored into a cell, which holds only what fits its modality (at t.stag:5:24-31), but must be at most gc, because weak is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default) |}];
  provenance
    (ext
     ^ {|
let clash u = let p = Ptr.make (ext "a") in let s @ gc = p.* in s
|});
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:4:31-40), then it is passed to a function (at t.stag:4:22-40), then Ptr.make holds it, then it is read from a pointer's cell, which may hold it (at t.stag:4:57-60), then it is dereferenced (at t.stag:4:57-60), then it is bound by an annotated let (at t.stag:4:52-54), but must be at most gc, because it is annotated gc (at t.stag:4:52-54) |}]
;;

let%expect_test "stores into a record held by a [ptr[t @@ external]]" =
  provenance
    (ext
     ^ {|
type r = { name : string; n : int }
let cell : ptr[r @@ external] = Ptr.make { name = "a"; n = 1 }
let rename u = cell.*.name <- ext "b"
let renumber u = cell.*.n <- 2
let plain = Ptr.make { name = "a"; n = 1 }
let renumber_plain u = plain.*.n <- 2
let rename_any (p : ptr[r @@ 'm]) s = p.*.name <- s
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    type r = { name : string; n : int }
    val cell : ptr[r @@ external]
    val rename : 'a @ _ -> unit @ _
    val renumber : 'a @ _ -> unit @ _
    val plain : ptr[r]
    val renumber_plain : 'a @ _ -> unit @ _
    val rename_any : ptr[r @@ 'm1] @ 'm0 -> (string @ 'm3 -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= global and gc <= 'm2
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
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:6:37-44), then it is stored into a cell, which holds only what fits its modality (at t.stag:6:37-44), but must be at most gc, because plain is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default) |}]
;;

let%expect_test "stores through an external pointer to a gc cell" =
  (* A [ptr] is always to a cell stag manages, whatever its own provenance. *)
  provenance
    {|
let ext (x : ptr[string]) = let y @ external = x in y
let f u = let p = ext (Ptr.make "a") in p.* <- Int.to_string 1
|};
  [%expect
    {|
    val ext : ptr[string] @ 'm0 -> ptr[string] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val f : 'a @ _ -> unit @ _
    |}]
;;

let%expect_test "what a cell holds is part of its type" =
  provenance {|
let f (p : ptr[string @@ external]) : ptr[string] = p
|};
  [%expect
    {|
    This expression has type ptr[string @@ external] but an expression was expected of type ptr[string]
    Their modes differ: The modes gc and external must be equal, because the types are unified
    |}];
  provenance {|
let f (p : ptr[string @@ static]) : ptr[string @@ gc] = p
|};
  [%expect
    {|
    This expression has type ptr[string @@ static] but an expression was expected of type ptr[string]
    Their modes differ: The modes gc and static must be equal, because the types are unified
    |}];
  provenance {|
let f (p : ptr[string @@ 'm]) : ptr[string @@ gc] = p
|};
  [%expect
    {| val f : ptr[string] @ 'm0 -> ptr[string] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) |}]
;;

let%expect_test "polymorphism over what a cell holds" =
  provenance
    (ext
     ^ {|
let get p = p.*
let set p v = p.* <- v
let swap p q = let v = p.* in p.* <- q.*; q.* <- v
let ints = Ptr.make 1
let strings : ptr[string @@ external] = Ptr.make "a"
let plain = Ptr.make "a"
let use u =
  set ints 2; set strings (ext "b"); set plain "c";
  let e = get strings in
  let g @ gc = get plain in
  Int.add (get ints) 1
|}
    );
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val get : ptr['a @@ 'm0] @ _ -> 'a @ 'm1 with 'm0 <= 'm1
    val set : ptr['a @@ 'm1] @ 'm0 -> ('a @ 'm3 -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and 'm3 <= 'm1 and 'm3 <= global and gc <= 'm2
    val swap : ptr['a @@ 'm1] @ 'm0 -> (ptr['a @@ 'm1] @ _ -> unit @ _) @ 'm2 with 'm0 <= 'm2 and portable <= 'm1 and gc <= 'm2
    val ints : ptr[int]
    val strings : ptr[string @@ external]
    val plain : ptr[string]
    val use : 'a @ _ -> int @ _
    |}];
  provenance
    (ext
     ^ {|
let get p = p.*
let strings : ptr[string @@ external] = Ptr.make "a"
let bad u = let g @ gc = get strings in g
|}
    );
  [%expect
    {| This is external, because the types are unified, then it is read from a pointer's cell, which may hold it (at t.stag:4:12-15), then it is dereferenced (at t.stag:4:12-15), then the function returns it (at t.stag:4:12-15), then the function returns it (at t.stag:6:25-36), then it is bound by an annotated let (at t.stag:6:20-22), but must be at most gc, because it is annotated gc (at t.stag:6:20-22) |}];
  provenance
    (ext
     ^ {|
let set p v = p.* <- v
let plain = Ptr.make "a"
let bad u = set plain (ext "b")
|}
    );
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function returns it (at t.stag:6:22-31), then it is passed to a function (at t.stag:6:12-31), but must be at most gc, because plain is not generalized, so what its cells hold is fixed by its definition (nonportable gc by default), then the types are unified, then it is stored into a cell, which holds only what fits its modality (at t.stag:4:21-22), then v is used (at t.stag:4:21-22), then v is passed to the function (at t.stag:4:14-22) |}]
;;

let%expect_test "printed types read back" =
  let source =
    {|
type r = { name : string; n : int }
let fn : ptr[(int -> int) @@ external] = Ptr.make (fun (x : int) -> x)
let pair : ptr[string * int @@ external] = Ptr.make ("a", 1)
let record : ptr[r @@ static] = Ptr.make { name = "a"; n = 1 }
let nested : ptr[ptr[int @@ external] @@ external] = Ptr.make (Ptr.make 1)
let in_list : list[ptr[string @@ external]] = [ Ptr.make "a" ]
type holder = { cell : ptr[string @@ external] }
type alias['a] = ptr['a @@ external]
type poly['a, 'b] = ptr['a]
let via_alias : alias[int] = Ptr.make 1
let get (p : ptr['a @@ 'm]) = p.*
let id_cell (p : ptr['a @@ 'm]) = p
|}
  in
  provenance source;
  [%expect
    {|
    type r = { name : string; n : int }
    val fn : ptr[(int -> int) @@ external]
    val pair : ptr[string * int @@ external]
    val record : ptr[r @@ static]
    val nested : ptr[ptr[int @@ external] @@ external]
    val in_list : list[ptr[string @@ external]]
    type holder = { cell : ptr[string @@ external] }
    type alias['a] = ptr['a @@ external]
    type poly['a, 'b] = ptr['a]
    val via_alias : ptr[int @@ external]
    val get : ptr['a @@ 'm0] @ _ -> 'a @ 'm1 with 'm0 <= 'm1
    val id_cell : ptr['a @@ 'm1] @ 'm0 -> ptr['a @@ 'm1] @ 'm2 with 'm0 <= 'm2 and release(locality('m0)) <= locality('m2)
    |}]
;;

let%expect_test "only provenance, only on [ptr]'s argument" =
  provenance {|
let f (p : ptr[string @@ local]) = p
|};
  [%expect
    {| A mode argument is on stage and provenance only ([const], [portable], [nonportable], [static], [gc], [external]) or a mode variable, as in [ptr[string @@ external]]: a pointer's cell is always [global], and a type's mode parameters relate only stage and provenance |}];
  provenance {|
let f (p : ptr[string @@ portable]) = p
|};
  [%expect
    {| val f : ptr[string @@ portable] @ 'm0 -> ptr[string @@ portable] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) |}];
  provenance {|
let f (p : list[string @@ external]) = p
|};
  [%expect
    {| A modality is only written on a record field's type, a constructor's argument or a pointer's type argument, as in [ptr[string @@ external]] |}];
  provenance {|
type t = { cell : ptr[list[string @@ external]] }
|};
  [%expect
    {| A modality is only written on a record field's type, a constructor's argument or a pointer's type argument, as in [ptr[string @@ external]] |}]
;;

let%expect_test "locality and stage as for [ptr[t]]" =
  provenance
    {|
let cell : ptr[(int -> int) @@ external] = Ptr.make (fun (x : int) -> x)
let local_store u = cell.* <- stack_ (fun (x : int) -> Int.add x 1)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:30-67), then it is part of the value (at t.stag:3:30-67), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:30-67) |}];
  provenance {|
let c : ptr[int @@ external] @ portable = Ptr.make 1
|};
  [%expect
    {| This is nonportable, because Ptr.make allocates it at run time, then the function returns it (at t.stag:2:42-52), but must be at most portable, because it is annotated portable (at t.stag:2:31-39), then it is bound by an annotated let (at t.stag:2:31-39) |}]
;;

let%expect_test "in quotes" =
  run
    (ext
     ^ {|
let code = .< (fun (p : ptr[string @@ external]) -> p.*) (Ptr.make (ext "a")) >.
let spliced = .~code
let poly = .< fun (p : ptr[string @@ 'm]) (v : string) -> p.* <- v; p.* >.
let use_poly = .~poly (Ptr.make "a") "b"
|}
    );
  [%expect
    {|
    val ext : string -> string @ external
    val code : <string>{..'a}
    val spliced : string
    val poly : <ptr[string] -> string -> string>{..'a}
    val use_poly : string
    -- evaluated at compile time
    (code
     (Code
      (App
       (Fun (args (p/19))
        (annots
         ((p/19
           ((ty
             ((Con (<ptr> @ t.stag:4:24-27)
               ((Ty
                 ((Modality
                   ((Con (<string> @ t.stag:4:28-34) ()) @ t.stag:4:28-34)
                   ((Atoms (External)) @ t.stag:4:38-46))
                  @ t.stag:4:28-46))))
              @ t.stag:4:24-47))))))
        (body
         (Deref (Var (Bound (Local p/19)) : "ptr[string @@ external]") : string))
        : "ptr[string @@ external] -> string @ external")
       (App
        (Var (Persistent (Toplevel prelude.Ptr.make/20)) :
         "string @ external -> ptr[string @@ external]")
        (App (Var (Persistent (Toplevel ext/4)) : "string -> string @ external")
         (Lit (String a) : string) : string)
        : "ptr[string @@ external]")
       : string)))
    (poly
     (Code
      (Fun (args (p/22 v/23))
       (annots
        ((p/22
          ((ty
            ((Con (<ptr> @ t.stag:6:23-26)
              ((Ty
                ((Modality
                  ((Con (<string> @ t.stag:6:27-33) ()) @ t.stag:6:27-33)
                  ((Var m) @ t.stag:6:37-39))
                 @ t.stag:6:27-39))))
             @ t.stag:6:23-40))))
         (v/23 ((ty ((Con (<string> @ t.stag:6:47-53) ()) @ t.stag:6:47-53))))))
       (body
        (Seq
         (Store (Deref (Var (Bound (Local p/22)) : ptr[string]) : string)
          (Var (Bound (Local v/23)) : string) : unit)
         (Deref (Var (Bound (Local p/22)) : ptr[string]) : string) : string))
       : "ptr[string] -> string -> string")))
    -- evaluated
    (ext <fun>)
    (code
     (Code
      (App
       (Fun (args (p/19))
        (annots
         ((p/19
           ((ty
             ((Con (<ptr> @ t.stag:4:24-27)
               ((Ty
                 ((Modality
                   ((Con (<string> @ t.stag:4:28-34) ()) @ t.stag:4:28-34)
                   ((Atoms (External)) @ t.stag:4:38-46))
                  @ t.stag:4:28-46))))
              @ t.stag:4:24-47))))))
        (body
         (Deref (Var (Bound (Local p/19)) : "ptr[string @@ external]") : string))
        : "ptr[string @@ external] -> string @ external")
       (App
        (Var (Persistent (Toplevel prelude.Ptr.make/20)) :
         "string @ external -> ptr[string @@ external]")
        (App (Var (Persistent (Toplevel ext/4)) : "string -> string @ external")
         (Lit (String a) : string) : string)
        : "ptr[string @@ external]")
       : string)))
    (spliced (String a))
    (poly
     (Code
      (Fun (args (p/22 v/23))
       (annots
        ((p/22
          ((ty
            ((Con (<ptr> @ t.stag:6:23-26)
              ((Ty
                ((Modality
                  ((Con (<string> @ t.stag:6:27-33) ()) @ t.stag:6:27-33)
                  ((Var m) @ t.stag:6:37-39))
                 @ t.stag:6:27-39))))
             @ t.stag:6:23-40))))
         (v/23 ((ty ((Con (<string> @ t.stag:6:47-53) ()) @ t.stag:6:47-53))))))
       (body
        (Seq
         (Store (Deref (Var (Bound (Local p/22)) : ptr[string]) : string)
          (Var (Bound (Local v/23)) : string) : unit)
         (Deref (Var (Bound (Local p/22)) : ptr[string]) : string) : string))
       : "ptr[string] -> string -> string")))
    (use_poly (String b))
    |}]
;;
