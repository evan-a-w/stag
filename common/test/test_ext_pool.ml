open! Import
module Float_u = Stdlib_upstream_compatible.Float_u
module Int64_u = Stdlib_upstream_compatible.Int64_u

module Triple = struct
  type t =
    #{ a : int
     ; b : int
     ; c : int
     }
end

module%template Triples =
  Ext_pool.Make [@kind immediate & immediate & immediate] (Triple)

module%template Floats = Ext_pool.Make [@kind float64] (struct
    type t = float#
  end)

module%template Int64s = Ext_pool.Make [@kind bits64] (struct
    type t = Int64_u.t
  end)

module%template Ints = Ext_pool.Make [@kind immediate] (Int)

let triple (x : Triple.t) = [%sexp (x.#a : int), (x.#b : int), (x.#c : int)]

let print_triples pool =
  let contents = ref [] in
  Triples.iter pool ~f:(fun handle x ->
    contents
    := [%sexp (handle : Triples.Handle.t), (triple x : Sexp.t)] :: !contents);
  print_s
    [%sexp
      { length = (Triples.length pool : int)
      ; contents = (List.rev !contents : Sexp.t list)
      }]
;;

let%expect_test "alloc, get and set an unboxed product" =
  Triples.with_ ~f:(fun pool ->
    let h0 = Triples.alloc pool #{ a = 1; b = 2; c = 3 } in
    let h1 = Triples.alloc pool #{ a = 4; b = 5; c = 6 } in
    let h2 = Triples.alloc pool #{ a = 7; b = 8; c = 9 } in
    Triples.set pool h1 #{ (Triples.get pool h1) with b = 50 };
    print_s [%sexp ([ h0; h1; h2 ] : Triples.Handle.t list)];
    print_s (triple (Triples.get pool h1));
    print_triples pool);
  [%expect
    {|
    ((0 1) (1 1) (2 1))
    (4 50 6)
    ((length 3) (contents (((0 1) (1 2 3)) ((1 1) (4 50 6)) ((2 1) (7 8 9)))))
    |}]
;;

let%expect_test "freed slots are reused, at a new generation" =
  Triples.with_ ~f:(fun pool ->
    let h0 = Triples.alloc pool #{ a = 1; b = 1; c = 1 } in
    let h1 = Triples.alloc pool #{ a = 2; b = 2; c = 2 } in
    Triples.free pool h0;
    print_s
      [%sexp (Triples.is_live pool h0 : bool), (Triples.is_live pool h1 : bool)];
    print_triples pool;
    let h0' = Triples.alloc pool #{ a = 3; b = 3; c = 3 } in
    print_s
      [%sexp
        (h0' : Triples.Handle.t)
        , (Triples.Handle.index h0' = Triples.Handle.index h0 : bool)
        , (Triples.Handle.equal h0 h0' : bool)
        , (Triples.is_live pool h0 : bool)];
    print_triples pool;
    (* The stale handle doesn't see the slot's new element. *)
    print_s
      [%sexp
        (Or_error.try_with (fun () -> triple (Triples.get pool h0))
         : Sexp.t Or_error.t)];
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Triples.free pool h0) : unit Or_error.t)];
    print_s
      [%sexp
        (Or_error.try_with (fun () ->
           Triples.set pool h0 #{ a = 0; b = 0; c = 0 })
         : unit Or_error.t)];
    print_triples pool);
  [%expect
    {|
    (false true)
    ((length 1) (contents (((1 1) (2 2 2)))))
    ((0 3) true false false)
    ((length 2) (contents (((0 3) (3 3 3)) ((1 1) (2 2 2)))))
    (Error ("Ext_pool: stale handle" (handle (0 1))))
    (Error ("Ext_pool: stale handle" (handle (0 1))))
    (Error ("Ext_pool: stale handle" (handle (0 1))))
    ((length 2) (contents (((0 3) (3 3 3)) ((1 1) (2 2 2)))))
    |}]
;;

let%expect_test "free list is last in, first out" =
  Ints.with_ ~f:(fun pool ->
    let handles = Array.init 5 ~f:(Ints.alloc pool) in
    List.iter [ 1; 3; 4 ] ~f:(fun i -> Ints.free pool handles.(i));
    let reused = Array.init 4 ~f:(fun i -> Ints.alloc pool (10 + i)) in
    print_s [%sexp (reused : Ints.Handle.t array)];
    let contents = ref [] in
    Ints.iter pool ~f:(fun handle x ->
      contents := (Ints.Handle.index handle, x) :: !contents);
    print_s
      [%sexp (List.rev !contents : (int * int) list), (Ints.length pool : int)]);
  [%expect
    {|
    ((4 3) (3 3) (1 3) (5 1))
    (((0 0) (1 12) (2 2) (3 11) (4 10) (5 13)) 6)
    |}]
;;

let%expect_test "clear invalidates every handle and reuses the storage" =
  Triples.with_ ~f:(fun pool ->
    let h0 = Triples.alloc pool #{ a = 1; b = 1; c = 1 } in
    let h1 = Triples.alloc pool #{ a = 2; b = 2; c = 2 } in
    let h2 = Triples.alloc pool #{ a = 3; b = 3; c = 3 } in
    Triples.free pool h1;
    Triples.clear pool;
    print_s
      [%sexp (List.map [ h0; h1; h2 ] ~f:(Triples.is_live pool) : bool list)];
    print_triples pool;
    let h0' = Triples.alloc pool #{ a = 4; b = 4; c = 4 } in
    let h1' = Triples.alloc pool #{ a = 5; b = 5; c = 5 } in
    print_s
      [%sexp
        ([ h0'; h1' ] : Triples.Handle.t list)
        , (List.map [ h0; h1; h2 ] ~f:(Triples.is_live pool) : bool list)];
    print_triples pool);
  [%expect
    {|
    (false false false)
    ((length 0) (contents ()))
    (((0 3) (1 3)) (false false false))
    ((length 2) (contents (((0 3) (4 4 4)) ((1 3) (5 5 5)))))
    |}]
;;

let%expect_test "many elements, through growth, free and reuse" =
  Ints.with_ ~f:(fun pool ->
    let handles = Array.init 1000 ~f:(fun i -> Ints.alloc pool (i * i)) in
    Array.iteri handles ~f:(fun i h -> if i % 3 = 0 then Ints.free pool h);
    let again = Array.init 334 ~f:(fun i -> Ints.alloc pool (-i)) in
    let ok =
      Array.for_alli handles ~f:(fun i h ->
        if i % 3 = 0 then not (Ints.is_live pool h) else Ints.get pool h = i * i)
      && Array.for_alli again ~f:(fun i h -> Ints.get pool h = -i)
    in
    let indices =
      Array.map again ~f:Ints.Handle.index |> Array.to_list |> Int.Set.of_list
    in
    print_s
      [%sexp
        { ok : bool
        ; length = (Ints.length pool : int)
        ; distinct_reused_indices = (Set.length indices : int)
        ; max_index = (Set.max_elt_exn indices : int)
        }]);
  [%expect
    {| ((ok true) (length 1000) (distinct_reused_indices 334) (max_index 999)) |}]
;;

let%expect_test "unboxed products and floats, grown past capacity, through \
                 free and clear"
  =
  Triples.with_ ~f:(fun pool ->
    let triple_of i : Triple.t = #{ a = i; b = -i; c = i * i } in
    let check handles ~live =
      List.for_all handles ~f:(fun (i, h) ->
        if live i
        then (
          let x = Triples.get pool h in
          x.#a = i && x.#b = -i && x.#c = i * i)
        else not (Triples.is_live pool h))
    in
    let first =
      List.init 40 ~f:(fun i -> i, Triples.alloc pool (triple_of i))
    in
    List.iter first ~f:(fun (i, h) -> if i % 4 = 0 then Triples.free pool h);
    let more =
      List.init 50 ~f:(fun i ->
        100 + i, Triples.alloc pool (triple_of (100 + i)))
    in
    let before_clear =
      check first ~live:(fun i -> i % 4 <> 0)
      && check more ~live:(fun _ -> true)
    in
    let length_before_clear = Triples.length pool in
    Triples.clear pool;
    let after =
      List.init 70 ~f:(fun i ->
        200 + i, Triples.alloc pool (triple_of (200 + i)))
    in
    print_s
      [%sexp
        { before_clear : bool
        ; length_before_clear : int
        ; after_clear =
            (check first ~live:(fun _ -> false)
             && check more ~live:(fun _ -> false)
             && check after ~live:(fun _ -> true)
             : bool)
        ; length = (Triples.length pool : int)
        ; max_index =
            (List.map after ~f:(fun (_, h) -> Triples.Handle.index h)
             |> List.max_elt ~compare
             : int option)
        }]);
  Floats.with_ ~f:(fun pool ->
    let handles =
      Array.init 100 ~f:(fun i -> Floats.alloc pool (Float_u.of_int i))
    in
    Array.iteri handles ~f:(fun i h -> if i % 2 = 0 then Floats.free pool h);
    let again =
      Array.init 60 ~f:(fun i -> Floats.alloc pool (Float_u.of_int (-i)))
    in
    let sum = ref 0. in
    Floats.iter pool ~f:(fun _ x -> sum := !sum +. Float_u.to_float x);
    print_s
      [%sexp
        { odd_kept =
            (Array.for_alli handles ~f:(fun i h ->
               if i % 2 = 0
               then not (Floats.is_live pool h)
               else
                 Float.equal
                   (Float_u.to_float (Floats.get pool h))
                   (Float.of_int i))
             : bool)
        ; again_kept =
            (Array.for_alli again ~f:(fun i h ->
               Float.equal
                 (Float_u.to_float (Floats.get pool h))
                 (Float.of_int (-i)))
             : bool)
        ; length = (Floats.length pool : int)
        ; sum = (!sum : float)
        }]);
  [%expect
    {|
    ((before_clear true) (length_before_clear 80) (after_clear true) (length 70)
     (max_index (69)))
    ((odd_kept true) (again_kept true) (length 110) (sum 730))
    |}]
;;

module Tiny =
  Ext_pool.For_testing.Make
    (struct
      let index_bits = 2
      let generation_bits = 3
    end)
    (Int)

let%expect_test "a stale handle is live again once its generation wraps around" =
  Tiny.with_ ~f:(fun pool ->
    let stale = Tiny.alloc pool 0 in
    Tiny.free pool stale;
    (* Generations are 3 bits, each reuse taking two: the fourth reuse of the
       slot has the stale handle's generation again. *)
    for reuse = 1 to 4 do
      let h = Tiny.alloc pool reuse in
      print_s
        [%sexp
          (reuse : int), (h : Tiny.Handle.t), (Tiny.is_live pool stale : bool)];
      if reuse < 4 then Tiny.free pool h
    done;
    print_s [%sexp (Tiny.get pool stale : int)]);
  [%expect
    {|
    (1 (0 3) false)
    (2 (0 5) false)
    (3 (0 7) false)
    (4 (0 1) true)
    4
    |}]
;;

let%expect_test "a pool holds at most 2^index_bits slots" =
  Tiny.with_ ~f:(fun pool ->
    let first = Tiny.alloc pool 0 in
    for i = 1 to 3 do
      ignore (Tiny.alloc pool i : _ Tiny.Handle.t)
    done;
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Tiny.alloc pool 4)
         : Tiny.Handle.t Or_error.t)];
    Tiny.free pool first;
    let reused = Tiny.alloc pool 5 in
    print_s [%sexp (reused : Tiny.Handle.t), (Tiny.length pool : int)]);
  [%expect {|
    (Error ("Ext_pool: full" (max_slots 4)))
    ((0 3) 4)
    |}]
;;

let%expect_test "configs that don't fit in a handle are rejected" =
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         let module _ =
           Ext_pool.For_testing.Make
             (struct
               let index_bits = 32
               let generation_bits = 32
             end)
             (Int)
         in
         ())
       : unit Or_error.t)];
  [%expect
    {| (Error ("Ext_pool: invalid config" (index_bits 32) (generation_bits 32))) |}]
;;

let%expect_test "float# and int64# elements" =
  Floats.with_ ~f:(fun pool ->
    let h = Floats.alloc pool #1.5 in
    Floats.set pool h (Float_u.add (Floats.get pool h) #2.25);
    print_s [%sexp (Float_u.to_float (Floats.get pool h) : float)]);
  Int64s.with_ ~f:(fun pool ->
    let h = Int64s.alloc pool #0x7fff_ffff_ffff_ffffL in
    print_s [%sexp (Int64_u.to_int64 (Int64s.get pool h) : int64)]);
  [%expect {|
    3.75
    9223372036854775807
    |}]
;;

let%expect_test "handles of a long-lived pool, kept with it" =
  let module Store = struct
    type t = T : 'brand Ints.t * 'brand Ints.Handle.t String.Table.t -> t
  end
  in
  let (T pool) = Ints.create () in
  let store = Store.T (pool, String.Table.create ()) in
  let add (Store.T (pool, names)) name x =
    Hashtbl.set names ~key:name ~data:(Ints.alloc pool x)
  in
  let find (Store.T (pool, names)) name =
    Hashtbl.find names name |> Option.map ~f:(Ints.get pool)
  in
  add store "a" 1;
  add store "b" 2;
  print_s [%sexp (find store "a" : int option), (find store "c" : int option)];
  [%expect {| ((1) ()) |}]
;;
