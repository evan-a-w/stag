open! Import

let loc ~file (line, col, offset) (line', col', offset') =
  Loc.create
    ~file
    ~start:#{ line; col; offset }
    ~stop:#{ line = line'; col = col'; offset = offset' }
;;

let%expect_test "files are interned, and ordered by name, not by interning" =
  let z = Loc.File.of_string "zz_interned_first.stag" in
  let a = Loc.File.of_string "aa_interned_second.stag" in
  let id (file : Loc.File.t) : int = Stdlib.Obj.magic file in
  print_s
    [%sexp
      { same =
          (phys_equal z (Loc.File.of_string "zz_interned_first.stag") : bool)
      ; z_interned_first = (id z < id a : bool)
      ; sorted =
          (List.sort [ z; a ] ~compare:Loc.File.compare : Loc.File.t list)
      }];
  [%expect
    {|
    ((same true) (z_interned_first true)
     (sorted (aa_interned_second.stag zz_interned_first.stag)))
    |}]
;;

let%expect_test "positions are held inline, without pointers" =
  let l = loc ~file:"a.stag" (1, 0, 0) (2, 3, 10) in
  print_s
    [%sexp
      { words = (Obj.reachable_words (Obj.repr l) : int)
      ; string = (Loc.to_string l : string)
      ; file = (Loc.file_name l : string)
      }];
  [%expect {| ((words 8) (string a.stag:1:0-2:3) (file a.stag)) |}]
;;

let%expect_test "compare, merge and sort" =
  let a1 = loc ~file:"a.stag" (1, 0, 0) (1, 4, 4) in
  let a2 = loc ~file:"a.stag" (2, 0, 10) (2, 5, 15) in
  let b1 = loc ~file:"b.stag" (1, 0, 0) (1, 1, 1) in
  print_s [%sexp (Loc.sort_by [ b1; a2; a1 ] ~f:Fn.id : Loc.t list)];
  print_s [%sexp (Loc.merge a2 a1 : Loc.t), (Loc.merge Loc.none b1 : Loc.t)];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Loc.merge a1 b1) : Loc.t Or_error.t)
      , (Loc.equal a1 (loc ~file:"a.stag" (1, 0, 0) (1, 4, 4)) : bool)
      , (Loc.hash a1 = Loc.hash (loc ~file:"a.stag" (1, 0, 0) (1, 4, 4)) : bool)
      , (Loc.is_none (Loc.start_of_file (Loc.File.of_string "_none_")) : bool)];
  [%expect
    {|
    (a.stag:1:0-4 a.stag:2:0-5 b.stag:1:0-1)
    (a.stag:1:0-2:5 b.stag:1:0-1)
    ((Error ("Loc.merge: different files" (a.file a.stag) (b.file b.stag))) true
     true true)
    |}]
;;
