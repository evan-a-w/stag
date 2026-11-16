open! Import

let loc ~file (line, col, offset) (line', col', offset') =
  Loc.create
    ~file
    ~start:#{ line; col; offset }
    ~stop:#{ line = line'; col = col'; offset = offset' }
;;

let%expect_test "files are interned, and ordered by name" =
  let b = Loc.File.of_string "b.stag" in
  let a = Loc.File.of_string "a.stag" in
  print_s
    [%sexp
      { same = (phys_equal b (Loc.File.of_string "b.stag") : bool)
      ; a_before_b = (Loc.File.compare a b < 0 : bool)
      ; names = ([ a; b ] : Loc.File.t list)
      }];
  [%expect {| ((same true) (a_before_b true) (names (a.stag b.stag))) |}]
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
      , (Loc.is_none (Loc.start_of_file "_none_") : bool)];
  [%expect
    {|
    (a.stag:1:0-4 a.stag:2:0-5 b.stag:1:0-1)
    (a.stag:1:0-2:5 b.stag:1:0-1)
    ((Error ("Loc.merge: different files" (a.file a.stag) (b.file b.stag))) true
     true true)
    |}]
;;
