open! Import
module Value = Treewalk.Value
open Value

let show value =
  print_s (Value.sexp_of_t value);
  print_endline (Value.to_string_hum value)
;;

let%expect_test "pointers expose values, not object identities" =
  let cell = ref (Value.Int 3) in
  show (Tuple [ Ptr cell; Ptr cell ]);
  cell := Tuple [ Int 4; Ptr (ref (Bool true)) ];
  show (Ptr cell);
  [%expect
    {|
    (Tuple (Ptr 3) (Ptr 3))
    (Ptr.make 3, Ptr.make 3)
    (Ptr (Tuple 4 (Ptr true)))
    Ptr.make (4, Ptr.make true)
    |}]
;;

let%expect_test "shared trees have a finite printing budget" =
  let node =
    { Typed.Constructor_ref.type_name = "tree"; name = "Node"; index = 1 }
  in
  let value =
    ref (Value.Construct ({ node with name = "Leaf"; index = 0 }, []))
  in
  for _ = 1 to 60 do
    value := Construct (node, [ Ptr (ref (Tuple [ !value; !value ])) ])
  done;
  let sexp = Value.sexp_of_t !value in
  print_s [%message "sexp" ~chars:(Sexp.to_string sexp |> String.length : int)];
  print_endline (Value.to_string_hum ~max_length:200 !value);
  let human = Value.to_string_hum !value in
  print_s
    [%message
      "bounded"
        ~truncated:(String.is_suffix human ~suffix:"<truncated>" : bool)
        ~below_one_megabyte:(String.length human < 1_000_000 : bool)];
  [%expect
    {|
    (sexp (chars 600595))
    Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Ptr.make (Node (Pt...
    (bounded (truncated true) (below_one_megabyte true))
    |}]
;;

let%expect_test "cyclic pointers terminate, including through records" =
  let cell = ref Value.Unit in
  cell := Ptr cell;
  show (Ptr cell);
  let other = ref Value.Unit in
  cell := Poly_record (String.Map.singleton "next" (Ptr other));
  other := Tuple [ Ptr cell; Int 1 ];
  show (Ptr cell);
  [%expect
    {|
    (Ptr <cycle>)
    Ptr.make (<cycle>)
    (Ptr ((next (Ptr (Tuple <cycle> 1)))))
    Ptr.make {`next = Ptr.make (<cycle>, 1)}
    |}]
;;

let%expect_test "pointer output obeys length and depth bounds" =
  let value = Value.Ptr (ref (String "abcdefghijklmnop")) in
  List.iter [ 0; 1; 10; 20 ] ~f:(fun max_length ->
    print_s
      [%sexp
        (max_length : int), (Value.to_string_hum ~max_length value : string)]);
  let deep = ref (Value.Int 0) in
  for _ = 1 to 2000 do
    deep := Ptr (ref !deep)
  done;
  let human = Value.to_string_hum !deep in
  let sexp = Sexp.to_string (Value.sexp_of_t !deep) in
  print_s
    [%message
      "bounds"
        ~human_truncated:
          (String.is_substring human ~substring:"<truncated>" : bool)
        ~sexp_truncated:
          (String.is_substring sexp ~substring:"<truncated>" : bool)];
  [%expect
    {|
    (0 ...)
    (1 P...)
    (10 "Ptr.make \"...")
    (20 "Ptr.make \"abcdefghij...")
    (bounds (human_truncated true) (sexp_truncated true))
    |}]
;;
