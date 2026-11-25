open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (Parser.Error.to_string error : string)]
;;

(* Each path [Decl_paths.map] gives [f], with where it is. *)
let show source =
  List.iter (parse source) ~f:(fun toplevel ->
    ignore
      (Decl_paths.map toplevel ~f:(fun namespace ~modules ~locals path ->
         print_s
           [%message
             ""
               ~_:(path.txt : Syntax.Decl_path.t)
               (namespace : Decl_paths.Namespace.t)
               (modules : string list)
               (locals : String.Set.t)];
         path)
       : Syntax.Toplevel.t))
;;

let%expect_test "a let binds its name and parameters in its value, its name in \
                 its body"
  =
  show "let a = let f x = (y, f, x) in f";
  [%expect
    {|
    (y (namespace Value) (modules ()) (locals (f x)))
    (f (namespace Value) (modules ()) (locals (f x)))
    (x (namespace Value) (modules ()) (locals (f x)))
    (f (namespace Value) (modules ()) (locals (f)))
    |}]
;;

let%expect_test "a let mut binds its name only in its body" =
  show "let a = let mut m = m in m";
  [%expect
    {|
    (m (namespace Value) (modules ()) (locals ()))
    (m (namespace Value) (modules ()) (locals (m)))
    |}]
;;

let%expect_test "a function binds its parameters" =
  show "let a = fun x (y : t) -> (x, y, z)";
  [%expect
    {|
    (t (namespace Type) (modules ()) (locals ()))
    (x (namespace Value) (modules ()) (locals (x y)))
    (y (namespace Value) (modules ()) (locals (x y)))
    (z (namespace Value) (modules ()) (locals (x y)))
    |}]
;;

let%expect_test "a match arm binds its pattern's variables" =
  show
    "let a = match s with | C (x, { f = y }) -> x | { `a = z; .. r } -> r | \
     exception E w -> w";
  [%expect
    {|
    (s (namespace Value) (modules ()) (locals ()))
    (C (namespace Constructor) (modules ()) (locals ()))
    (f (namespace Field) (modules ()) (locals ()))
    (x (namespace Value) (modules ()) (locals (x y)))
    (r (namespace Value) (modules ()) (locals (r z)))
    (E (namespace Constructor) (modules ()) (locals ()))
    (w (namespace Value) (modules ()) (locals (w)))
    |}]
;;

let%expect_test "a toplevel binds its parameters, not its name" =
  show "let f x = (f, x) let%bench \"b\" n in [ k ] = (n, k)";
  [%expect
    {|
    (f (namespace Value) (modules ()) (locals (x)))
    (x (namespace Value) (modules ()) (locals (x)))
    (k (namespace Value) (modules ()) (locals ()))
    (n (namespace Value) (modules ()) (locals (n)))
    (k (namespace Value) (modules ()) (locals (n)))
    |}]
;;

let%expect_test "modules enclose their declarations, which are not locals" =
  show "module M = struct let x = 1 module N = struct let y = x end end";
  [%expect {| (x (namespace Value) (modules (M N)) (locals ())) |}]
;;

let%expect_test "a type splice sees the locals around it" =
  show "let f x = (x : .~(t x))";
  [%expect
    {|
    (x (namespace Value) (modules ()) (locals (x)))
    (t (namespace Value) (modules ()) (locals (x)))
    (x (namespace Value) (modules ()) (locals (x)))
    |}]
;;

let%expect_test "a struct quote's declarations are locals, in no module" =
  show
    {|
module M = struct
  let f x =
    .< struct
      let y = x
      type t = .~(k x)
      module N = struct let z = y end
      let w = (y, z)
    end >.
end
|};
  [%expect
    {|
    (x (namespace Value) (modules (M)) (locals (x)))
    (k (namespace Value) (modules (M)) (locals (x y)))
    (x (namespace Value) (modules (M)) (locals (x y)))
    (y (namespace Value) (modules (M)) (locals (x y)))
    (y (namespace Value) (modules (M)) (locals (x y)))
    (z (namespace Value) (modules (M)) (locals (x y)))
    |}]
;;

let%expect_test "expansions and list literals are not visited" =
  show {|
include%inline .~(gen (type t))
let x = y
end%inline
let l = [ a ]
|};
  [%expect
    {|
    (gen (namespace Value) (modules ()) (locals ()))
    (t (namespace Type) (modules ()) (locals ()))
    (a (namespace Value) (modules ()) (locals ()))
    |}]
;;
