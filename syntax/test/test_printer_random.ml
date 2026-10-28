open! Import
open Syntax

(* Random syntax, of the forms the parser produces, printed at several widths
   and checked to round trip. *)

let loc txt = Located.create txt

module Gen = struct
  type t =
    { random : Random.State.t
    ; mutable size : int
    }

  let int t n = Random.State.int t.random n
  let bool t = Random.State.bool t.random
  let pick t xs = List.nth_exn xs (int t (List.length xs))

  let list t ~min ~max f =
    List.init (min + int t (max - min + 1)) ~f:(fun _ -> f t)
  ;;

  (* Each choice is weighted, and the recursive ones are only taken while
     there is size left, which they use up. *)
  let choose t ~leaves ~nodes =
    if t.size <= 0 || int t 4 = 0
    then (pick t leaves) t
    else (
      t.size <- t.size - 1;
      (pick t nodes) t)
  ;;

  let lident t = pick t [ "x"; "y"; "long_name"; "f" ]
  let uident t = pick t [ "A"; "B"; "Some_constructor" ]
  let label t = loc (pick t [ "a"; "b"; "long_label" ])
  let name t = loc (lident t)

  let path t ~last =
    Path.of_list_exn
      (List.init (int t 3 / 2) ~f:(fun _ -> pick t [ "M"; "N" ]) @ [ last t ])
  ;;

  let string t =
    pick t [ ""; "s"; "a\"b\\c\n\t\001é"; "|}"; "a long string literal" ]
  ;;

  let lit t : Lit.t =
    pick
      t
      [ Lit.Int 0
      ; Int 42
      ; Int (-1)
      ; Int Int.min_value
      ; Bool true
      ; Bool false
      ; String (string t)
      ; Char (Char.of_int_exn (int t 256))
      ; Unit
      ]
  ;;

  let rec row t ~entry : Type_expr.row =
    loc
      (match int t 3 with
       | 0 -> Type_expr.Empty
       | 1 -> Var (loc "r")
       | _ -> Extend (label t, entry t, row t ~entry))
  ;;

  let rec type_expr t : Type_expr.t =
    let unit = loc (Type_expr.Con (loc (Path.name "unit"), [])) in
    loc
      (choose
         t
         ~leaves:
           [ (fun t : Type_expr.desc -> Var (pick t [ "a"; "b" ]))
           ; (fun t -> Con (loc (path t ~last:lident), []))
           ; (fun _ -> Tuple [])
           ]
         ~nodes:
           [ (fun t ->
               Con
                 ( loc (path t ~last:lident)
                 , list t ~min:1 ~max:3 (fun t : Type_expr.arg ->
                     if bool t
                     then Ty (type_expr t)
                     else Row (row t ~entry:type_expr)) ))
           ; (fun t -> Arrow (type_expr t, type_expr t))
           ; (fun t -> Tuple (list t ~min:2 ~max:3 type_expr))
           ; (fun t ->
               Code { inner = type_expr t; context = row t ~entry:type_expr })
           ; (fun t -> Record (row t ~entry:type_expr))
           ; (fun t ->
               Variant
                 (row t ~entry:(fun t -> if bool t then unit else type_expr t)))
           ])
  ;;

  let rec pattern t : Pattern.t =
    loc
      (choose
         t
         ~leaves:
           [ (fun _ -> Pattern.Any)
           ; (fun t -> Var (lident t))
           ; (fun t -> Lit (lit t))
           ; (fun t -> Construct (loc (path t ~last:uident), []))
           ; (fun t -> Poly_variant (label t, None))
           ]
         ~nodes:
           [ (fun t -> Tuple (list t ~min:2 ~max:3 pattern))
           ; (fun t -> Construct (loc (path t ~last:uident), [ non_tuple t ]))
           ; (fun t ->
               Construct
                 (loc (path t ~last:uident), list t ~min:2 ~max:3 pattern))
           ; (fun t -> Poly_variant (label t, Some (pattern t)))
           ; (fun t ->
               Record
                 (list t ~min:1 ~max:3 (fun t ->
                    loc (path t ~last:lident), pattern t)))
           ; (fun t ->
               Poly_record
                 { fields = list t ~min:0 ~max:3 (fun t -> label t, pattern t)
                 ; rest = pick t [ Pattern.Rest.Closed; Open; Bind (name t) ]
                 })
           ])

  and non_tuple t =
    match pattern t with
    | { txt = Tuple _; _ } -> non_tuple t
    | p -> p
  ;;

  let binding t : Binding.t = name t, Option.some_if (bool t) (type_expr t)

  let rec expr t : Expr.t =
    loc
      (choose
         t
         ~leaves:
           [ (fun t -> Expr.Lit (lit t))
           ; (fun t -> Var (path t ~last:lident, None))
           ; (fun t -> Var (path t ~last:lident, Some (type_expr t)))
           ; (fun t -> Construct (loc (path t ~last:uident), []))
           ; (fun t -> Poly_variant (label t, None))
           ; (fun t -> Expect (loc (string t)))
           ; (fun _ -> Poly_record [])
           ]
         ~nodes:
           [ (fun t -> App (expr t, expr t))
           ; (fun t -> App (expr t, expr t))
           ; (fun t ->
               Let
                 ( name t
                 , list t ~min:0 ~max:2 binding
                 , Option.some_if (bool t) (type_expr t)
                 , expr t
                 , expr t ))
           ; (fun t -> Let (loc "_", [], None, expr t, expr t))
           ; (fun t ->
               Splice
                 { expr = expr t
                 ; subs =
                     Option.some_if
                       (bool t)
                       (list t ~min:0 ~max:2 (fun t ->
                          name t, Option.some_if (bool t) (expr t)))
                 })
           ; (fun t -> Quote (expr t))
           ; (fun t -> Tuple (list t ~min:2 ~max:3 expr))
           ; (fun t -> Construct (loc (path t ~last:uident), [ non_tuple t ]))
           ; (fun t ->
               Construct (loc (path t ~last:uident), list t ~min:2 ~max:3 expr))
           ; (fun t ->
               Record
                 (list t ~min:1 ~max:3 (fun t ->
                    loc (path t ~last:lident), expr t)))
           ; (fun t -> Field (expr t, loc (path t ~last:lident)))
           ; (fun t ->
               Match (expr t, list t ~min:1 ~max:3 (fun t -> pattern t, expr t)))
           ; (fun t ->
               Poly_record (list t ~min:1 ~max:3 (fun t -> label t, expr t)))
           ; (fun t -> Poly_field (expr t, label t))
           ; (fun t ->
               Poly_extend
                 (list t ~min:1 ~max:3 (fun t -> label t, expr t), expr t))
           ; (fun t -> Poly_restrict (expr t, label t))
           ; (fun t ->
               Poly_update
                 (expr t, list t ~min:1 ~max:3 (fun t -> label t, expr t)))
           ; (fun t -> Poly_variant (label t, Some (expr t)))
           ])

  (* A tuple argument would be taken as several. *)
  and non_tuple t =
    match expr t with
    | { txt = Tuple _; _ } -> non_tuple t
    | e -> e
  ;;

  let type_decl t : Type_decl.t =
    { name = name t
    ; params =
        list t ~min:0 ~max:2 (fun t ->
          loc (if bool t then Type_decl.Param.Ty "a" else Row "r"))
    ; kind =
        (match int t 4 with
         | 0 -> Abstract
         | 1 -> Alias (type_expr t)
         | 2 ->
           Variant
             (list t ~min:0 ~max:3 (fun t ->
                loc (uident t), list t ~min:0 ~max:3 type_expr))
         | _ -> Record (list t ~min:1 ~max:3 (fun t -> name t, type_expr t)))
    }
  ;;

  let rec toplevel t : Toplevel.t =
    loc
      (match int t 6 with
       | 0 ->
         Toplevel.Let
           ( name t
           , list t ~min:0 ~max:2 binding
           , Option.some_if (bool t) (type_expr t)
           , expr t )
       | 1 | 2 -> Type (type_decl t)
       | 3 -> Expect_test { name = loc (string t); body = expr t }
       | 4 when t.size > 0 ->
         t.size <- t.size - 1;
         Module (loc (uident t), list t ~min:0 ~max:3 toplevel)
       | _ -> Let (name t, [], None, expr t))
  ;;
end

let%expect_test "random syntax round trips" =
  let random = Random.State.make [| 0 |] in
  let widths = [ 80; 30; 1 ] in
  for size = 0 to 5000 do
    let gen () = { Gen.random; size = size % 60 } in
    Round_trip.type_expr ~widths (Gen.type_expr (gen ()));
    Round_trip.pattern ~widths (Gen.pattern (gen ()));
    Round_trip.expr ~widths (Gen.expr (gen ()));
    let gen = gen () in
    Round_trip.program ~widths (List.init 3 ~f:(fun _ -> Gen.toplevel gen))
  done;
  [%expect {| |}]
;;
