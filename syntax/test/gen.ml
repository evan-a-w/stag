open! Import
open Syntax

let loc txt = Located.create txt

type t =
  { random : Random.State.t
  ; mutable size : int
  ; deep : bool (** Whether to take the recursive choices while size is left. *)
  }

let int t n = Random.State.int t.random n
let bool t = Random.State.bool t.random
let pick t xs = List.nth_exn xs (int t (List.length xs))

let list t ~min ~max f =
  List.init (min + int t (max - min + 1)) ~f:(fun _ -> f t)
;;

(* Each choice is weighted, and the recursive ones are only taken while there is
   size left, which they use up. *)
let choose t ~leaves ~nodes =
  if t.size <= 0 || ((not t.deep) && int t 4 = 0)
  then (pick t leaves) t
  else (
    t.size <- t.size - 1;
    (pick t nodes) t)
;;

let lident t =
  pick t [ "x"; "y"; "long_name"; "f"; "x'"; "_x"; "in_"; "letx"; "with'" ]
;;

let uident t = pick t [ "A"; "B"; "Some_constructor"; "A'"; "B_" ]
let label t = loc (pick t [ "a"; "b"; "long_label"; "A"; "Some_tag"; "_x'" ])
let name t = loc (lident t)

let path t ~last =
  Path.of_list_exn
    (List.init (int t 3 / 2) ~f:(fun _ -> pick t [ "M"; "N" ]) @ [ last t ])
;;

(* Root or not by the path's hash, keeping the random sequence as it was before
   root paths. *)
let decl_path t ~last =
  let path = path t ~last in
  if String.hash (Path.to_string path) % 3 = 0
  then Decl_path.Root path
  else Path path
;;

let string t =
  pick
    t
    [ ""
    ; "s"
    ; "a\"b\\c\n\t\001é"
    ; "|}"
    ; "{|"
    ; "a |x} b |} c"
    ; "\r\n"
    ; "\255\254 bytes"
    ; "a long string literal"
    ; "\n\nleading and trailing blank lines\n\n"
    ; "\n  indented\n    more\n  less\n"
    ; "line one\nline two"
    ]
;;

(* Unlocated, as the rest. *)
let list_items items = List.map items ~f:(fun item -> Loc.none, item)

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

let rec row ?(label = label) t ~entry : Type_expr.row =
  loc
    (match int t 3 with
     | 0 -> Type_expr.Empty
     | 1 -> Var (loc "r")
     | _ -> Extend (label t, entry t, row ~label t ~entry))

and mode_expr t : Mode_expr.t =
  loc
    (if int t 4 = 0
     then Mode_expr.Var (pick t [ "m"; "n" ])
     else
       Atoms
         (List.filter_map
            [ [ Mode_expr.Atom.Global; Regional; Local ]
            ; [ Const; Portable; Nonportable ]
            ; [ Static; Gc; External ]
            ]
            ~f:(fun atoms -> Option.some_if (bool t) (pick t atoms))
          |> function
          | [] -> [ Mode_expr.Atom.Local ]
          | atoms -> atoms))

and moded t : Type_expr.t =
  let ty = type_expr t in
  if int t 3 = 0 then loc (Type_expr.Moded (ty, mode_expr t)) else ty

and maybe_modality t : Type_expr.t =
  let ty = type_expr t in
  if int t 4 = 0 then loc (Type_expr.Modality (ty, mode_expr t)) else ty

and annot t : Annot.t =
  { ty = Option.some_if (bool t) (annot_type t)
  ; mode = Option.some_if (int t 3 = 0) (mode_expr t)
  }

(* An annotation's type, maybe with mode constraints. *)
and annot_type t : Type_expr.t =
  let ty = type_expr t in
  if int t 3 = 0
  then (
    let var () = loc (Mode_expr.Var (pick t [ "m"; "n" ])) in
    let constraint_ () : Mode_expr.Constraint.t =
      match int t 3 with
      | 0 -> { lower = var (); upper = mode_expr t }
      | 1 -> { lower = mode_expr t; upper = var () }
      | _ -> { lower = var (); upper = var () }
    in
    loc
      (Type_expr.Constrained
         (ty, List.init (1 + int t 2) ~f:(fun _ -> constraint_ ()))))
  else ty

and type_expr t : Type_expr.t =
  let unit =
    loc (Type_expr.Con (loc (Decl_path.Root (Path.name "unit")), []))
  in
  loc
    (choose
       t
       ~leaves:
         [ (fun t : Type_expr.desc -> Var (pick t [ "a"; "b" ]))
         ; (fun t -> Con (loc (decl_path t ~last:lident), []))
         ; (fun _ -> Tuple [])
         ]
       ~nodes:
         [ (fun t ->
             Con
               ( loc (decl_path t ~last:lident)
               , list t ~min:1 ~max:3 (fun t : Type_expr.arg ->
                   match int t 5 with
                   | 0 | 1 -> Ty (type_expr t)
                   | 2 -> Ty (maybe_modality t)
                   | 3 -> Row (row t ~entry:type_expr)
                   | _ ->
                     (* One atom would read back as a type. *)
                     Mode
                       (loc
                          (Mode_expr.Atoms
                             [ pick
                                 t
                                 [ Mode_expr.Atom.Const; Portable; Nonportable ]
                             ; pick t [ Mode_expr.Atom.Static; Gc; External ]
                             ]))) ))
         ; (fun t -> Arrow (moded t, moded t))
         ; (fun t -> Tuple (list t ~min:2 ~max:3 type_expr))
         ; (fun t ->
             Code
               { inner = type_expr t
               ; context =
                   row ~label:name t ~entry:(fun t ->
                     match int t 6 with
                     | 0 -> loc (Type_expr.Mut (type_expr t))
                     | 1 -> loc (Type_expr.Immut (type_expr t))
                     | _ -> type_expr t)
               })
         ; (fun t -> Record (row t ~entry:type_expr))
         ; (fun t ->
             Variant
               (row t ~entry:(fun t -> if bool t then unit else type_expr t)))
         ; (fun t -> Splice (expr t))
         ])

and pattern t : Pattern.t =
  loc
    (choose
       t
       ~leaves:
         [ (fun _ -> Pattern.Any)
         ; (fun t -> Var (lident t))
         ; (fun t -> Lit (lit t))
         ; (fun t -> Construct (loc (decl_path t ~last:uident), []))
         ; (fun t -> Poly_variant (label t, None))
         ; (fun _ -> Construct (loc List_literal.nil, []))
         ]
       ~nodes:
         [ (fun t -> Tuple (list t ~min:2 ~max:3 pattern))
         ; (fun t ->
             (List_literal.pattern
                ~items:(list_items (list t ~min:1 ~max:4 pattern))
                ~close:Loc.none)
               .txt)
         ; (fun t ->
             Construct (loc List_literal.cons, [ pattern t; pattern t ]))
         ; (fun t -> Construct (loc (decl_path t ~last:uident), [ pattern t ]))
         ; (fun t ->
             Construct
               (loc (decl_path t ~last:uident), list t ~min:2 ~max:3 pattern))
         ; (fun t -> Poly_variant (label t, Some (pattern t)))
         ; (fun t ->
             Record
               (list t ~min:1 ~max:3 (fun t ->
                  loc (decl_path t ~last:lident), pattern t)))
         ; (fun t ->
             Poly_record
               { fields = list t ~min:0 ~max:3 (fun t -> label t, pattern t)
               ; rest = pick t [ Pattern.Rest.Closed; Open; Bind (name t) ]
               })
         ; (fun t -> Exception (pattern t))
         ])

and binding t : Binding.t = name t, annot t

and expr t : Expr.t =
  loc
    (choose
       t
       ~leaves:
         [ (fun t -> Expr.Lit (lit t))
         ; (fun t -> Var (decl_path t ~last:lident, Annot.none))
         ; (fun t ->
             Var
               ( decl_path t ~last:lident
               , if bool t
                 then { ty = Some (annot_type t); mode = None }
                 else { ty = None; mode = Some (mode_expr t) } ))
         ; (fun t -> Construct (loc (decl_path t ~last:uident), []))
         ; (fun t -> Poly_variant (label t, None))
         ; (fun t -> Expect (loc (string t)))
         ; (fun _ -> Poly_record [])
         ; (fun _ -> Construct (loc List_literal.nil, []))
         ]
       ~nodes:
         [ (fun t -> App (expr t, expr t))
         ; (fun t -> App (expr t, expr t))
         ; (fun t ->
             Let (name t, list t ~min:0 ~max:2 binding, annot t, expr t, expr t))
         ; (fun t -> Let (loc "_", [], Annot.none, expr t, expr t))
         ; (fun t -> Let_mut (name t, annot t, expr t, expr t))
         ; (fun t -> Assign (loc (decl_path t ~last:lident), expr t))
         ; (fun t -> Store (loc (Expr.Deref (expr t)), expr t))
         ; (fun t ->
             Store
               ( loc
                   (Expr.Field
                      (loc (Expr.Deref (expr t)), loc (decl_path t ~last:lident)))
               , expr t ))
         ; (fun t ->
             Store
               ( loc (Expr.Poly_field (loc (Expr.Deref (expr t)), label t))
               , expr t ))
         ; (fun t -> Fun (list t ~min:1 ~max:2 binding, expr t))
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
         ; (fun t -> Type (type_expr t))
         ; (fun t -> Tuple (list t ~min:2 ~max:3 expr))
         ; (fun t ->
             (List_literal.expr
                ~items:(list_items (list t ~min:1 ~max:4 expr))
                ~close:Loc.none)
               .txt)
         ; (fun t -> Construct (loc List_literal.cons, [ expr t; expr t ]))
         ; (fun t -> Construct (loc (decl_path t ~last:uident), [ expr t ]))
         ; (fun t ->
             Construct
               (loc (decl_path t ~last:uident), list t ~min:2 ~max:3 expr))
         ; (fun t ->
             Record
               (list t ~min:1 ~max:3 (fun t ->
                  loc (decl_path t ~last:lident), expr t)))
         ; (fun t -> Field (expr t, loc (decl_path t ~last:lident)))
         ; (fun t ->
             Record_update
               ( expr t
               , list t ~min:1 ~max:3 (fun t ->
                   loc (decl_path t ~last:lident), expr t) ))
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
         ; (fun t -> Struct_quote (list t ~min:0 ~max:3 toplevel))
         ; (fun t -> Stack (expr t))
         ; (fun t -> Exclave (expr t))
         ; (fun t -> Nontail (loc (Expr.App (expr t, expr t))))
         ])

and type_decl t : Type_decl.t =
  { name = name t
  ; params =
      list t ~min:0 ~max:2 (fun t ->
        loc
          (match int t 3 with
           | 0 -> Type_decl.Param.Ty "a"
           | 1 -> Row "r"
           | _ -> Mode "p"))
  ; kind =
      (match int t 5 with
       | 0 -> Abstract
       | 1 -> Alias (type_expr t)
       | 2 ->
         Variant
           (list t ~min:0 ~max:3 (fun t ->
              loc (uident t), list t ~min:0 ~max:3 maybe_modality))
       | 3 -> Record (list t ~min:1 ~max:3 (fun t -> name t, maybe_modality t))
       | _ -> Splice (Located.create (expr t)))
  }

and toplevel t : Toplevel.t =
  loc
    (match int t 11 with
     | 0 -> Toplevel.Let (name t, list t ~min:0 ~max:2 binding, annot t, expr t)
     | 1 | 2 -> Type (type_decl t)
     | 3 -> Expect_test { name = loc (string t); body = expr t }
     | 4 when t.size > 0 ->
       t.size <- t.size - 1;
       Module (loc (uident t), list t ~min:0 ~max:3 toplevel)
     | 5 -> Module_splice (loc (uident t), expr t, expansion t)
     | 6 -> Include (expr t, expansion t)
     | 7 -> Exception (loc (uident t), list t ~min:0 ~max:3 type_expr)
     | 8 ->
       Bench
         { name = loc (string t)
         ; param =
             (if bool t
              then
                Some
                  { Bench_param.name = name t
                  ; args = list t ~min:1 ~max:3 expr
                  }
              else None)
         ; body = expr t
         }
     | 9 ->
       External { name = name t; ty = type_expr t; symbol = loc (string t) }
     | 10 -> Let_mut (name t, annot t, expr t)
     | _ -> Let (name t, [], Annot.none, expr t))

and expansion t =
  if t.size > 0 && bool t
  then (
    t.size <- t.size - 1;
    Some (loc (list t ~min:0 ~max:3 toplevel)))
  else None
;;
