open! Import

module Binder = struct
  type t =
    | Name of Syntax.Name.t
    | Ident of Typed.Ident.t

  let name = function
    | Name name -> name.txt
    | Ident ident -> ident.name
  ;;

  let loc t ~enclosing =
    match t with
    | Name name -> name.loc
    | Ident _ -> enclosing
  ;;
end

module Constructor = struct
  type t =
    | Path of Syntax.Decl_path.t Located.t
    | Ref of Typed.Constructor_ref.t
end

module Field = struct
  type t =
    | Path of Syntax.Decl_path.t Located.t
    | Ref of Typed.Field_ref.t
end

module Pattern = struct
  type t =
    | Syntax of Syntax.Pattern.t
    | Generated of Typed.Pattern.t

  module Rest = struct
    type t =
      | Closed
      | Open
      | Bind of Binder.t
  end

  module View = struct
    type nonrec t =
      | Any
      | Var of Binder.t
      | Lit of Syntax.Lit.t
      | Tuple of t list
      | Construct of Constructor.t * t list
      | Record of (Field.t * t) list
      | Poly_variant of string * t option
      | Poly_record of
          { fields : (Syntax.Name.t * t) list
          ; rest : Rest.t
          }
  end

  let syntax_view ({ txt; loc } : Syntax.Pattern.t) : View.t =
    let sub pattern = Syntax pattern in
    let fields = List.map ~f:(fun (key, pattern) -> key, sub pattern) in
    match txt with
    | Any -> Any
    | Var name -> Var (Name { txt = name; loc })
    | Lit lit -> Lit lit
    | Tuple patterns -> Tuple (List.map patterns ~f:sub)
    | Construct (path, patterns) ->
      Construct (Path path, List.map patterns ~f:sub)
    | Record fields' ->
      Record
        (List.map fields' ~f:(fun (path, pattern) ->
           Field.Path path, sub pattern))
    | Poly_variant (tag, arg) -> Poly_variant (tag.txt, Option.map arg ~f:sub)
    | Poly_record { fields = fields'; rest } ->
      Poly_record
        { fields = fields fields'
        ; rest =
            (match rest with
             | Closed -> Closed
             | Open -> Open
             | Bind name -> Bind (Name name))
        }
  ;;

  let generated_view ({ desc; ty = _; loc } : Typed.Pattern.t) : View.t =
    let sub pattern = Generated pattern in
    match desc with
    | Any -> Any
    | Var ident -> Var (Ident ident)
    | Lit lit -> Lit lit
    | Tuple patterns -> Tuple (List.map patterns ~f:sub)
    | Construct (constructor, patterns) ->
      Construct (Ref constructor, List.map patterns ~f:sub)
    | Record fields ->
      Record
        (List.map fields ~f:(fun (field, pattern) ->
           Field.Ref field, sub pattern))
    | Poly_variant (tag, arg) -> Poly_variant (tag, Option.map arg ~f:sub)
    | Poly_record { fields; rest } ->
      Poly_record
        { fields =
            List.map fields ~f:(fun (label, p) ->
              Located.create ~loc label, sub p)
        ; rest =
            (match rest with
             | Closed -> Closed
             | Open -> Open
             | Bind ident -> Bind (Ident ident))
        }
  ;;

  let view = function
    | Syntax pattern -> syntax_view pattern
    | Generated pattern -> generated_view pattern
  ;;

  let loc = function
    | Syntax pattern -> pattern.loc
    | Generated pattern -> pattern.loc
  ;;

  let tuple ts =
    let loc = Loc.merge (loc (List.hd_exn ts)) (loc (List.last_exn ts)) in
    let syntax, generated =
      List.partition_map ts ~f:(function
        | Syntax pattern -> First pattern
        | Generated pattern -> Second pattern)
    in
    match syntax, generated with
    | syntax, [] -> Syntax { txt = Tuple syntax; loc }
    | [], generated ->
      Generated
        { desc = Tuple generated
        ; ty =
            TTuple
              (List.map generated ~f:(fun (pattern : Typed.Pattern.t) ->
                 pattern.ty))
        ; loc
        }
    | _ :: _, _ :: _ ->
      raise_s [%message "Tuple of syntax and generated patterns"]
  ;;
end

module Expr = struct
  type t =
    | Syntax of Syntax.Expr.t
    | Generated of Typed.Expr.t

  module Var = struct
    type t =
      | Path of Syntax.Path.t * Syntax.Type_expr.t option
      | Ref of Typed.Var_ref.t
  end

  module Subs = struct
    type nonrec t =
      | Explicit of (Syntax.Name.t * t option) list
      | Implicit
      | Providers of Typed.Binder.t String.Map.t
  end

  module View = struct
    type nonrec t =
      | Lit of Syntax.Lit.t
      | Var of Var.t
      | App of t * t
      | Let of
          { binder : Binder.t
          ; args : (Binder.t * Syntax.Type_expr.t option) list
          ; annot : Syntax.Type_expr.t option
          ; body : t
          ; in_ : t
          }
      | Fun of
          { args : (Binder.t * Syntax.Type_expr.t option) list
          ; body : t
          }
      | Splice of
          { code : t
          ; subs : Subs.t
          }
      | Quote of t
      | Tuple of t list
      | Construct of Constructor.t * t list
      | Record of (Field.t * t) list
      | Field of t * Field.t
      | Match of t * (Pattern.t * t) list
      | Poly_record of (Syntax.Name.t * t) list
      | Poly_field of t * string
      | Poly_extend of (Syntax.Name.t * t) list * t
      | Poly_restrict of t * string
      | Poly_update of t * (Syntax.Name.t * t) list
      | Poly_variant of string * t option
      | Expect of string Located.t
      | Type of Syntax.Type_expr.t
  end

  let syntax_view (expr : Syntax.Expr.t) : View.t =
    let sub expr = Syntax expr in
    let subs = List.map ~f:sub in
    let fields = List.map ~f:(fun (key, expr) -> key, sub expr) in
    match expr.txt with
    | Lit lit -> Lit lit
    | Var (path, annot) -> Var (Path (path, annot))
    | App (f, x) -> App (sub f, sub x)
    | Let (name, args, annot, equal, in_) ->
      Let
        { binder = Name name
        ; args = List.map args ~f:(fun (name, annot) -> Binder.Name name, annot)
        ; annot
        ; body = sub equal
        ; in_ = sub in_
        }
    | Fun (args, body) ->
      Fun
        { args = List.map args ~f:(fun (name, annot) -> Binder.Name name, annot)
        ; body = sub body
        }
    | Splice { expr; subs } ->
      Splice
        { code = sub expr
        ; subs =
            (match subs with
             | None -> Implicit
             | Some subs ->
               Explicit
                 (List.map subs ~f:(fun (name, expr) ->
                    name, Option.map expr ~f:sub)))
        }
    | Quote expr -> Quote (sub expr)
    | Tuple exprs -> Tuple (subs exprs)
    | Construct (path, exprs) -> Construct (Path path, subs exprs)
    | Record fields' ->
      Record
        (List.map fields' ~f:(fun (path, expr) -> Field.Path path, sub expr))
    | Field (expr, path) -> Field (sub expr, Path path)
    | Match (expr, cases) ->
      Match
        ( sub expr
        , List.map cases ~f:(fun (pattern, body) ->
            Pattern.Syntax pattern, sub body) )
    | Poly_record fields' -> Poly_record (fields fields')
    | Poly_field (expr, label) -> Poly_field (sub expr, label.txt)
    | Poly_extend (fields', expr) -> Poly_extend (fields fields', sub expr)
    | Poly_restrict (expr, label) -> Poly_restrict (sub expr, label.txt)
    | Poly_update (expr, fields') -> Poly_update (sub expr, fields fields')
    | Poly_variant (tag, arg) -> Poly_variant (tag.txt, Option.map arg ~f:sub)
    | Expect expected -> Expect expected
    | Type ty -> Type ty
  ;;

  let generated_view ({ desc; ty = _; loc } : Typed.Expr.t) : View.t =
    let sub expr = Generated expr in
    let subs = List.map ~f:sub in
    let fields =
      List.map ~f:(fun (key, expr) -> Located.create ~loc key, sub expr)
    in
    match desc with
    | Lit lit -> Lit lit
    | Var var_ref -> Var (Ref var_ref)
    | App (f, x) -> App (sub f, sub x)
    | Let { ident; args; generalized = _; body; in_ } ->
      Let
        { binder = Ident ident
        ; args = List.map args ~f:(fun ident -> Binder.Ident ident, None)
        ; annot = None
        ; body = sub body
        ; in_ = sub in_
        }
    | Fun { args; body } ->
      Fun
        { args = List.map args ~f:(fun ident -> Binder.Ident ident, None)
        ; body = sub body
        }
    | Splice { code; subs = Explicit subs } ->
      Splice
        { code = sub code
        ; subs =
            Explicit
              (List.map subs ~f:(fun (name, expr) ->
                 Located.create ~loc name, Some (sub expr)))
        }
    | Splice { code; subs = Implicit providers } ->
      Splice { code = sub code; subs = Providers providers }
    | Toplevel_splice _ ->
      raise_s [%message "Generated code has a toplevel splice"]
    | Quote expr -> Quote (sub expr)
    | Tuple exprs -> Tuple (subs exprs)
    | Construct (constructor, exprs) -> Construct (Ref constructor, subs exprs)
    | Record fields' ->
      Record
        (List.map fields' ~f:(fun (field, expr) -> Field.Ref field, sub expr))
    | Field (expr, field) -> Field (sub expr, Ref field)
    | Match (expr, cases) ->
      Match
        ( sub expr
        , List.map cases ~f:(fun (pattern, body) ->
            Pattern.Generated pattern, sub body) )
    | Poly_record fields' -> Poly_record (fields fields')
    | Poly_field (expr, label) -> Poly_field (sub expr, label)
    | Poly_extend (fields', expr) -> Poly_extend (fields fields', sub expr)
    | Poly_restrict (expr, label) -> Poly_restrict (sub expr, label)
    | Poly_update (expr, fields') -> Poly_update (sub expr, fields fields')
    | Poly_variant (tag, arg) -> Poly_variant (tag, Option.map arg ~f:sub)
    | Expect expected -> Expect expected
    | Type ty -> Type ty
  ;;

  let view = function
    | Syntax expr -> syntax_view expr
    | Generated expr -> generated_view expr
  ;;

  let loc = function
    | Syntax expr -> expr.loc
    | Generated expr -> expr.loc
  ;;

  let tuple ts =
    let loc = Loc.merge (loc (List.hd_exn ts)) (loc (List.last_exn ts)) in
    let syntax, generated =
      List.partition_map ts ~f:(function
        | Syntax expr -> First expr
        | Generated expr -> Second expr)
    in
    match syntax, generated with
    | syntax, [] -> Syntax { txt = Tuple syntax; loc }
    | [], generated ->
      Generated
        { desc = Tuple generated
        ; ty =
            TTuple
              (List.map generated ~f:(fun (expr : Typed.Expr.t) -> expr.ty))
        ; loc
        }
    | _ :: _, _ :: _ -> raise_s [%message "Tuple of syntax and generated exprs"]
  ;;
end
