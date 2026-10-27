open! Core

module Binder = struct
  type t =
    | Name of string
    | Ident of Typed.Ident.t

  let name = function
    | Name name -> name
    | Ident ident -> ident.name
  ;;
end

module Constructor = struct
  type t =
    | Path of Syntax.Path.t
    | Ref of Typed.Constructor_ref.t
end

module Field = struct
  type t =
    | Path of Syntax.Path.t
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
          { fields : (string * t) list
          ; rest : Rest.t
          }
  end

  let syntax_view : Syntax.Pattern.t -> View.t =
    let sub pattern = Syntax pattern in
    let fields = List.map ~f:(fun (key, pattern) -> key, sub pattern) in
    function
    | Any -> Any
    | Var name -> Var (Name name)
    | Lit lit -> Lit lit
    | Tuple patterns -> Tuple (List.map patterns ~f:sub)
    | Construct (path, patterns) ->
      Construct (Path path, List.map patterns ~f:sub)
    | Record fields' ->
      Record
        (List.map fields' ~f:(fun (path, pattern) ->
           Field.Path path, sub pattern))
    | Poly_variant (tag, arg) -> Poly_variant (tag, Option.map arg ~f:sub)
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

  let generated_view ({ desc; ty = _ } : Typed.Pattern.t) : View.t =
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
        { fields = List.map fields ~f:(fun (label, p) -> label, sub p)
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

  let is_irrefutable = function
    | Syntax pattern -> Syntax.Pattern.is_irrefutable pattern
    | Generated pattern -> Typed.Pattern.is_irrefutable pattern
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
          ; generalized : bool
          }
      | Splice of
          { code : t
          ; subs : (string * t option) list option
          }
      | Quote of t
      | Tuple of t list
      | Construct of Constructor.t * t list
      | Record of (Field.t * t) list
      | Field of t * Field.t
      | Match of t * (Pattern.t * t) list
      | Poly_record of (string * t) list
      | Poly_field of t * string
      | Poly_extend of (string * t) list * t
      | Poly_restrict of t * string
      | Poly_update of t * (string * t) list
      | Poly_variant of string * t option
  end

  let syntax_view : Syntax.Expr.t -> View.t =
    let sub expr = Syntax expr in
    let subs = List.map ~f:sub in
    let fields = List.map ~f:(fun (key, expr) -> key, sub expr) in
    function
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
        ; generalized = Syntax.Expr.binding_is_value name ~args ~equal
        }
    | Splice { expr; subs } ->
      Splice
        { code = sub expr
        ; subs =
            Option.map subs ~f:(fun subs ->
              List.map subs ~f:(fun (name, expr) ->
                name, Option.map expr ~f:sub))
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
    | Poly_field (expr, label) -> Poly_field (sub expr, label)
    | Poly_extend (fields', expr) -> Poly_extend (fields fields', sub expr)
    | Poly_restrict (expr, label) -> Poly_restrict (sub expr, label)
    | Poly_update (expr, fields') -> Poly_update (sub expr, fields fields')
    | Poly_variant (tag, arg) -> Poly_variant (tag, Option.map arg ~f:sub)
  ;;

  let generated_view ({ desc; ty = _ } : Typed.Expr.t) : View.t =
    let sub expr = Generated expr in
    let subs = List.map ~f:sub in
    let fields = List.map ~f:(fun (key, expr) -> key, sub expr) in
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
        ; generalized = Typed.Expr.binding_is_value ~ident ~args ~body
        }
    | Splice { code; subs = Explicit subs } ->
      Splice
        { code = sub code
        ; subs =
            Some (List.map subs ~f:(fun (name, expr) -> name, Some (sub expr)))
        }
    | Splice { code; subs = Implicit _ } ->
      Splice { code = sub code; subs = None }
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
  ;;

  let view = function
    | Syntax expr -> syntax_view expr
    | Generated expr -> generated_view expr
  ;;
end
