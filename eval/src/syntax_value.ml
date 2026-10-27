open! Import
module Value = Treewalk.Value

module type S = sig
  type t

  val to_value : t -> Value.t
  val of_value : Value.t -> loc:Loc.t -> t Or_error.t
end

(* By canonical name, from the prelude's declarations, so they agree with the
   refs the typechecker makes. *)
let constructor_refs =
  lazy
    (match Source_file.parse Prelude.file with
     | Error error ->
       raise_s
         [%message "Prelude failed to parse" (error : Source_file.Error.t)]
     | Ok { txt = Module (_, items); _ } ->
       let refs = String.Table.create () in
       let qualify path name =
         Prelude.canonical (String.concat (path @ [ name ]) ~sep:".")
       in
       let rec add ~path (item : Syntax.Toplevel.t) =
         match item.txt with
         | Module (name, items) ->
           List.iter items ~f:(add ~path:(path @ [ name.txt ]))
         | Type { name; kind = Variant constructors; params = _ } ->
           let type_name = qualify path name.txt in
           List.iteri constructors ~f:(fun index (constructor, _) ->
             let name = qualify path constructor.txt in
             Hashtbl.add_exn
               refs
               ~key:name
               ~data:{ Typed.Constructor_ref.type_name; name; index })
         | Type _ | Let _ | Expect_test _ -> ()
       in
       List.iter items ~f:(add ~path:[]);
       refs
     | Ok _ -> raise_s [%message "Prelude is not a module"])
;;

module To_value = struct
  open Syntax

  let con name args =
    Value.Construct
      (Hashtbl.find_exn (force constructor_refs) (Prelude.canonical name), args)
  ;;

  let list f xs =
    List.fold_right xs ~init:(con "Nil" []) ~f:(fun x tail ->
      con "Cons" [ f x; tail ])
  ;;

  let option f = function
    | None -> con "None" []
    | Some x -> con "Some" [ f x ]
  ;;

  let string s = Value.String s
  let name (name : Name.t) = string name.txt
  let path (path : Path.t) = string (Path.to_string path)
  let pair f g (a, b) = Value.Tuple [ f a; g b ]
  let record fields = Value.Poly_record (String.Map.of_alist_exn fields)

  let lit : Lit.t -> Value.t = function
    | Int n -> con "Lit.Int" [ Int n ]
    | Bool b -> con "Lit.Bool" [ Bool b ]
    | String s -> con "Lit.String" [ String s ]
    | Char c -> con "Lit.Char" [ Char c ]
    | Unit -> con "Lit.Unit" []
  ;;

  let rec type_expr ({ txt; loc = _ } : Type_expr.t) =
    let con name = con ("Type_expr." ^ name) in
    match txt with
    | Var var -> con "Var" [ string var ]
    | Con (p, args) -> con "Con" [ path p.txt; list type_arg args ]
    | Arrow (a, b) -> con "Arrow" [ type_expr a; type_expr b ]
    | Tuple ts -> con "Tuple" [ list type_expr ts ]
    | Code { inner; context } ->
      con "Code" [ record [ "inner", type_expr inner; "context", row context ] ]
    | Record r -> con "Record" [ row r ]
    | Variant r -> con "Variant" [ row r ]

  and row ({ txt; loc = _ } : Type_expr.row) =
    let con name = con ("Type_expr.Row." ^ name) in
    match txt with
    | Empty -> con "Empty" []
    | Extend (label, ty, rest) ->
      con "Extend" [ name label; type_expr ty; row rest ]
    | Var var -> con "Var" [ name var ]

  and type_arg : Type_expr.arg -> Value.t = function
    | Ty ty -> con "Type_expr.Arg.Ty" [ type_expr ty ]
    | Row r -> con "Type_expr.Arg.Row" [ row r ]
  ;;

  let type_decl ({ name = decl_name; params; kind } : Type_decl.t) =
    let param ({ txt; loc = _ } : Type_decl.Param.t Located.t) =
      match txt with
      | Ty var -> con "Type_decl.Param.Ty" [ string var ]
      | Row var -> con "Type_decl.Param.Row" [ string var ]
    in
    let decl_kind : Type_decl.Kind.t -> Value.t = function
      | Variant constructors ->
        con
          "Type_decl.Kind.Variant"
          [ list (pair name (list type_expr)) constructors ]
      | Record fields ->
        con "Type_decl.Kind.Record" [ list (pair name type_expr) fields ]
      | Alias ty -> con "Type_decl.Kind.Alias" [ type_expr ty ]
      | Abstract -> con "Type_decl.Kind.Abstract" []
    in
    record
      [ "name", name decl_name
      ; "params", list param params
      ; "kind", decl_kind kind
      ]
  ;;

  let binding = pair name (option type_expr)

  let rec pattern ({ txt; loc = _ } : Pattern.t) =
    let con name = con ("Pattern." ^ name) in
    match txt with
    | Any -> con "Any" []
    | Var var -> con "Var" [ string var ]
    | Lit l -> con "Lit" [ lit l ]
    | Tuple ps -> con "Tuple" [ list pattern ps ]
    | Construct (p, args) -> con "Construct" [ path p.txt; list pattern args ]
    | Record fields ->
      con
        "Record"
        [ list (pair (fun (p : _ Located.t) -> path p.txt) pattern) fields ]
    | Poly_variant (tag, arg) ->
      con "Poly_variant" [ name tag; option pattern arg ]
    | Poly_record { fields; rest } ->
      let rest_value : Pattern.Rest.t -> Value.t = function
        | Closed -> con "Rest.Closed" []
        | Open -> con "Rest.Open" []
        | Bind var -> con "Rest.Bind" [ name var ]
      in
      con
        "Poly_record"
        [ record
            [ "fields", list (pair name pattern) fields
            ; "rest", rest_value rest
            ]
        ]
  ;;

  let rec expr ({ txt; loc = _ } : Expr.t) =
    let con name = con ("Expr." ^ name) in
    let fields = list (pair name expr) in
    match txt with
    | Lit l -> con "Lit" [ lit l ]
    | Var (p, annot) -> con "Var" [ path p; option type_expr annot ]
    | App (f, x) -> con "App" [ expr f; expr x ]
    | Let (var, bindings, annot, equal, body) ->
      con
        "Let"
        [ name var
        ; list binding bindings
        ; option type_expr annot
        ; expr equal
        ; expr body
        ]
    | Splice { expr = spliced; subs } ->
      con
        "Splice"
        [ record
            [ "expr", expr spliced
            ; "subs", option (list (pair name (option expr))) subs
            ]
        ]
    | Quote e -> con "Quote" [ expr e ]
    | Tuple es -> con "Tuple" [ list expr es ]
    | Construct (p, args) -> con "Construct" [ path p.txt; list expr args ]
    | Record record_fields ->
      con
        "Record"
        [ list (pair (fun (p : _ Located.t) -> path p.txt) expr) record_fields ]
    | Field (e, p) -> con "Field" [ expr e; path p.txt ]
    | Match (e, arms) -> con "Match" [ expr e; list (pair pattern expr) arms ]
    | Poly_record poly_fields -> con "Poly_record" [ fields poly_fields ]
    | Poly_field (e, label) -> con "Poly_field" [ expr e; name label ]
    | Poly_extend (extension, e) ->
      con "Poly_extend" [ fields extension; expr e ]
    | Poly_restrict (e, label) -> con "Poly_restrict" [ expr e; name label ]
    | Poly_update (e, updates) -> con "Poly_update" [ expr e; fields updates ]
    | Poly_variant (tag, arg) ->
      con "Poly_variant" [ name tag; option expr arg ]
    | Expect expected -> con "Expect" [ string expected.txt ]
  ;;

  let rec toplevel ({ txt; loc = _ } : Toplevel.t) =
    let con name = con ("Toplevel." ^ name) in
    match txt with
    | Let (var, bindings, annot, body) ->
      con
        "Let"
        [ name var; list binding bindings; option type_expr annot; expr body ]
    | Type decl -> con "Type" [ type_decl decl ]
    | Module (module_name, items) ->
      con "Module" [ name module_name; list toplevel items ]
    | Expect_test { name = test_name; body } ->
      con
        "Expect_test"
        [ record [ "name", string test_name.txt; "body", expr body ] ]
  ;;

  let program = list toplevel
end

module Of_value = struct
  open Syntax

  module Malformed = struct
    type t =
      { expected : string
      ; got : string
      ; mutable within : string list (** Outermost first. *)
      }
    [@@deriving sexp_of]
  end

  exception Malformed of Malformed.t

  let malformed ~expected got = raise (Malformed { expected; got; within = [] })

  let unexpected ~expected value =
    malformed ~expected (Value.to_string_hum value)
  ;;

  (* Raised by a [variant] case for a constructor applied to the wrong
     arguments. *)
  exception Bad_arguments

  (** [f] is given the constructor's name relative to its type's module. Errors
      within it are said to be within the constructor, if [within]. *)
  let variant ?(within = true) ty value ~f =
    let type_name = Prelude.canonical ty in
    match value with
    | Value.Construct ({ type_name = type_name'; name; index = _ }, args)
      when String.equal type_name type_name' ->
      let prefix =
        match String.rsplit2 type_name ~on:'.' with
        | Some (prefix, _) -> prefix ^ "."
        | None -> ""
      in
      (match f (String.chop_prefix_exn name ~prefix) args with
       | result -> result
       | exception Bad_arguments -> unexpected ~expected:type_name value
       | exception Malformed malformed ->
         if within then malformed.within <- name :: malformed.within;
         raise (Malformed malformed))
    | _ -> unexpected ~expected:type_name value
  ;;

  let list f value =
    let rec loop acc value =
      match
        variant "list" ~within:false value ~f:(fun name args ->
          match name, args with
          | "Nil", [] -> None
          | "Cons", [ x; tail ] -> Some (f x, tail)
          | _ -> raise Bad_arguments)
      with
      | None -> List.rev acc
      | Some (x, tail) -> loop (x :: acc) tail
    in
    loop [] value
  ;;

  let option f =
    variant "option" ~within:false ~f:(fun name args ->
      match name, args with
      | "None", [] -> None
      | "Some", [ x ] -> Some (f x)
      | _ -> raise Bad_arguments)
  ;;

  let string : Value.t -> string = function
    | String s -> s
    | value -> unexpected ~expected:"string" value
  ;;

  let pair f g : Value.t -> _ = function
    | Tuple [ a; b ] -> f a, g b
    | value -> unexpected ~expected:"a pair" value
  ;;

  let field (value : Value.t) label =
    match value with
    | Poly_record fields when Map.mem fields label -> Map.find_exn fields label
    | _ ->
      unexpected ~expected:(sprintf "a record with a field `%s" label) value
  ;;

  module Ident_kind = struct
    type t =
      | Lower
      | Upper
      | Label
      | Tyvar

    let describe = function
      | Lower -> "a lowercase identifier"
      | Upper -> "a capitalized identifier"
      | Label -> "a label"
      | Tyvar -> "a type variable name"
    ;;

    (* As the lexer reads it, so printed syntax would parse back. *)
    let accepts t text =
      let prefix, token =
        match t with
        | Lower ->
          ( ""
          , if String.equal text "_"
            then Token.Underscore
            else Token.Lident text )
        | Upper -> "", Uident text
        | Label -> "`", Label text
        | Tyvar -> "'", Tyvar text
      in
      match Lexer.tokenize ~file:"" (prefix ^ text) with
      | Ok [| { token = token'; _ }; { token = Eof; _ } |] ->
        Token.equal token token'
      | Ok _ | Error _ -> false
    ;;
  end

  let ident kind value =
    let text = string value in
    if Ident_kind.accepts kind text
    then text
    else malformed ~expected:(Ident_kind.describe kind) (sprintf "%S" text)
  ;;

  module Readers = struct
    type t =
      { type_expr : Value.t -> Type_expr.t
      ; type_decl : Value.t -> Type_decl.t
      ; pattern : Value.t -> Pattern.t
      ; expr : Value.t -> Expr.t
      ; toplevel : Value.t -> Toplevel.t
      }
  end

  let readers ~loc : Readers.t =
    let open Ident_kind in
    let at txt = Located.create ~loc txt in
    let name kind value = at (ident kind value) in
    (* The modules of a path are capitalized, its last name of [kind]. *)
    let path kind value =
      let text = string value in
      let names = String.split text ~on:'.' in
      let modules, last = List.split_n names (List.length names - 1) in
      if
        List.for_all modules ~f:(Ident_kind.accepts Upper)
        && List.for_all last ~f:(Ident_kind.accepts kind)
      then Path.of_list_exn names
      else
        malformed
          ~expected:
            (sprintf
               "a path of capitalized identifiers ending in %s"
               (Ident_kind.describe kind))
          (sprintf "%S" text)
    in
    let lit =
      variant "Lit.t" ~f:(fun constructor args : Lit.t ->
        match constructor, args with
        | "Int", [ Int n ] -> Int n
        | "Bool", [ Bool b ] -> Bool b
        | "String", [ String s ] -> String s
        | "Char", [ Char c ] -> Char c
        | "Unit", [] -> Unit
        | _ -> raise Bad_arguments)
    in
    let rec type_expr value : Type_expr.t =
      at
        (variant
           "Type_expr.t"
           value
           ~f:(fun constructor args : Type_expr.desc ->
             match constructor, args with
             | "Var", [ var ] -> Var (ident Tyvar var)
             | "Con", [ p; args ] -> Con (at (path Lower p), list type_arg args)
             | "Arrow", [ a; b ] -> Arrow (type_expr a, type_expr b)
             | "Tuple", [ ts ] -> Tuple (list type_expr ts)
             | "Code", [ code ] ->
               Code
                 { inner = type_expr (field code "inner")
                 ; context = row ~labels:Lower (field code "context")
                 }
             | "Record", [ r ] -> Record (row ~labels:Label r)
             | "Variant", [ r ] -> Variant (row ~labels:Label r)
             | _ -> raise Bad_arguments))
    and row ~labels value : Type_expr.row =
      at
        (variant
           "Type_expr.Row.t"
           value
           ~f:(fun constructor args : Type_expr.row_desc ->
             match constructor, args with
             | "Empty", [] -> Empty
             | "Extend", [ label; ty; rest ] ->
               Extend (name labels label, type_expr ty, row ~labels rest)
             | "Var", [ var ] -> Var (name Tyvar var)
             | _ -> raise Bad_arguments))
    and type_arg value =
      variant
        "Type_expr.Arg.t"
        value
        ~f:(fun constructor args : Type_expr.arg ->
          match constructor, args with
          | "Ty", [ ty ] -> Ty (type_expr ty)
          | "Row", [ r ] -> Row (row ~labels:Label r)
          | _ -> raise Bad_arguments)
    in
    let type_decl value : Type_decl.t =
      let param value =
        at
          (variant
             "Type_decl.Param.t"
             value
             ~f:(fun constructor args : Type_decl.Param.t ->
               match constructor, args with
               | "Ty", [ var ] -> Ty (ident Tyvar var)
               | "Row", [ var ] -> Row (ident Tyvar var)
               | _ -> raise Bad_arguments))
      in
      let kind =
        variant
          "Type_decl.Kind.t"
          ~f:(fun constructor args : Type_decl.Kind.t ->
            match constructor, args with
            | "Variant", [ constructors ] ->
              Variant (list (pair (name Upper) (list type_expr)) constructors)
            | "Record", [ fields ] ->
              Record (list (pair (name Lower) type_expr) fields)
            | "Alias", [ ty ] -> Alias (type_expr ty)
            | "Abstract", [] -> Abstract
            | _ -> raise Bad_arguments)
      in
      { name = name Lower (field value "name")
      ; params = list param (field value "params")
      ; kind = kind (field value "kind")
      }
    in
    let binding = pair (name Lower) (option type_expr) in
    let rec pattern value : Pattern.t =
      at
        (variant "Pattern.t" value ~f:(fun constructor args : Pattern.desc ->
           match constructor, args with
           | "Any", [] -> Any
           | "Var", [ var ] -> Var (ident Lower var)
           | "Lit", [ l ] -> Lit (lit l)
           | "Tuple", [ ps ] -> Tuple (list pattern ps)
           | "Construct", [ p; args ] ->
             Construct (at (path Upper p), list pattern args)
           | "Record", [ fields ] ->
             Record (list (pair (fun p -> at (path Lower p)) pattern) fields)
           | "Poly_variant", [ tag; arg ] ->
             Poly_variant (name Label tag, option pattern arg)
           | "Poly_record", [ r ] ->
             let rest =
               variant
                 "Pattern.Rest.t"
                 ~f:(fun constructor args : Pattern.Rest.t ->
                   match constructor, args with
                   | "Closed", [] -> Closed
                   | "Open", [] -> Open
                   | "Bind", [ var ] -> Bind (name Lower var)
                   | _ -> raise Bad_arguments)
             in
             Poly_record
               { fields = list (pair (name Label) pattern) (field r "fields")
               ; rest = rest (field r "rest")
               }
           | _ -> raise Bad_arguments))
    in
    let rec expr value : Expr.t =
      let fields = list (pair (name Label) expr) in
      at
        (variant "Expr.t" value ~f:(fun constructor args : Expr.desc ->
           match constructor, args with
           | "Lit", [ l ] -> Lit (lit l)
           | "Var", [ p; annot ] -> Var (path Lower p, option type_expr annot)
           | "App", [ f; x ] -> App (expr f, expr x)
           | "Let", [ var; bindings; annot; equal; body ] ->
             Let
               ( name Lower var
               , list binding bindings
               , option type_expr annot
               , expr equal
               , expr body )
           | "Splice", [ splice ] ->
             Splice
               { expr = expr (field splice "expr")
               ; subs =
                   option
                     (list (pair (name Lower) (option expr)))
                     (field splice "subs")
               }
           | "Quote", [ e ] -> Quote (expr e)
           | "Tuple", [ es ] -> Tuple (list expr es)
           | "Construct", [ p; args ] ->
             Construct (at (path Upper p), list expr args)
           | "Record", [ record_fields ] ->
             Record
               (list (pair (fun p -> at (path Lower p)) expr) record_fields)
           | "Field", [ e; p ] -> Field (expr e, at (path Lower p))
           | "Match", [ e; arms ] ->
             Match (expr e, list (pair pattern expr) arms)
           | "Poly_record", [ poly_fields ] -> Poly_record (fields poly_fields)
           | "Poly_field", [ e; label ] -> Poly_field (expr e, name Label label)
           | "Poly_extend", [ extension; e ] ->
             Poly_extend (fields extension, expr e)
           | "Poly_restrict", [ e; label ] ->
             Poly_restrict (expr e, name Label label)
           | "Poly_update", [ e; updates ] ->
             Poly_update (expr e, fields updates)
           | "Poly_variant", [ tag; arg ] ->
             Poly_variant (name Label tag, option expr arg)
           | "Expect", [ expected ] -> Expect (at (string expected))
           | _ -> raise Bad_arguments))
    in
    let rec toplevel value : Toplevel.t =
      at
        (variant "Toplevel.t" value ~f:(fun constructor args : Toplevel.desc ->
           match constructor, args with
           | "Let", [ var; bindings; annot; body ] ->
             Let
               ( name Lower var
               , list binding bindings
               , option type_expr annot
               , expr body )
           | "Type", [ decl ] -> Type (type_decl decl)
           | "Module", [ module_name; items ] ->
             Module (name Upper module_name, list toplevel items)
           | "Expect_test", [ test ] ->
             Expect_test
               { name = at (string (field test "name"))
               ; body = expr (field test "body")
               }
           | _ -> raise Bad_arguments))
    in
    { type_expr; type_decl; pattern; expr; toplevel }
  ;;

  let run f value =
    match f value with
    | result -> Ok result
    | exception Malformed malformed ->
      Error
        (Error.create_s
           [%message "Malformed syntax" ~_:(malformed : Malformed.t)])
  ;;
end

let of_value read value ~loc =
  Of_value.run (fun value -> read (Of_value.readers ~loc) value) value
;;

module Type_expr = struct
  let to_value = To_value.type_expr
  let of_value = of_value (fun readers -> readers.type_expr)
end

module Type_decl = struct
  let to_value = To_value.type_decl
  let of_value = of_value (fun readers -> readers.type_decl)
end

module Pattern = struct
  let to_value = To_value.pattern
  let of_value = of_value (fun readers -> readers.pattern)
end

module Expr = struct
  let to_value = To_value.expr
  let of_value = of_value (fun readers -> readers.expr)
end

module Toplevel = struct
  let to_value = To_value.toplevel
  let of_value = of_value (fun readers -> readers.toplevel)
end

module Program = struct
  let to_value = To_value.program
  let of_value = of_value (fun readers -> Of_value.list readers.toplevel)
end
