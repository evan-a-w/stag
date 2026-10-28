open! Import
include Syntax_value_intf

(* By canonical name, from the prelude's declarations, so they agree with the
   refs the typechecker makes. *)
let constructor_refs =
  lazy
    (let refs = String.Table.create () in
     let qualify path name =
       Prelude_path.canonical (String.concat (path @ [ name ]) ~sep:".")
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
       | Type _
       | Exception _
       | Let _
       | Expect_test _
       | Bench _
       | Module_splice _
       | Include _ -> ()
     in
     List.iter (force Prelude.toplevels) ~f:(add ~path:[]);
     refs)
;;

module To_value = struct
  open Syntax

  let con name args =
    Value.Construct
      ( Hashtbl.find_exn (force constructor_refs) (Prelude_path.canonical name)
      , args )
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

  let decl_path kind ({ txt; loc = _ } : Decl_path.t Located.t) =
    match txt with
    | Path p -> con "Stag.Path.Name" [ path p ]
    | Resolved name -> con "Stag.Path.Ref" [ Value.Ref { kind; name } ]
  ;;

  let record fields = Value.Poly_record (String.Map.of_alist_exn fields)

  let lit : Lit.t -> Value.t = function
    | Int n -> con "Stag.Lit.Int" [ Int n ]
    | Bool b -> con "Stag.Lit.Bool" [ Bool b ]
    | String s -> con "Stag.Lit.String" [ String s ]
    | Char c -> con "Stag.Lit.Char" [ Char c ]
    | Unit -> con "Stag.Lit.Unit" []
  ;;

  let rec type_expr ({ txt; loc = _ } : Type_expr.t) =
    let con name = con ("Stag.Type_expr." ^ name) in
    match txt with
    | Var var -> con "Var" [ string var ]
    | Con (p, args) -> con "Con" [ decl_path Type p; list type_arg args ]
    | Arrow (a, b) -> con "Arrow" [ type_expr a; type_expr b ]
    | Tuple ts -> con "Tuple" [ list type_expr ts ]
    | Code { inner; context } ->
      con "Code" [ record [ "inner", type_expr inner; "context", row context ] ]
    | Record r -> con "Record" [ row r ]
    | Variant r -> con "Variant" [ row r ]

  and row ({ txt; loc = _ } : Type_expr.row) =
    let con name = con ("Stag.Type_expr.Row." ^ name) in
    match txt with
    | Empty -> con "Empty" []
    | Extend (label, ty, rest) ->
      con "Extend" [ name label; type_expr ty; row rest ]
    | Var var -> con "Var" [ name var ]

  and type_arg : Type_expr.arg -> Value.t = function
    | Ty ty -> con "Stag.Type_expr.Arg.Ty" [ type_expr ty ]
    | Row r -> con "Stag.Type_expr.Arg.Row" [ row r ]
  ;;

  let type_param ({ txt; loc = _ } : Type_decl.Param.t Located.t) =
    match txt with
    | Ty var -> con "Stag.Type_decl.Param.Ty" [ string var ]
    | Row var -> con "Stag.Type_decl.Param.Row" [ string var ]
  ;;

  let type_decl ({ name = decl_name; params; kind } : Type_decl.t) =
    let decl_kind : Type_decl.Kind.t -> Value.t = function
      | Variant constructors ->
        con
          "Stag.Type_decl.Kind.Variant"
          [ list (pair name (list type_expr)) constructors ]
      | Record fields ->
        con "Stag.Type_decl.Kind.Record" [ list (pair name type_expr) fields ]
      | Alias ty -> con "Stag.Type_decl.Kind.Alias" [ type_expr ty ]
      | Abstract -> con "Stag.Type_decl.Kind.Abstract" []
    in
    record
      [ "name", name decl_name
      ; "params", list type_param params
      ; "kind", decl_kind kind
      ]
  ;;

  let binding = pair name (option type_expr)

  let rec pattern ({ txt; loc = _ } : Pattern.t) =
    let con name = con ("Stag.Pattern." ^ name) in
    match txt with
    | Any -> con "Any" []
    | Var var -> con "Var" [ string var ]
    | Lit l -> con "Lit" [ lit l ]
    | Tuple ps -> con "Tuple" [ list pattern ps ]
    | Construct (p, args) ->
      con "Construct" [ decl_path Constructor p; list pattern args ]
    | Record fields ->
      con "Record" [ list (pair (decl_path Field) pattern) fields ]
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
    | Exception p -> con "Exception" [ pattern p ]
  ;;

  let rec expr ({ txt; loc = _ } : Expr.t) =
    let con name = con ("Stag.Expr." ^ name) in
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
    | Fun (params, body) -> con "Fun" [ list binding params; expr body ]
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
    | Construct (p, args) ->
      con "Construct" [ decl_path Constructor p; list expr args ]
    | Record record_fields ->
      con "Record" [ list (pair (decl_path Field) expr) record_fields ]
    | Record_update (e, record_fields) ->
      con
        "Record_update"
        [ expr e; list (pair (decl_path Field) expr) record_fields ]
    | Field (e, p) -> con "Field" [ expr e; decl_path Field p ]
    | Type ty -> con "Type" [ type_expr ty ]
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
    let con name = con ("Stag.Toplevel." ^ name) in
    match txt with
    | Let (var, bindings, annot, body) ->
      con
        "Let"
        [ name var; list binding bindings; option type_expr annot; expr body ]
    | Type decl -> con "Type" [ type_decl decl ]
    | Exception (exn_name, args) ->
      con "Exception" [ name exn_name; list type_expr args ]
    | Module (module_name, items) ->
      con "Module" [ name module_name; list toplevel items ]
    | Module_splice (module_name, e, _) ->
      con "Module_splice" [ name module_name; expr e ]
    | Include (e, _) -> con "Include" [ expr e ]
    | Expect_test { name = test_name; body } ->
      con
        "Expect_test"
        [ record [ "name", string test_name.txt; "body", expr body ] ]
    | Bench { name = bench_name; param; body } ->
      con
        "Bench"
        [ record
            [ "name", string bench_name.txt
            ; ( "param"
              , option
                  (fun ({ name = param; args } : Bench_param.t) ->
                     pair name (list expr) (param, args))
                  param )
            ; "body", expr body
            ]
        ]
  ;;

  let program = list toplevel

  let reflected_decl decl ~decl_type =
    option
      (fun ({ params; kind } : Reflected_decl.t) ->
         let ref_ kind name = Value.Ref { kind; name } in
         let kind =
           match kind with
           | Variant constructors ->
             con
               "Stag.Type.Kind.Variant"
               [ list (pair (ref_ Constructor) (list type_expr)) constructors ]
           | Record fields ->
             con
               "Stag.Type.Kind.Record"
               [ list (pair (ref_ Field) type_expr) fields ]
           | Alias ty -> con "Stag.Type.Kind.Alias" [ type_expr ty ]
         in
         Value.Record (decl_type, [| list type_param params; kind |]))
      decl
  ;;
end

module Of_value = struct
  open Syntax

  module Step = struct
    type t =
      | Constructor of
          { name : string
          ; argument : int option (** Of a constructor of several. *)
          }
      | Element of int
      | Component of int
      | Field of string

    let to_string = function
      | Constructor { name; argument = None } -> name
      | Constructor { name; argument = Some i } ->
        sprintf "%s argument %d" name i
      | Element i -> sprintf "element %d" i
      | Component i -> sprintf "component %d" i
      | Field label -> sprintf "field `%s" label
    ;;
  end

  module Malformed = struct
    type t =
      { expected : string
      ; got : Value.t
      ; within : Step.t list (** Outermost first. *)
      }

    let to_string { expected; got; within } =
      let within =
        match within with
        | [] -> ""
        | steps ->
          sprintf
            ", within %s"
            (String.concat ~sep:" > " (List.map steps ~f:Step.to_string))
      in
      sprintf
        "Malformed syntax: expected %s, got %s%s"
        expected
        (Value.to_string_for_message got)
        within
    ;;
  end

  exception Malformed of Malformed.t

  let unexpected ~expected got =
    raise (Malformed { expected; got; within = [] })
  ;;

  let within step read value =
    try read value with
    | Malformed malformed ->
      raise (Malformed { malformed with within = step :: malformed.within })
  ;;

  module Args = struct
    type t =
      { constructor : string
      ; values : Value.t array
      }

    let count t = Array.length t.values

    let read t i read =
      let argument = Option.some_if (count t > 1) i in
      within (Constructor { name = t.constructor; argument }) read t.values.(i)
    ;;
  end

  (* Raised by a [variant] case for a constructor applied to a number of
     arguments other than it is declared with. *)
  exception Bad_arguments

  (** The constructor of [value], of the prelude's type [ty], by its name
      relative to its type's module. *)
  let constructor ty value =
    let type_name = Prelude_path.canonical ty in
    match (value : Value.t) with
    | Construct ({ type_name = type_name'; name; index = _ }, args)
      when String.equal type_name type_name' ->
      let prefix =
        match String.rsplit2 type_name ~on:'.' with
        | Some (prefix, _) -> prefix ^ "."
        | None -> ""
      in
      ( String.chop_prefix_exn name ~prefix
      , { Args.constructor = Prelude_path.to_string_hum name
        ; values = Array.of_list args
        } )
    | _ -> unexpected ~expected:ty value
  ;;

  let variant ty value ~f =
    let name, args = constructor ty value in
    match f name args with
    | result -> result
    | exception Bad_arguments ->
      unexpected
        ~expected:(args.constructor ^ " applied to its declared arguments")
        value
  ;;

  let list f value =
    let rec loop i acc value =
      match constructor "list" value with
      | "Nil", { values = [||]; _ } -> List.rev acc
      | "Cons", { values = [| x; tail |]; _ } ->
        loop (i + 1) (within (Element i) f x :: acc) tail
      | _, { constructor; _ } ->
        unexpected
          ~expected:(constructor ^ " applied to its declared arguments")
          value
    in
    loop 0 [] value
  ;;

  let option f value =
    match constructor "option" value with
    | "None", { values = [||]; _ } -> None
    | "Some", { values = [| x |]; _ } -> Some (f x)
    | _, { constructor; _ } ->
      unexpected
        ~expected:(constructor ^ " applied to its declared arguments")
        value
  ;;

  let int : Value.t -> int = function
    | Int n -> n
    | value -> unexpected ~expected:"an int" value
  ;;

  let bool : Value.t -> bool = function
    | Bool b -> b
    | value -> unexpected ~expected:"a bool" value
  ;;

  let char : Value.t -> char = function
    | Char c -> c
    | value -> unexpected ~expected:"a char" value
  ;;

  let string : Value.t -> string = function
    | String s -> s
    | value -> unexpected ~expected:"a string" value
  ;;

  let pair f g : Value.t -> _ = function
    | Tuple [ a; b ] -> within (Component 0) f a, within (Component 1) g b
    | value -> unexpected ~expected:"a pair" value
  ;;

  let field label read (value : Value.t) =
    match value with
    | Poly_record fields when Map.mem fields label ->
      within (Field label) read (Map.find_exn fields label)
    | _ ->
      unexpected ~expected:(sprintf "a record with a field `%s" label) value
  ;;

  let ident kind value =
    let text = string value in
    if Ident_kind.accepts kind text
    then text
    else unexpected ~expected:(Ident_kind.describe kind) value
  ;;

  let name kind value ~loc = Located.create ~loc (ident kind value)

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
      unexpected
        ~expected:
          (sprintf
             "a path of capitalized identifiers ending in %s"
             (Ident_kind.describe kind))
        value
  ;;

  let decl_path kind value ~loc =
    Located.create
      ~loc
      (variant "Stag.Path.t" value ~f:(fun constructor args : Decl_path.t ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Name", 1 -> Path (arg 0 (path kind))
         | "Ref", 1 ->
           arg 0 (fun (value : Value.t) : Decl_path.t ->
             match value with
             | Ref { name; kind = _ } -> Resolved name
             | _ -> unexpected ~expected:"a ref" value)
         | _ -> raise Bad_arguments))
  ;;

  (* [Tuple []] is the type [()], but [Lit Unit] in patterns and expressions. *)
  let components ~min read value =
    let components = list read value in
    if List.length components < min || List.length components = 1
    then
      unexpected
        ~expected:
          (if min = 0
           then "no components or at least two"
           else "at least two components")
        value
    else components
  ;;

  let params read value =
    match list read value with
    | [] -> unexpected ~expected:"at least one parameter" value
    | params -> params
  ;;

  let fields read value =
    match list read value with
    | [] -> unexpected ~expected:"at least one field" value
    | fields -> fields
  ;;

  open Ident_kind

  let lit value =
    variant "Stag.Lit.t" value ~f:(fun constructor args : Lit.t ->
      let arg i read = Args.read args i read in
      match constructor, Args.count args with
      | "Int", 1 -> Int (arg 0 int)
      | "Bool", 1 -> Bool (arg 0 bool)
      | "String", 1 -> String (arg 0 string)
      | "Char", 1 -> Char (arg 0 char)
      | "Unit", 0 -> Unit
      | _ -> raise Bad_arguments)
  ;;

  let rec type_expr value ~loc : Type_expr.t =
    Located.create
      ~loc
      (variant
         "Stag.Type_expr.t"
         value
         ~f:(fun constructor args : Type_expr.desc ->
           let arg i read = Args.read args i read in
           match constructor, Args.count args with
           | "Var", 1 -> Var (arg 0 (ident Tyvar))
           | "Con", 2 ->
             Con (arg 0 (decl_path Lower ~loc), arg 1 (list (type_arg ~loc)))
           | "Arrow", 2 -> Arrow (arg 0 (type_expr ~loc), arg 1 (type_expr ~loc))
           | "Tuple", 1 -> Tuple (arg 0 (components ~min:0 (type_expr ~loc)))
           | "Code", 1 ->
             arg 0 (fun code : Type_expr.desc ->
               Code
                 { inner = field "inner" (type_expr ~loc) code
                 ; context = field "context" (row ~labels:Lower ~loc) code
                 })
           | "Record", 1 -> Record (arg 0 (row ~labels:Label ~loc))
           | "Variant", 1 -> Variant (arg 0 (row ~labels:Label ~loc))
           | _ -> raise Bad_arguments))

  and row ~(labels : Ident_kind.t) value ~loc : Type_expr.row =
    Located.create
      ~loc
      (variant
         "Stag.Type_expr.Row.t"
         value
         ~f:(fun constructor args : Type_expr.row_desc ->
           let arg i read = Args.read args i read in
           match constructor, Args.count args with
           | "Empty", 0 -> Empty
           | "Extend", 3 ->
             Extend
               ( arg 0 (name labels ~loc)
               , arg 1 (type_expr ~loc)
               , arg 2 (row ~labels ~loc) )
           | "Var", 1 -> Var (arg 0 (name Tyvar ~loc))
           | _ -> raise Bad_arguments))

  and type_arg value ~loc =
    variant
      "Stag.Type_expr.Arg.t"
      value
      ~f:(fun constructor args : Type_expr.arg ->
        let arg i read = Args.read args i read in
        match constructor, Args.count args with
        | "Ty", 1 -> Ty (arg 0 (type_expr ~loc))
        | "Row", 1 -> Row (arg 0 (row ~labels:Label ~loc))
        | _ -> raise Bad_arguments)
  ;;

  let type_decl value ~loc : Type_decl.t =
    let param value =
      Located.create
        ~loc
        (variant
           "Stag.Type_decl.Param.t"
           value
           ~f:(fun constructor args : Type_decl.Param.t ->
             let arg i read = Args.read args i read in
             match constructor, Args.count args with
             | "Ty", 1 -> Ty (arg 0 (ident Tyvar))
             | "Row", 1 -> Row (arg 0 (ident Tyvar))
             | _ -> raise Bad_arguments))
    in
    let kind value =
      variant
        "Stag.Type_decl.Kind.t"
        value
        ~f:(fun constructor args : Type_decl.Kind.t ->
          let arg i read = Args.read args i read in
          match constructor, Args.count args with
          | "Variant", 1 ->
            Variant
              (arg 0 (list (pair (name Upper ~loc) (list (type_expr ~loc)))))
          | "Record", 1 ->
            Record (arg 0 (fields (pair (name Lower ~loc) (type_expr ~loc))))
          | "Alias", 1 -> Alias (arg 0 (type_expr ~loc))
          | "Abstract", 0 -> Abstract
          | _ -> raise Bad_arguments)
    in
    { name = field "name" (name Lower ~loc) value
    ; params = field "params" (list param) value
    ; kind = field "kind" kind value
    }
  ;;

  let binding ~loc = pair (name Binder ~loc) (option (type_expr ~loc))

  let rec pattern value ~loc : Pattern.t =
    Located.create
      ~loc
      (variant "Stag.Pattern.t" value ~f:(fun constructor args : Pattern.desc ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Any", 0 -> Any
         | "Var", 1 -> Var (arg 0 (ident Lower))
         | "Lit", 1 -> Lit (arg 0 lit)
         | "Tuple", 1 -> Tuple (arg 0 (components ~min:2 (pattern ~loc)))
         | "Construct", 2 ->
           Construct (arg 0 (decl_path Upper ~loc), arg 1 (list (pattern ~loc)))
         | "Record", 1 ->
           Record (arg 0 (fields (pair (decl_path Lower ~loc) (pattern ~loc))))
         | "Poly_variant", 2 ->
           Poly_variant (arg 0 (name Label ~loc), arg 1 (option (pattern ~loc)))
         | "Poly_record", 1 ->
           arg 0 (fun record : Pattern.desc ->
             Poly_record
               { fields =
                   field
                     "fields"
                     (list (pair (name Label ~loc) (pattern ~loc)))
                     record
               ; rest = field "rest" (rest ~loc) record
               })
         | "Exception", 1 -> Exception (arg 0 (pattern ~loc))
         | _ -> raise Bad_arguments))

  and rest value ~loc =
    variant
      "Stag.Pattern.Rest.t"
      value
      ~f:(fun constructor args : Pattern.Rest.t ->
        let arg i read = Args.read args i read in
        match constructor, Args.count args with
        | "Closed", 0 -> Closed
        | "Open", 0 -> Open
        | "Bind", 1 -> Bind (arg 0 (name Lower ~loc))
        | _ -> raise Bad_arguments)
  ;;

  let rec expr value ~loc : Expr.t =
    let labelled = list (pair (name Label ~loc) (expr ~loc)) in
    Located.create
      ~loc
      (variant "Stag.Expr.t" value ~f:(fun constructor args : Expr.desc ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Lit", 1 -> Lit (arg 0 lit)
         | "Var", 2 -> Var (arg 0 (path Lower), arg 1 (option (type_expr ~loc)))
         | "App", 2 -> App (arg 0 (expr ~loc), arg 1 (expr ~loc))
         | "Let", 5 ->
           Let
             ( arg 0 (name Binder ~loc)
             , arg 1 (list (binding ~loc))
             , arg 2 (option (type_expr ~loc))
             , arg 3 (expr ~loc)
             , arg 4 (expr ~loc) )
         | "Fun", 2 -> Fun (arg 0 (params (binding ~loc)), arg 1 (expr ~loc))
         | "Splice", 1 ->
           arg 0 (fun splice : Expr.desc ->
             Splice
               { expr = field "expr" (expr ~loc) splice
               ; subs =
                   field
                     "subs"
                     (option
                        (list (pair (name Lower ~loc) (option (expr ~loc)))))
                     splice
               })
         | "Quote", 1 -> Quote (arg 0 (expr ~loc))
         | "Tuple", 1 -> Tuple (arg 0 (components ~min:2 (expr ~loc)))
         | "Construct", 2 ->
           Construct (arg 0 (decl_path Upper ~loc), arg 1 (list (expr ~loc)))
         | "Record", 1 ->
           Record (arg 0 (fields (pair (decl_path Lower ~loc) (expr ~loc))))
         | "Field", 2 -> Field (arg 0 (expr ~loc), arg 1 (decl_path Lower ~loc))
         | "Record_update", 2 ->
           Record_update
             ( arg 0 (expr ~loc)
             , arg 1 (fields (pair (decl_path Lower ~loc) (expr ~loc))) )
         | "Type", 1 -> Type (arg 0 (type_expr ~loc))
         | "Match", 2 ->
           Match
             (arg 0 (expr ~loc), arg 1 (list (pair (pattern ~loc) (expr ~loc))))
         | "Poly_record", 1 -> Poly_record (arg 0 labelled)
         | "Poly_field", 2 ->
           Poly_field (arg 0 (expr ~loc), arg 1 (name Label ~loc))
         | "Poly_extend", 2 -> Poly_extend (arg 0 labelled, arg 1 (expr ~loc))
         | "Poly_restrict", 2 ->
           Poly_restrict (arg 0 (expr ~loc), arg 1 (name Label ~loc))
         | "Poly_update", 2 -> Poly_update (arg 0 (expr ~loc), arg 1 labelled)
         | "Poly_variant", 2 ->
           Poly_variant (arg 0 (name Label ~loc), arg 1 (option (expr ~loc)))
         | "Expect", 1 -> Expect (Located.create ~loc (arg 0 string))
         | _ -> raise Bad_arguments))
  ;;

  let rec toplevel value ~loc : Toplevel.t =
    Located.create
      ~loc
      (variant
         "Stag.Toplevel.t"
         value
         ~f:(fun constructor args : Toplevel.desc ->
           let arg i read = Args.read args i read in
           match constructor, Args.count args with
           | "Let", 4 ->
             Let
               ( arg 0 (name Binder ~loc)
               , arg 1 (list (binding ~loc))
               , arg 2 (option (type_expr ~loc))
               , arg 3 (expr ~loc) )
           | "Type", 1 -> Type (arg 0 (type_decl ~loc))
           | "Exception", 2 ->
             Exception (arg 0 (name Upper ~loc), arg 1 (list (type_expr ~loc)))
           | "Module", 2 ->
             Module (arg 0 (name Upper ~loc), arg 1 (list (toplevel ~loc)))
           | "Module_splice", 2 ->
             Module_splice (arg 0 (name Upper ~loc), arg 1 (expr ~loc), None)
           | "Include", 1 -> Include (arg 0 (expr ~loc), None)
           | "Expect_test", 1 ->
             arg 0 (fun test : Toplevel.desc ->
               Expect_test
                 { name = Located.create ~loc (field "name" string test)
                 ; body = field "body" (expr ~loc) test
                 })
           | "Bench", 1 ->
             arg 0 (fun bench : Toplevel.desc ->
               Bench
                 { name = Located.create ~loc (field "name" string bench)
                 ; param =
                     field
                       "param"
                       (option (fun value : Bench_param.t ->
                          let name, args =
                            pair (name Lower ~loc) (list (expr ~loc)) value
                          in
                          match args with
                          | [] ->
                            unexpected
                              ~expected:"a parameter with at least one argument"
                              value
                          | args -> { name; args }))
                       bench
                 ; body = field "body" (expr ~loc) bench
                 })
           | _ -> raise Bad_arguments))
  ;;

  let program value ~loc = list (toplevel ~loc) value

  let run read value ~loc =
    match read value ~loc with
    | result -> Ok result
    | exception Malformed malformed ->
      Error (Error.of_string (Malformed.to_string malformed))
  ;;
end

let reflected_decl = To_value.reflected_decl

module Type_expr = struct
  let to_value = To_value.type_expr
  let of_value = Of_value.(run type_expr)
end

module Type_decl = struct
  let to_value = To_value.type_decl
  let of_value = Of_value.(run type_decl)
end

module Pattern = struct
  let to_value = To_value.pattern
  let of_value = Of_value.(run pattern)
end

module Expr = struct
  let to_value = To_value.expr
  let of_value = Of_value.(run expr)
end

module Toplevel = struct
  let to_value = To_value.toplevel
  let of_value = Of_value.(run toplevel)
end

module Program = struct
  let to_value = To_value.program
  let of_value = Of_value.(run program)
end
