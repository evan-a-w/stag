open! Import

module type S = Syntax_value_intf.S with type value := Value.t

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
       | External _
       | Malformed_exception _
       | Let _
       | Let_mut _
       | Expect_test _
       | Bench _
       | Module_splice _
       | Include _ -> ()
     in
     List.iter (force Prelude.toplevels) ~f:(add ~path:[]);
     refs)
;;

module To_value = struct
  let con name args =
    Value.Construct
      ( Hashtbl.find_exn (force constructor_refs) (Prelude_path.canonical name)
      , args )
  ;;

  include Lift.Make (struct
      type t = Value.t

      let con = con
      let int n = Value.Int n
      let bool b = Value.Bool b
      let string s = Value.String s
      let char c = Value.Char c
      let tuple vs = Value.Tuple vs
      let ptr v = Value.Ptr (ref v)
      let record fields = Value.Poly_record (String.Map.of_alist_exn fields)
      let ref_ kind name = Value.Ref { kind; name }

      let rec append (xs : Value.t) ys =
        match xs with
        | Construct ({ name; _ }, [ Ptr { contents = Tuple [ x; rest ] } ])
          when String.equal name (Prelude_path.canonical "Cons") ->
          con "Cons" [ ptr (tuple [ x; append rest ys ]) ]
        | Construct ({ name; _ }, [])
          when String.equal name (Prelude_path.canonical "Nil") -> ys
        | _ ->
          raise_s [%message "Syntax_value.append: not a list" (xs : Value.t)]
      ;;
    end)

  let reflected_decl decl ~decl_type =
    option
      (fun ({ params; kind } : Reflected_decl.t) ->
        let ref_ kind name = Value.Ref { kind; name } in
        let pair f g (a, b) = Value.Tuple [ f a; g b ] in
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
      let args =
        match Lift.indirect_arity (Prelude_path.to_string_hum name), args with
        | None, _ -> args
        | Some arity, [ Ptr cell ] ->
          if arity = 1
          then [ !cell ]
          else (
            match !cell with
            | Tuple args when List.length args = arity -> args
            | _ -> unexpected ~expected:"an indirect constructor's tuple" !cell)
        | Some _, _ ->
          unexpected ~expected:"an indirect constructor's pointer" value
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
      | "Cons", { values = [| Ptr cell |]; _ } ->
        (match !cell with
         | Tuple [ x; tail ] ->
           loop (i + 1) (within (Element i) f x :: acc) tail
         | other -> unexpected ~expected:"a list cell's pair" other)
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

  (* The modules of a path are capitalized, its last name of [kind]. A leading
     [.] makes it a root path. *)
  let path kind value : Decl_path.t =
    let text = string value in
    let root, text =
      match String.chop_prefix text ~prefix:"." with
      | Some text -> true, text
      | None -> false, text
    in
    let names = String.split text ~on:'.' in
    let #(modules, last) = List.split_n names (List.length names - 1) in
    if List.for_all modules ~f:(Ident_kind.accepts Upper)
       && List.for_all last ~f:(Ident_kind.accepts kind)
    then (
      let path = Path.of_list_exn names in
      if root then Root path else Path path)
    else
      unexpected
        ~expected:
          (sprintf
             "a path, optionally from the root ([.M.x]), of capitalized \
              identifiers ending in %s"
             (Ident_kind.describe kind))
        value
  ;;

  let decl_path kind value ~loc =
    Located.create
      ~loc
      (variant "Stag.Path.t" value ~f:(fun constructor args : Decl_path.t ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Name", 1 -> arg 0 (path kind)
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

  (* Where a type is, so which of [t @ m], [t @@ m] and [t with ...] it may be,
     as the parser allows. *)
  module Type_position = struct
    type t =
      | Plain
      | Arrow_side
      | Member
      | Annotation
      | Type_arg
  end

  let rec type_expr ?(at = Type_position.Plain) value ~loc : Type_expr.t =
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
           | "Arrow", 2 ->
             Arrow
               ( arg 0 (type_expr ~at:Arrow_side ~loc)
               , arg 1 (type_expr ~at:Arrow_side ~loc) )
           | "Tuple", 1 -> Tuple (arg 0 (components ~min:0 (type_expr ~loc)))
           | "Code", 1 ->
             arg 0 (fun code : Type_expr.desc ->
               Code
                 { inner = field "inner" (type_expr ~loc) code
                 ; context = field "context" (row ~labels:Lower ~loc) code
                 })
           | "Record", 1 -> Record (arg 0 (row ~labels:Label ~loc))
           | "Variant", 1 -> Variant (arg 0 (row ~labels:Label ~loc))
           | "Mut", 1 -> Mut (arg 0 (type_expr ~loc))
           | "Immut", 1 -> Immut (arg 0 (type_expr ~loc))
           | "Splice", 1 -> Splice (arg 0 (expr ~loc))
           | "Moded", 2 ->
             (match at with
              | Arrow_side ->
                Moded (arg 0 (type_expr ~loc), arg 1 (mode_expr ~loc))
              | Plain | Member | Annotation | Type_arg ->
                unexpected
                  ~expected:
                    "a type without a mode: a mode is only on a function \
                     type's argument or result"
                  value)
           | "Constrained", 2 ->
             (match at with
              | Annotation ->
                let constraint_ value : Mode_expr.Constraint.t =
                  let constraint_ : Mode_expr.Constraint.t =
                    match value with
                    | Value.Tuple [ lower; upper ] ->
                      { lower = within (Component 0) (mode_term ~loc) lower
                      ; upper = within (Component 1) (mode_term ~loc) upper
                      }
                    | value -> unexpected ~expected:"a pair" value
                  in
                  match Mode_expr.Constraint.check constraint_ with
                  | Ok () -> constraint_
                  | Error message ->
                    unexpected
                      ~expected:
                        (sprintf "a well-formed mode constraint (%s)" message)
                      value
                in
                (match arg 1 (list constraint_) with
                 | [] ->
                   unexpected ~expected:"at least one mode constraint" value
                 | constraints ->
                   Constrained (arg 0 (type_expr ~loc), constraints))
              | Plain | Arrow_side | Member | Type_arg ->
                unexpected
                  ~expected:
                    "a type without mode constraints: they are only on an \
                     annotation's type"
                  value)
           | "Modality", 2 ->
             (match at with
              | Type_arg | Member ->
                Modality (arg 0 (type_expr ~loc), arg 1 (mode_expr ~loc))
              | Plain | Arrow_side | Annotation ->
                unexpected
                  ~expected:
                    "a type without a modality: a modality is only on a type \
                     argument, a record field or a constructor's argument, as \
                     in [ptr[t @@ external]]"
                  value)
           | _ -> raise Bad_arguments))

  and mode_axis value : Mode_expr.Axis.t =
    variant
      "Stag.Mode_expr.Axis.t"
      value
      ~f:(fun constructor args : Mode_expr.Axis.t ->
        match constructor, Args.count args with
        | "Stage", 0 -> Stage
        | "Provenance", 0 -> Provenance
        | "Locality", 0 -> Locality
        | _ -> raise Bad_arguments)

  and mode_atom value : Mode_expr.Atom.t =
    variant
      "Stag.Mode_expr.Atom.t"
      value
      ~f:(fun constructor args : Mode_expr.Atom.t ->
        match Args.count args with
        | 0 -> Mode_expr.Atom.t_of_sexp (Atom constructor)
        | _ -> raise Bad_arguments)

  and mode_term value ~loc : Mode_expr.Term.t =
    variant
      "Stag.Mode_expr.Term.t"
      value
      ~f:(fun constructor args : Mode_expr.Term.t ->
        let arg i read = Args.read args i read in
        match constructor, Args.count args with
        | "Mode", 1 -> Mode (arg 0 (mode_expr ~loc))
        | "Axis", 2 -> Axis (arg 0 mode_axis, arg 1 (mode_expr ~loc))
        | "Meet", 2 -> Meet (arg 0 (mode_term ~loc), arg 1 mode_atom)
        | "Release", 1 -> Release (arg 0 (mode_term ~loc))
        | "Static_if_const", 1 -> Static_if_const (arg 0 (mode_term ~loc))
        | "Const_if_static", 1 -> Const_if_static (arg 0 (mode_term ~loc))
        | _ -> raise Bad_arguments)

  and mode_expr value ~loc : Mode_expr.t =
    let atom value =
      variant
        "Stag.Mode_expr.Atom.t"
        value
        ~f:(fun constructor args : Mode_expr.Atom.t ->
          match Args.count args with
          | 0 -> Mode_expr.Atom.t_of_sexp (Atom constructor)
          | _ -> raise Bad_arguments)
    in
    Located.create
      ~loc
      (variant
         "Stag.Mode_expr.t"
         value
         ~f:(fun constructor args : Mode_expr.desc ->
           let arg i read = Args.read args i read in
           match constructor, Args.count args with
           | "Var", 1 -> Var (arg 0 (ident Tyvar))
           | "Atoms", 1 ->
             arg 0 (fun value : Mode_expr.desc ->
               let atoms = list atom value in
               if List.is_empty atoms
                  || List.contains_dup
                       (List.map atoms ~f:Mode_expr.Atom.axis)
                       ~compare:Mode_expr.Axis.compare
               then
                 unexpected
                   ~expected:"atoms of distinct axes, at least one"
                   value
               else Atoms atoms)
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
        | "Ty", 1 -> Ty (arg 0 (type_expr ~at:Type_arg ~loc))
        | "Row", 1 -> Row (arg 0 (row ~labels:Label ~loc))
        | "Mode", 1 -> Mode (arg 0 (mode_expr ~loc))
        | _ -> raise Bad_arguments)

  and type_decl value ~loc : Type_decl.t =
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
             | "Mode", 1 -> Mode (arg 0 (ident Tyvar))
             | _ -> raise Bad_arguments))
    in
    { name = field "name" (name Lower ~loc) value
    ; params = field "params" (list param) value
    ; kind = field "kind" (type_decl_kind ~loc) value
    }

  and type_decl_kind value ~loc : Type_decl.Kind.t =
    variant
      "Stag.Type_decl.Kind.t"
      value
      ~f:(fun constructor args : Type_decl.Kind.t ->
        let arg i read = Args.read args i read in
        match constructor, Args.count args with
        | "Variant", 1 ->
          Variant
            (arg
               0
               (list
                  (pair (name Upper ~loc) (list (type_expr ~at:Member ~loc)))))
        | "Record", 1 ->
          Record
            (arg
               0
               (fields (pair (name Lower ~loc) (type_expr ~at:Member ~loc))))
        | "Alias", 1 -> Alias (arg 0 (type_expr ~loc))
        | "Abstract", 0 -> Abstract
        | "Splice", 1 -> Splice (Located.create ~loc (arg 0 (expr ~loc)))
        | _ -> raise Bad_arguments)

  and binding ~loc = pair (name Binder ~loc) (annot ~loc)

  and annot value ~loc : Annot.t =
    { ty = field "ty" (option (type_expr ~at:Annotation ~loc)) value
    ; mode = field "mode" (option (mode_expr ~loc)) value
    }

  and pattern value ~loc : Pattern.t =
    Located.create
      ~loc
      (variant "Stag.Pattern.t" value ~f:(fun constructor args : Pattern.desc ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Any", 0 -> Any
         | "Var", 1 -> Var (arg 0 (ident Lower))
         | "Lit", 1 -> Lit (arg 0 lit)
         | "Tuple", 1 -> Tuple (arg 0 (components ~min:2 (pattern ~loc)))
         | "Deref", 1 -> Deref (arg 0 (pattern ~loc))
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

  and expr value ~loc : Expr.t =
    let labelled = list (pair (name Label ~loc) (expr ~loc)) in
    Located.create
      ~loc
      (variant "Stag.Expr.t" value ~f:(fun constructor args : Expr.desc ->
         let arg i read = Args.read args i read in
         match constructor, Args.count args with
         | "Lit", 1 -> Lit (arg 0 lit)
         | "Var", 2 ->
           Var
             ( arg 0 (fun value -> (decl_path Lower value ~loc).txt)
             , arg 1 (annot ~loc) )
         | "App", 2 -> App (arg 0 (expr ~loc), arg 1 (expr ~loc))
         | "Let", 5 ->
           Let
             ( arg 0 (name Binder ~loc)
             , arg 1 (list (binding ~loc))
             , arg 2 (annot ~loc)
             , arg 3 (expr ~loc)
             , arg 4 (expr ~loc) )
         | "Let_mut", 4 ->
           Let_mut
             ( arg 0 (name Binder ~loc)
             , arg 1 (annot ~loc)
             , arg 2 (expr ~loc)
             , arg 3 (expr ~loc) )
         | "Assign", 2 ->
           Assign (arg 0 (decl_path Lower ~loc), arg 1 (expr ~loc))
         | "Store", 2 -> Store (arg 0 (expr ~loc), arg 1 (expr ~loc))
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
         | "Deref", 1 -> Deref (arg 0 (expr ~loc))
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
         | "Struct_quote", 1 -> Struct_quote (arg 0 (list (toplevel ~loc)))
         | "Stack", 1 -> Stack (arg 0 (expr ~loc))
         | "Exclave", 1 -> Exclave (arg 0 (expr ~loc))
         | "Nontail", 1 ->
           (match arg 0 (expr ~loc) with
            | { txt = App _; _ } as app -> Nontail app
            | _ -> unexpected ~expected:"an application under Nontail" value)
         | _ -> raise Bad_arguments))

  and toplevel value ~loc : Toplevel.t =
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
               , arg 2 (annot ~loc)
               , arg 3 (expr ~loc) )
           | "Let_mut", 3 ->
             Let_mut
               (arg 0 (name Binder ~loc), arg 1 (annot ~loc), arg 2 (expr ~loc))
           | "Type", 1 -> Type (arg 0 (type_decl ~loc))
           | "Exception", 2 ->
             Exception (arg 0 (name Upper ~loc), arg 1 (list (type_expr ~loc)))
           | "External", 1 ->
             arg 0 (fun external_ : Toplevel.desc ->
               External
                 { name = field "name" (name Lower ~loc) external_
                 ; ty = field "ty" (type_expr ~loc) external_
                 ; symbol =
                     Located.create ~loc (field "symbol" string external_)
                 })
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
  let to_value t = To_value.type_expr t
  let of_value = Of_value.(run (fun value ~loc -> type_expr value ~loc))
end

module Type_decl = struct
  let to_value t = To_value.type_decl t
  let of_value = Of_value.(run type_decl)

  module Kind = struct
    let to_value t = To_value.type_decl_kind t
    let of_value = Of_value.(run type_decl_kind)
  end
end

module Pattern = struct
  let to_value = To_value.pattern
  let of_value = Of_value.(run pattern)
end

module Expr = struct
  let to_value t = To_value.expr t
  let of_value = Of_value.(run expr)
end

module Toplevel = struct
  let to_value t = To_value.toplevel t
  let of_value = Of_value.(run toplevel)
end

module Program = struct
  let to_value t = To_value.program t
  let of_value = Of_value.(run program)
end

let type_ref_path path =
  let names = To_value.list (fun s -> Value.String s) in
  match path with
  | `Prelude path -> To_value.con "Stag.Type_ref.Path.Prelude" [ names path ]
  | `Program path -> To_value.con "Stag.Type_ref.Path.Program" [ names path ]
;;
