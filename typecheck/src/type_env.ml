open! Import
open Syntax
module Value_layout = Stag_types.Value_layout

module Constructor = struct
  type t =
    { name : string
    ; type_name : string
    ; index : int
    ; args : Type.ty list
    ; result : Type.ty
    ; modalities : Member_modality.t list
    }
  [@@deriving sexp_of]
end

module Field = struct
  type t =
    { record_type : Typed.Record_type.t
    ; record : Type.ty
    ; index : int
    ; ty : Type.ty
    ; modality : Member_modality.t
    }
  [@@deriving sexp_of]

  let name t = t.record_type.fields.(t.index)
end

module Pending_layouts = struct
  (** Of a type a splice is computing: whether it has pointers whatever its
      arguments, and for each argument of a type or row, whether one with
      pointers gives it pointers. *)
  type t =
    { whole : Mode.Layout.t
    ; args : Mode.Layout.t option list
    }
end

module Decl = struct
  type t =
    { mutable decl : Type_decl.t (* Its definition once computed, if spliced. *)
    ; name : string
    ; module_ : Module_scope.t
    ; params : Type.arg list Lazy.t
    ; mutable wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable used_while_defined : bool
    ; mutable pending_layouts : Pending_layouts.t option
    (** Asked for while its splice computes its definition, until it is known. *)
    ; mutable prepared :
        (Signature.Type_decl.Kind.t * Reflected_decl.Kind.t option) option
    ; mutable value_parameters : Int.Set.t option
    ; mutable variances : Variance.t list option
    ; mutable checked : Signature.Type_decl.t option
    ; mutable failed : bool (** Checking it reported an error. *)
    ; mutable reflected : Reflected_decl.t option
    (** Once checked, unless abstract. *)
    }

  let params t = force t.params

  let param_map t ~params =
    List.zip_exn
      (List.map t.decl.params ~f:(fun param -> Type_decl.Param.name param.txt))
      params
    |> String.Map.of_alist_exn
  ;;
end

module Exception_decl = struct
  type t =
    { args : Type_expr.t list
    ; malformed : bool
    ; module_ : Module_scope.t
    ; wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable checked : Constructor.t option
    }
end

module Type_splice = struct
  module State = struct
    type t =
      | Evaluating
      | Evaluated of (Type_expr.t, Typecheck_error.t) Result.t
  end

  type t =
    { expr : Expr.t
    ; mutable state : State.t
    }
end

(* Whether what a [Link] reaches is pointer-free, if it reaches only the
   variables of its [Type.frontier]: then that and which of their layouts it
   depends on are the same until the frontier is, or a variable of it is
   generalized, or made unbound again. *)
module Link_layout = struct
  type t =
    { target : Type.ty
    ; frontier : (Type.ty, Nothing.t) Type.Var.t list
    ; generic : bool list
    ; pointer_free : bool
    ; depends_on : (Type.ty, Nothing.t) Type.Var.t list
    }

  let generic (frontier : (Type.ty, Nothing.t) Type.Var.t list) =
    List.map frontier ~f:(fun (var : _ Type.Var.t) ->
      match var.state with
      | Generic -> true
      | Unbound _ | Link _ -> false)
  ;;

  (* With undo, as the structure between [target] and [frontier] may be. *)
  let set table (var : (Type.ty, Nothing.t) Type.Var.t) t =
    let old = Hashtbl.find table var.id in
    Mode.Private.Undo.change
      ~undo:(fun () ->
        match old with
        | Some old -> Hashtbl.set table ~key:var.id ~data:old
        | None -> Hashtbl.remove table var.id)
      (fun () -> Hashtbl.set table ~key:var.id ~data:t)
  ;;

  let find table (var : (Type.ty, Nothing.t) Type.Var.t) ~target ~frontier =
    match Hashtbl.find table var.id with
    | Some t
      when phys_equal t.target target
           && List.equal phys_equal t.frontier frontier
           && List.equal Bool.equal t.generic (generic frontier) -> Some t
    | Some _ | None -> None
  ;;
end

type t =
  { fresh : Fresh.t
  ; evaluate_splice :
      Module_scope.t
      -> Expr.t
      -> loc:Loc.t
      -> (Type_expr.t, Typecheck_error.t) Result.t
  ; type_splices : Type_splice.t list Loc.Table.t
      (* By location, then physically: each splice is evaluated once. *)
  ; types : Decl.t String.Table.t
  ; exceptions : Exception_decl.t String.Table.t
  ; constructors : Constructor.t String.Table.t
  ; fields : Field.t String.Table.t
  ; constructor_types : string String.Table.t
  ; field_types : string String.Table.t
  (** The type declaring each constructor and field, whose entry in
      [constructors] or [fields] is added once it is checked. *)
  ; declared_types : Loc.t String.Table.t
  ; declared_members : Loc.t String.Table.t
  (** Constructors and fields, whose names differ by case. *)
  ; references : Name_reference.t Loc.Table.t option
  ; report : Typecheck_error.t -> unit
  ; layouts : Var_layout.t Set_once.t
  (** Set by [create]: what a layout is depends on the types declared. *)
  ; link_layouts : Link_layout.t Int.Table.t
  (** Of [Link]s of type variables, by id. *)
  }

let create_tables fresh ~record_references ~report ~evaluate_splice =
  { fresh
  ; evaluate_splice
  ; type_splices = Loc.Table.create ()
  ; report
  ; types = String.Table.create ()
  ; exceptions = String.Table.create ()
  ; constructors = String.Table.create ()
  ; fields = String.Table.create ()
  ; constructor_types = String.Table.create ()
  ; field_types = String.Table.create ()
  ; declared_types = String.Table.create ()
  ; declared_members = String.Table.create ()
  ; references = Option.some_if record_references (Loc.Table.create ())
  ; layouts = Set_once.create ()
  ; link_layouts = Int.Table.create ()
  }
;;

let builtins =
  List.map
    [ "int"; "bool"; "string"; "char"; "unit"; Type.exn_name; Type.error_name ]
    ~f:(fun name ->
      { Type_decl.name = Located.create name
      ; params = []
      ; kind = Abstract
      ; nonrec_ = false
      })
  @ [ { Type_decl.name = Located.create "ptr"
      ; params =
          [ Located.create (Type_decl.Param.Ty "a")
          ; Located.create (Type_decl.Param.Mode "p")
          ]
      ; kind = Abstract
      ; nonrec_ = false
      }
    ]
;;

let find_resolved t module_ namespace path ~loc ~unbound ~find =
  let%bind.Result name =
    Module_scope.find module_ namespace path ~loc ~unbound
  in
  find t name ~loc
;;

let resolve_type module_ (path : Decl_path.t) ~loc =
  match path with
  | Resolved name -> Ok name
  | Path _ | Root _ ->
    Module_scope.find module_ Type path ~loc ~unbound:(fun path ->
      Unbound_type path)
;;

let rec has_splice (expr : Type_expr.t) =
  match expr.txt with
  | Splice _ -> true
  | Var _ -> false
  | Con (_, args) ->
    List.exists args ~f:(function
      | Ty ty -> has_splice ty
      | Row row -> row_has_splice row
      | Mode _ -> false)
  | Arrow (ty1, ty2) -> has_splice ty1 || has_splice ty2
  | Tuple tys -> List.exists tys ~f:has_splice
  | Code { inner; context } -> has_splice inner || row_has_splice context
  | Record row | Variant row -> row_has_splice row
  | Mut ty | Immut ty | Moded (ty, _) | Constrained (ty, _) | Modality (ty, _)
    -> has_splice ty

and row_has_splice (row : Type_expr.row) =
  match row.txt with
  | Empty | Var _ -> false
  | Extend (_, ty, rest) -> has_splice ty || row_has_splice rest
;;

let rec kind_has_splice : Type_decl.Kind.t -> bool = function
  | Splice _ -> true
  | Abstract -> false
  | Alias ty -> has_splice ty
  | Variant constructors ->
    List.exists constructors ~f:(fun (_, args) ->
      List.exists args ~f:has_splice)
  | Record fields -> List.exists fields ~f:(fun (_, ty) -> has_splice ty)
  | Malformed kind -> kind_has_splice kind
;;

(* What a splice gives may not contain splices, so that evaluating splices ends.
   If its evaluation is abandoned (raising [Uses_unfinished]), it is evaluated
   anew. *)
let evaluate_splice t module_ (expr : Expr.t) ~loc =
  let splices = Hashtbl.find_multi t.type_splices expr.loc in
  match List.find splices ~f:(fun splice -> phys_equal splice.expr expr) with
  | Some { state = Evaluated result; _ } -> result
  | Some { state = Evaluating; _ } ->
    Error (Typecheck_error.create ~loc Type_splice_cycle)
  | None ->
    let splice = { Type_splice.expr; state = Evaluating } in
    Hashtbl.add_multi t.type_splices ~key:expr.loc ~data:splice;
    (match t.evaluate_splice module_ expr ~loc with
     | exception exn ->
       Hashtbl.change t.type_splices expr.loc ~f:(fun splices ->
         match
           List.filter (Option.value splices ~default:[]) ~f:(fun splice' ->
             not (phys_equal splice splice'))
         with
         | [] -> None
         | splices -> Some splices);
       raise exn
     | result ->
       let result =
         match result with
         | Ok generated when has_splice generated ->
           Result.Error (Typecheck_error.create ~loc Nested_type_splice)
         | Ok _ | Error _ -> result
       in
       splice.state <- Evaluated result;
       result)
;;

let record_reference t ~declared name ~loc =
  Option.iter t.references ~f:(fun references ->
    Option.iter (Hashtbl.find declared name) ~f:(fun declaration ->
      Hashtbl.set references ~key:loc ~data:{ Name_reference.name; declaration }))
;;

let record_type_reference t name ~loc =
  record_reference t ~declared:t.declared_types name ~loc
;;

let record_member_reference t name ~loc =
  record_reference t ~declared:t.declared_members name ~loc
;;

let references t =
  Option.map t.references ~f:(fun references ->
    Hashtbl.to_alist references
    |> List.sort ~compare:(Comparable.lift Loc.compare ~f:fst))
;;

let find_type t module_ path ~loc =
  let%bind.Result name = resolve_type module_ path ~loc in
  match Hashtbl.find t.types name with
  | Some decl ->
    (match path with
     | Path _ | Root _ -> record_type_reference t name ~loc
     | Resolved _ -> ());
    Ok decl
  | None ->
    Error
      (Typecheck_error.create ~loc (Unbound_type (Path (Path.of_string name))))
;;

type env = t

module Annotation_vars = struct
  type t =
    { fresh : Fresh.t
    ; layouts : Var_layout.t
    ; rank : Type.Rank.t
    ; tys : Type.ty String.Table.t
    ; rows : Type.row String.Table.t
    ; modes : Mode.Var.t String.Table.t
    }

  let create (env : env) ~rank =
    { fresh = env.fresh
    ; layouts = Set_once.get_exn env.layouts
    ; rank
    ; tys = String.Table.create ()
    ; rows = String.Table.create ()
    ; modes = String.Table.create ()
    }
  ;;

  (* A mode variable's name is not also a type or row variable's (a type and a
     row variable may share one, as in [<'r>{..'r}]). *)
  let only_kind name ~loc ~(in_ : _ Hashtbl.t) ~others ~default =
    match Hashtbl.find in_ name with
    | Some var -> Ok var
    | None when List.exists others ~f:(fun other -> other name) ->
      Error (Typecheck_error.create ~loc (Type_variable_kind_mismatch name))
    | None ->
      let var = default () in
      Hashtbl.set in_ ~key:name ~data:var;
      Ok var
  ;;

  let ty t name ~loc =
    only_kind
      name
      ~loc
      ~in_:t.tys
      ~others:[ Hashtbl.mem t.modes ]
      ~default:(fun () -> Type.TVar (Fresh.var t.fresh ~rank:t.rank))
  ;;

  let row t name ~loc =
    only_kind
      name
      ~loc
      ~in_:t.rows
      ~others:[ Hashtbl.mem t.modes ]
      ~default:(fun () -> Type.RVar (Fresh.var t.fresh ~rank:t.rank))
  ;;

  let fresh_mode t = Mode.Var.create (Fresh.modes t.fresh) ~rank:t.rank

  let mode t name ~loc =
    if String.equal name "_"
    then Ok (fresh_mode t)
    else
      only_kind
        name
        ~loc
        ~in_:t.modes
        ~others:[ Hashtbl.mem t.tys; Hashtbl.mem t.rows ]
        ~default:(fun () -> fresh_mode t)
  ;;

  (* In a constraint, [_] would relate nothing else. *)
  let related t name ~loc =
    if String.equal name "_"
    then Error (Typecheck_error.create ~loc Anonymous_mode_in_constraint)
    else mode t name ~loc
  ;;
end

(* A mode atom written as a type argument, as in [cell[t, external]]. *)
let atom_of_path (path : Path.t) =
  match (path :> string list) with
  | [ name ] -> Syntax.Mode_expr.Atom.of_string name
  | _ -> None
;;

(* An argument as of its parameter's kind, [None] if of another: a mode variable
   or a single mode atom parses as a type. *)
let resolve_arg (param : Type_decl.Param.t) (arg : Type_expr.arg)
  : Type_expr.arg option
  =
  match param, arg with
  | Mode _, Ty { txt = Var name; loc } ->
    Some (Mode (Located.create ~loc (Mode_expr.Var name)))
  | Mode _, Ty { txt = Con ({ txt = Path path; _ }, []); loc } ->
    Option.map (atom_of_path path) ~f:(fun atom ->
      Type_expr.Mode (Located.create ~loc (Mode_expr.Atoms [ atom ])))
  | Mode _, Mode _ | Ty _, (Ty _ | Row _) | Row _, Row _ -> Some arg
  | Mode _, (Ty _ | Row _) | Ty _, Mode _ | Row _, (Ty _ | Mode _) -> None
;;

let arg_loc : Type_expr.arg -> Loc.t = function
  | Ty { loc; _ } | Row { loc; _ } | Mode { loc; _ } -> loc
;;

(* [ptr[t @@ m]] is [ptr[t, m]], and [ptr[t]] is [ptr[t, nonportable gc]]. *)
module Ptr_sugar = struct
  type t =
    | Modality of Loc.t
    | Default
    | Args

  let desugar (args : Type_expr.arg list) ~loc =
    match args with
    | [ Ty { txt = Modality (ty, mode); loc } ] ->
      [ Type_expr.Ty ty; Mode mode ], Modality loc
    | [ ((Ty _ | Row _) as arg) ] ->
      ( [ arg; Mode (Located.create ~loc (Mode_expr.Atoms [ Nonportable; Gc ])) ]
      , Default )
    | args -> args, Args
  ;;

  let resugar t (args : Type_expr.arg list) =
    match t, args with
    | Modality loc, [ Ty ty; Mode mode ] ->
      [ Type_expr.Ty (Located.create ~loc (Type_expr.Modality (ty, mode))) ]
    | Default, [ arg; Mode _ ] -> [ arg ]
    | (Modality _ | Default | Args), args -> args
  ;;
end

module Vars = struct
  module Bound = struct
    type t = { f : 'a. 'a Mode.Axis.t -> 'a -> (unit, Mode.Error.t) Result.t }
  end

  type t =
    | Params of Type.arg String.Map.t
    | Annotation of Annotation_vars.t

  let error ~loc kind = Error (Typecheck_error.create ~loc kind)

  let ty t name ~loc : (Type.ty, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Annotation_vars.ty vars name ~loc
    | Params params ->
      (match Map.find params name with
       | Some (ATy ty) -> Ok ty
       | Some (ARow _ | AMode _) ->
         error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Unbound_type_variable name))
  ;;

  (* Whether an unmarked context entry is assigned: either, in an annotation,
     but not in a declaration, which can't introduce a variable. *)
  let assigned : t -> Type.presence = function
    | Annotation vars -> PVar (Fresh.var vars.fresh ~rank:vars.rank)
    | Params _ -> Absent
  ;;

  let row t name ~loc : (Type.row, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Annotation_vars.row vars name ~loc
    | Params params ->
      (match Map.find params name with
       | Some (ARow row) -> Ok row
       | Some (ATy _ | AMode _) -> error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Unbound_type_variable name))
  ;;

  (* Unwritten, in an annotation any (it is never printed, so reads back as what
     it annotates has), in a declaration the default. *)
  let curry : t -> Arrow_modes.Side.t = function
    | Annotation vars -> Var (Annotation_vars.fresh_mode vars)
    | Params _ -> Arrow_modes.default_curry
  ;;

  (* In an annotation, a variable fixed to [const], so that errors say why; in a
     declaration, [const]. *)
  let fixed t (const : Mode.Const.t) ~reason ~loc : Arrow_modes.Side.t =
    match t with
    | Params _ -> Fixed const
    | Annotation vars ->
      let var = Annotation_vars.fresh_mode vars in
      let reason = Mode.Reason.create ~loc reason in
      (match
         ( Mode.Var.at_least_const var const reason
         , Mode.Var.at_most_const var const reason )
       with
       | Ok (), Ok () -> ()
       | Error _, _ | _, Error _ ->
         raise_s [%message "a fresh mode variable is any constant"]);
      Var var
  ;;

  (* An arrow side written without a mode: the default. *)
  let unwritten t ~default ~loc =
    fixed t default ~reason:"its type is written without a mode" ~loc
  ;;

  let mode_side t ({ txt; loc } : Syntax.Mode_expr.t) ~default
    : (Arrow_modes.Side.t, Typecheck_error.t) Result.t
    =
    (* A mode parameter's locality is [global] unless written with it. *)
    let var_side name (written : Mode.Locality.t option)
      : (Arrow_modes.Side.t, Typecheck_error.t) Result.t
      =
      let locality = Option.value written ~default:Global in
      let why () =
        Mode.Reason.create
          ~loc
          (match written with
           | Some locality ->
             sprintf
               "its type is written %s"
               (Mode.Locality.sexp_of_t locality
                |> Sexp.to_string
                |> String.lowercase)
           | None ->
             "it is a mode parameter on an arrow in a declaration, which is \
              global")
      in
      match t with
      | Annotation vars ->
        Result.map (Annotation_vars.mode vars name ~loc) ~f:(fun var ->
          match written with
          | Some _ -> Arrow_modes.Side.Var_at { var; locality; why = why () }
          | None -> Var var)
      | Params params ->
        (match Map.find params name with
         | Some (AMode (Fixed { stage; provenance })) ->
           Ok (Arrow_modes.Side.Fixed { stage; provenance; locality })
         | Some (AMode (Var var)) -> Ok (Var_at { var; locality; why = why () })
         | Some (ATy _ | ARow _) ->
           error ~loc (Type_variable_kind_mismatch name)
         | None -> error ~loc (Mode_variable_in_declaration name))
    in
    match txt with
    | Var name -> var_side name None
    | Var_at (atom, name) -> var_side name (Some (Mode_atoms.locality atom))
    | Atoms atoms ->
      let%map.Result const = Mode_atoms.declared atoms ~default ~loc in
      fixed
        t
        const
        ~reason:
          (sprintf
             "it is annotated %s"
             (List.map atoms ~f:Syntax.Mode_expr.Atom.to_string
              |> String.concat ~sep:" "))
        ~loc
  ;;

  (* A mode argument: of a type's mode parameter, or what the cell of a
     [ptr[t @@ mode]] may hold, which are on stage and provenance only. *)
  let mode_arg t ({ txt; loc } : Syntax.Mode_expr.t)
    : (Mode_arg.t, Typecheck_error.t) Result.t
    =
    match t, txt with
    | Annotation vars, Var name ->
      Result.map (Annotation_vars.mode vars name ~loc) ~f:(fun var ->
        Mode_arg.Var var)
    | Params params, Var name ->
      (match Map.find params name with
       | Some (AMode mode) -> Ok mode
       | Some (ATy _ | ARow _) -> error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Mode_variable_in_declaration name))
    | _, Var_at _ -> error ~loc Locality_in_mode_argument
    | _, Atoms atoms ->
      if List.exists atoms ~f:(fun atom ->
           match Syntax.Mode_expr.Atom.axis atom with
           | Locality -> true
           | Stage | Provenance -> false)
      then error ~loc Locality_in_mode_argument
      else (
        let { Mode_arg.Const.stage; provenance } = Mode_arg.Const.default in
        let%map.Result const =
          Mode_atoms.declared
            atoms
            ~default:{ stage; provenance; locality = Global }
            ~loc
        in
        Mode_arg.Fixed (Mode_arg.Const.of_mode const))
  ;;

  let rec term_loc : Syntax.Mode_expr.Term.t -> Loc.t = function
    | Mode mode | Axis (_, mode) -> mode.loc
    | Meet (t, _) | Release t | Static_if_const t | Const_if_static t ->
      term_loc t
  ;;

  let rec term_var : Syntax.Mode_expr.Term.t -> string option = function
    | Mode { txt = Var name | Var_at (_, name); _ }
    | Axis (_, { txt = Var name | Var_at (_, name); _ }) -> Some name
    | Mode { txt = Atoms _; _ } | Axis (_, { txt = Atoms _; _ }) -> None
    | Meet (t, _) | Release t | Static_if_const t | Const_if_static t ->
      term_var t
  ;;

  (* A term of one variable as [m(var)] on the right of [<=], or, on the left,
     as the morphism whose left adjoint it applies. *)
  let rec term vars (expr : Syntax.Mode_expr.Term.t) ~loc =
    let open Result.Let_syntax in
    let axis : Syntax.Mode_expr.Axis.t -> Mode.Axis.Any.t = function
      | Stage -> T Stage
      | Provenance -> T Provenance
      | Locality -> T Locality
    in
    (* [m1] applied after [m2]'s left adjoint, on the left; [m1] of [m2], on the
       right: [Morphism.compose m2 m1] and [compose m1 m2]. *)
    let under t m ~left =
      let%map var, inner = term vars t ~loc in
      ( var
      , if left
        then Mode.Morphism.compose inner m
        else Mode.Morphism.compose m inner )
    in
    match expr with
    | Axis (on, { txt = Var name; loc }) ->
      let%map var = Annotation_vars.related vars name ~loc in
      var, Mode.Morphism.identity (axis on)
    | Meet (t, atom) ->
      let (T (on, point)) = Mode_atoms.point atom in
      let module C = (val Mode.Axis.chain on) in
      under t (Mode.Morphism.meet_with (T on) (C.index point)) ~left:true
    | Release t -> under t Mode.Morphism.release ~left:true
    | Static_if_const t -> under t Mode.Morphism.const_is_static ~left:false
    | Const_if_static t -> under t Mode.Morphism.runtime_is_gc ~left:false
    | Axis (_, { txt = Var_at _; _ }) ->
      Error
        (Typecheck_error.create
           ~loc
           (Invalid_mode Syntax.Mode_expr.var_at_in_constraint))
    | Mode _ | Axis (_, { txt = Atoms _; _ }) ->
      Error
        (Typecheck_error.create
           ~loc
           (Invalid_mode "a constraint on one axis relates two mode variables"))
  ;;

  (* Of ['a] or [..'r]. *)
  let layout vars name ~loc =
    let%map.Result ty =
      match String.chop_prefix name ~prefix:"..'" with
      | Some name ->
        let%map.Result row = Annotation_vars.row vars name ~loc in
        Type.TRecord { id = -1; row }
      | None ->
        Annotation_vars.ty
          vars
          (String.chop_prefix_if_exists name ~prefix:"'")
          ~loc
    in
    Var_layout.layout_of vars.layouts ty
  ;;

  (* [lower <= upper]: between variables, on every axis or the one named, or
     bounding a variable by the atoms, on their axes. *)
  let rec mode_constraint t (constraint_ : Syntax.Mode_expr.Constraint.t) ~loc
    : (unit, Typecheck_error.t) Result.t
    =
    let reason =
      Mode.Reason.create
        ~loc
        (sprintf
           "it is annotated %s"
           (Syntax.Mode_expr.Constraint.to_string constraint_))
    in
    let checked result =
      Result.map_error result ~f:(fun error ->
        Typecheck_error.create ~loc (Mode_error error))
    in
    match t, constraint_ with
    | Params _, Pointer_free name ->
      error ~loc (Mode_variable_in_declaration name)
    | Annotation vars, Pointer_free name ->
      (match%bind.Result layout vars name ~loc with
       | Pointer_free -> Ok ()
       | Has_pointers ->
         error
           ~loc
           (Invalid_mode
              (sprintf "%s has pointers, so is not pointer_free" name))
       | Depends layouts ->
         checked
           (List.fold_result layouts ~init:() ~f:(fun () layout ->
              Mode.Layout.bound_by layout reason)))
    | _, Submode { lower; upper } -> submode t ~lower ~upper ~loc

  and submode t ~lower ~upper ~loc =
    let constraint_ : Syntax.Mode_expr.Constraint.t =
      Submode { lower; upper }
    in
    let reason =
      Mode.Reason.create
        ~loc
        (sprintf
           "it is annotated %s"
           (Syntax.Mode_expr.Constraint.to_string constraint_))
    in
    let bound (atoms : Syntax.Mode_expr.Atom.t list) ~(f : Bound.t) =
      List.fold_result atoms ~init:() ~f:(fun () atom ->
        let (T (axis, point)) = Mode_atoms.point atom in
        f.f axis point)
    in
    let checked result =
      Result.map_error result ~f:(fun error ->
        Typecheck_error.create ~loc (Mode_error error))
    in
    match t, lower, upper with
    | Params _, _, _ ->
      (match Option.first_some (term_var lower) (term_var upper) with
       | Some name -> error ~loc (Mode_variable_in_declaration name)
       | None ->
         error ~loc (Invalid_mode "a constraint must be on a mode variable"))
    | Annotation vars, Mode lower, Mode upper ->
      (match lower.txt, upper.txt with
       | Var_at _, _ | _, Var_at _ ->
         error ~loc (Invalid_mode Syntax.Mode_expr.var_at_in_constraint)
       | Atoms _, Atoms _ ->
         error ~loc (Invalid_mode "a constraint must be on a mode variable")
       | Var lower_name, Var upper_name ->
         let%bind.Result lower = Annotation_vars.related vars lower_name ~loc in
         let%bind.Result upper = Annotation_vars.related vars upper_name ~loc in
         checked (Mode.Var.submode lower upper reason)
       | Var lower, Atoms atoms ->
         let%bind.Result var = Annotation_vars.related vars lower ~loc in
         checked
           (bound
              atoms
              ~f:
                { f = (fun axis point -> Mode.Var.at_most var axis point reason)
                })
       | Atoms atoms, Var upper ->
         let%bind.Result var = Annotation_vars.related vars upper ~loc in
         checked
           (bound
              atoms
              ~f:
                { f =
                    (fun axis point -> Mode.Var.at_least var axis point reason)
                }))
    | Annotation vars, lower, upper ->
      let%bind.Result () =
        Syntax.Mode_expr.Constraint.check constraint_
        |> Result.map_error ~f:(fun message ->
          Typecheck_error.create ~loc (Invalid_mode message))
      in
      let%bind.Result lower, left = term vars lower ~loc in
      let%bind.Result upper, right = term vars upper ~loc in
      checked
        (Mode.Var.relate lower (Mode.Morphism.compose left right) upper reason)
  ;;
end

(* An expansion of an alias: [knot], linked to the alias's body once elaborated,
   stands for the expansion. Each alias is expanded once per (physical)
   arguments during one elaboration, so references to it share the knot, also
   while it is being expanded, which closes a cycle. As the knot is a [Link],
   traversals of the expansion's shared occurrences take it once. It is
   [Generic] until linked, but only reachable from the elaboration, which stops
   at its first error and is then dropped. *)
module Expansion = struct
  type t =
    { args : Type.arg list
    ; knot : (Type.ty, Nothing.t) Type.Var.t
    ; mutable expanded : bool
    ; mutable reached_back : bool
    }

  let same_args t args =
    List.equal
      (fun (arg1 : Type.arg) (arg2 : Type.arg) ->
        match arg1, arg2 with
        | ATy ty1, ATy ty2 -> phys_equal ty1 ty2
        | ARow row1, ARow row2 -> phys_equal row1 row2
        | AMode mode1, AMode mode2 -> Mode_arg.same mode1 mode2
        | (ATy _ | ARow _ | AMode _), _ -> false)
      t.args
      args
  ;;
end

(* Like [List.map] then [Result.all], but stops at the first error. *)
let map_result xs ~f =
  List.fold_result xs ~init:[] ~f:(fun ys x ->
    let%map.Result y = f x in
    y :: ys)
  |> Result.map ~f:List.rev
;;

(* An application's result is computed at run time, so never [const]: neither is
   an arrow's result, which printing then leaves unsaid. *)
let result_floor (side : Arrow_modes.Side.t) ~loc =
  let never_const =
    Typecheck_error.create
      ~loc
      (Invalid_mode "the result of a function is never const")
  in
  match side with
  | Fixed { stage = Const; _ } -> Error never_const
  | Fixed { stage = Portable | Nonportable; _ } -> Ok ()
  | Var var | Var_at { var; _ } ->
    Mode.Var.at_least
      var
      Stage
      Portable
      (Mode.Reason.create ~loc "it is the result of a function")
    |> Result.map_error ~f:(fun (_ : Mode.Error.t) -> never_const)
;;

(* A mode argument is at least what its parameter's uses need of it, as a
   function's result is never [const]. *)
let mode_args_at_floors (decl : Decl.t) args ~loc =
  if not (Lazy.is_val decl.params)
  then Ok ()
  else
    List.zip_exn (force decl.params) args
    |> List.fold_result ~init:() ~f:(fun () (param, (arg : Type.arg)) ->
      match param, arg with
      | Type.AMode (Var param), AMode arg ->
        let floor = Mode.Var.lower param in
        (match arg with
         | Fixed const ->
           if Mode.Axis.Stage.compare const.stage floor.stage >= 0
              && Mode.Axis.Provenance.compare const.provenance floor.provenance
                 >= 0
           then Ok ()
           else
             Error
               (Typecheck_error.create
                  ~loc
                  (Invalid_mode
                     (sprintf
                        "this mode argument of %s must be at least %s"
                        decl.name
                        (List.filter_opt
                           [ Option.some_if
                               (not
                                  (Mode.Axis.Stage.equal
                                     floor.stage
                                     Mode.Axis.Stage.min))
                               (Mode.Axis.Stage.sexp_of_t floor.stage)
                           ; Option.some_if
                               (not
                                  (Mode.Axis.Provenance.equal
                                     floor.provenance
                                     Mode.Axis.Provenance.min))
                               (Mode.Axis.Provenance.sexp_of_t floor.provenance)
                           ]
                         |> List.map
                              ~f:(Fn.compose String.lowercase Sexp.to_string)
                         |> String.concat ~sep:" "))))
         | Var var ->
           let reason =
             Mode.Reason.create
               ~loc
               (sprintf "it is a mode argument of %s" decl.name)
           in
           (let%bind.Result () =
              Mode.Var.at_least var Stage floor.stage reason
            in
            Mode.Var.at_least var Provenance floor.provenance reason)
           |> Result.map_error ~f:(fun error ->
             Typecheck_error.create ~loc (Mode_error error)))
      | _ -> Ok ())
;;

(* Each elaborates to the type and the syntax with each type it names by its
   canonical name ([Resolved]), so that it names the same types wherever it is. *)
let rec elaborate'
  ?(curried = Mode.Locality.Global)
  t
  module_
  vars
  ~splices
  ~expansions
  (expr : Type_expr.t)
  : (Type.ty * Type_expr.t, Typecheck_error.t) Result.t
  =
  let elaborate = elaborate' t module_ vars ~splices ~expansions in
  let elaborate_row ?allow_mut row =
    elaborate_row ?allow_mut t module_ vars ~splices ~expansions row
  in
  let loc = expr.loc in
  let%map.Result ty, txt =
    match expr.txt with
    | Var name ->
      let%map.Result ty = Vars.ty vars name ~loc in
      ty, expr.txt
    | Con (path, args) ->
      elaborate_con t module_ vars ~splices ~expansions path args ~loc
    | Arrow (ty1, ty2) ->
      let side (ty : Type_expr.t) ~default ~elaborate =
        match ty.txt with
        | Moded (inner, mode) ->
          let%bind.Result elaborated, syntax = elaborate inner in
          let%map.Result side = Vars.mode_side vars mode ~default in
          elaborated, side, { ty with txt = Type_expr.Moded (syntax, mode) }
        | _ ->
          let%map.Result elaborated, syntax = elaborate ty in
          elaborated, Vars.unwritten vars ~default ~loc:ty.loc, syntax
      in
      let default = Arrow_modes.default_const in
      let%bind.Result ty1, side1, syntax1 = side ty1 ~default ~elaborate in
      (* A curried declaration's partial applications hold its arguments so far,
         so are as local as they are, as in OxCaml. *)
      let%bind.Result ty2, side2, syntax2 =
        match ty2.txt with
        | Arrow _ ->
          (* A named argument adds nothing: its locality isn't known. *)
          let curried =
            match side1 with
            | Fixed { locality; _ } | Var_at { locality; _ } ->
              Mode.Locality.join curried locality
            | Var var ->
              let lower = (Mode.Var.lower var).locality in
              if Mode.Locality.equal lower (Mode.Var.upper var).locality
              then Mode.Locality.join curried lower
              else curried
          in
          side
            ty2
            ~default:{ default with locality = curried }
            ~elaborate:(elaborate' ~curried t module_ vars ~splices ~expansions)
        | _ -> side ty2 ~default ~elaborate
      in
      let%map.Result () = result_floor side2 ~loc:syntax2.loc in
      ( Type.TArrow
          (ty1, ty2, { arg = side1; res = side2; curry = Vars.curry vars })
      , Type_expr.Arrow (syntax1, syntax2) )
    | Tuple tys ->
      let%map.Result tys = map_result tys ~f:elaborate in
      let tys, syntax = List.unzip tys in
      Type.TTuple tys, Type_expr.Tuple syntax
    | Code { inner; context } ->
      let%bind.Result inner, inner_syntax = elaborate inner in
      let%map.Result context, context_syntax = elaborate_row context in
      ( Type.TCode { inner; context }
      , Type_expr.Code { inner = inner_syntax; context = context_syntax } )
    | Record row ->
      let%bind.Result row, syntax = elaborate_row row ~allow_mut:false in
      let%map.Result () = in_record t row ~loc in
      Fresh.record t.fresh row, Type_expr.Record syntax
    | Variant row ->
      let%bind.Result row, syntax = elaborate_row row ~allow_mut:false in
      let%map.Result () = in_record t row ~loc in
      Fresh.variant t.fresh row, Type_expr.Variant syntax
    | Splice e when splices ->
      let%bind.Result generated = evaluate_splice t module_ e ~loc in
      let%map.Result ty, syntax =
        elaborate generated
        |> Result.map_error ~f:(fun error ->
          Typecheck_error.create
            ~loc
            (In_type_splice { generated; error = error.kind }))
      in
      ty, syntax.txt
    | Splice _ -> Error (Typecheck_error.create ~loc Misplaced_type_splice)
    | Mut _ | Immut _ -> Error (Typecheck_error.create ~loc Misplaced_mut)
    | Moded (ty, mode) ->
      let%map.Result ty, syntax = elaborate ty in
      ty, Type_expr.Moded (syntax, mode)
    | Constrained (ty, constraints) ->
      let%bind.Result ty, syntax = elaborate ty in
      let%map.Result () =
        List.fold_result constraints ~init:() ~f:(fun () constraint_ ->
          Vars.mode_constraint
            vars
            constraint_
            ~loc:
              (match constraint_ with
               | Submode { lower; upper } ->
                 Loc.merge (Vars.term_loc lower) (Vars.term_loc upper)
               | Pointer_free _ -> loc))
      in
      ty, Type_expr.Constrained (syntax, constraints)
    | Modality _ -> Error (Typecheck_error.create ~loc Misplaced_modality)
  in
  ty, { expr with txt }

(* A row variable named in a code context may be in a record's too, and a row
   argument may be [mut], so a record's row may have been given assigned
   entries. *)
and in_record t row ~loc =
  Unify.in_record t.fresh row
  |> Result.map_error ~f:(Typecheck_error.unification_error ~loc)

(* Entries are [mut] only in contexts, and in row arguments, which may be. *)
and elaborate_row
  ?(allow_mut = true)
  t
  module_
  vars
  ~splices
  ~expansions
  (row : Type_expr.row)
  : (Type.row * Type_expr.row, Typecheck_error.t) Result.t
  =
  let rec go (row : Type_expr.row) ~seen =
    let%map.Result elaborated, txt =
      match row.txt with
      | Empty -> Ok (Type.REmpty, row.txt)
      | Extend ({ txt = name; loc }, _, _) when Set.mem seen name ->
        Error (Typecheck_error.create ~loc (Duplicate_row_entry name))
      | Extend (({ txt = name; loc = _ } as label), entry, rest) ->
        let marked (assigned : Type.presence) inner ~mark =
          let%map.Result ty, inner_syntax =
            elaborate' t module_ vars ~splices ~expansions inner
          in
          assigned, ty, { entry with txt = mark inner_syntax }
        in
        let%bind.Result (assigned : Type.presence), ty, ty_syntax =
          match entry.txt with
          | Mut inner when allow_mut ->
            marked Present inner ~mark:(fun ty -> Type_expr.Mut ty)
          | Immut inner when allow_mut ->
            marked Absent inner ~mark:(fun ty -> Type_expr.Immut ty)
          | _ ->
            let%map.Result ty, ty_syntax =
              elaborate' t module_ vars ~splices ~expansions entry
            in
            ( (if allow_mut then Vars.assigned vars else Type.Absent)
            , ty
            , ty_syntax )
        in
        let%map.Result rest, rest_syntax = go rest ~seen:(Set.add seen name) in
        ( Type.RExtend (name, Present, assigned, ty, rest)
        , Type_expr.Extend (label, ty_syntax, rest_syntax) )
      | Var { txt = name; loc } ->
        let%map.Result var = Vars.row vars name ~loc in
        var, row.txt
    in
    elaborated, { row with txt }
  in
  go row ~seen:String.Set.empty

and elaborate_con
  t
  module_
  vars
  ~splices
  ~expansions
  ({ txt = path; loc = path_loc } as located_path : Decl_path.t Located.t)
  args
  ~loc
  =
  let error ~loc kind = Error (Typecheck_error.create ~loc kind) in
  let%bind.Result decl = find_type t module_ path ~loc:path_loc in
  let%bind.Result () =
    if decl.failed then Error Typecheck_error.already_reported else Ok ()
  in
  let ptr_sugar = String.equal decl.name Type.ptr_name in
  let args, sugar =
    if ptr_sugar then Ptr_sugar.desugar args ~loc else args, Ptr_sugar.Args
  in
  let expected = List.length decl.decl.params in
  let got = List.length args in
  let%bind.Result () =
    if expected = got
    then Ok ()
    else error ~loc (Type_arity_mismatch { type_name = path; expected; got })
  in
  let%bind.Result args =
    map_result (List.zip_exn decl.decl.params args) ~f:(fun (param, arg) ->
      match param.txt, resolve_arg param.txt arg with
      | _, Some (Ty ty) ->
        let%map.Result ty, syntax =
          elaborate' t module_ vars ~splices ~expansions ty
        in
        Type.ATy ty, Type_expr.Ty syntax
      (* [{...}] parses as a row, but is a record type for a type parameter. *)
      | Ty _, Some (Row row) ->
        let%map.Result row, syntax =
          elaborate_row t module_ vars ~splices ~expansions row
        in
        Type.ATy (Fresh.record t.fresh row), Type_expr.Row syntax
      | (Row _ | Mode _), Some (Row row) ->
        let%map.Result row, syntax =
          elaborate_row t module_ vars ~splices ~expansions row
        in
        Type.ARow row, Type_expr.Row syntax
      | _, Some (Mode mode) ->
        let%map.Result mode_arg = Vars.mode_arg vars mode in
        Type.AMode mode_arg, Type_expr.Mode mode
      | _, None ->
        error
          ~loc:(arg_loc arg)
          (Type_arg_kind_mismatch { type_name = path; param = param.txt }))
  in
  let args, args_syntax = List.unzip args in
  let syntax =
    Type_expr.Con
      ( { located_path with txt = Resolved decl.name }
      , Ptr_sugar.resugar sugar args_syntax )
  in
  let%bind.Result kind = definition decl ~loc in
  let%bind.Result () = mode_args_at_floors decl args ~loc in
  let%map.Result ty =
    match (kind : Type_decl.Kind.t) with
    | Variant _ | Record _ | Abstract | Splice _ | Malformed _ ->
      Ok (Type.TCon (decl.name, args))
    | Alias body ->
      let instances = Hashtbl.find_multi expansions decl.name in
      (match
         List.find instances ~f:(fun expansion ->
           Expansion.same_args expansion args)
       with
       | Some expansion ->
         if not expansion.expanded then expansion.reached_back <- true;
         Ok (Type.TVar expansion.knot)
       | None ->
         if List.exists instances ~f:(fun expansion -> not expansion.expanded)
         then error ~loc (Non_regular_recursive_alias decl.name)
         else
           expand t decl ~args ~expansions body
           |> Result.map ~f:fst
           |> Result.map_error ~f:(fun (error : Typecheck_error.t) ->
             (* Only a [mut] argument gets a record assigned entries. *)
             match error.kind with
             | Unification_error (Assigned_in_record _) -> { error with loc }
             | _ -> error))
  in
  ty, syntax

(* Its definition, computed if spliced. While being computed, it is taken to be
   nominal, which [set_definition] checks. *)
and definition (decl : Decl.t) ~loc =
  match decl.decl.kind with
  | Splice _ when Module_scope.defining decl.module_ decl.decl.name.txt ->
    decl.used_while_defined <- true;
    Ok decl.decl.kind
  | Splice _ ->
    let%bind.Result () =
      Module_scope.force_definition decl.module_ decl.decl.name.txt ~loc
      |> Result.map_error ~f:(fun error ->
        if Typecheck_error.is_already_reported error then decl.failed <- true;
        error)
    in
    if decl.failed
    then Error Typecheck_error.already_reported
    else Ok decl.decl.kind
  | Variant _ | Record _ | Alias _ | Abstract | Malformed _ -> Ok decl.decl.kind

and expand t (decl : Decl.t) ~args ~expansions body =
  let knot = Type.Var.create (Fresh.id t.fresh) Generic in
  let expansion =
    { Expansion.args; knot; expanded = false; reached_back = false }
  in
  Hashtbl.add_multi expansions ~key:decl.name ~data:expansion;
  let%bind.Result body, syntax =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:args))
      ~splices:true
      ~expansions
      body
  in
  (* Nominal types are taken to guard here, as their layouts may not be known
     yet: the layout of the declaration is checked after elaboration. *)
  let reaches_unguarded ?through_structure () =
    Value_layout.reaches_unguarded
      ?through_structure
      ~con:(fun (_ : string) -> Some Int.Set.empty)
      ~needs_structure:true
      ~var:knot.id
      [ ATy body ]
  in
  if expansion.reached_back && reaches_unguarded ()
  then
    Error
      (Typecheck_error.create
         ~loc:decl.decl.name.loc
         (* Through a type constructor, a [ptr] would break the cycle. *)
         (if reaches_unguarded ~through_structure:true ()
          then Infinite_type_layout decl.name
          else Cyclic_type_alias decl.name))
  else (
    Type.Var.set_state knot (Link body);
    expansion.expanded <- true;
    Ok (Type.TVar knot, syntax))
;;

let elaborate t module_ vars expr ~splices =
  elaborate'
    t
    module_
    (Annotation vars)
    ~splices
    ~expansions:(String.Table.create ())
    expr
;;

let elaborate_declared t module_ expr =
  elaborate'
    t
    module_
    (Params String.Map.empty)
    ~splices:true
    ~expansions:(String.Table.create ())
    expr
  |> Result.map ~f:fst
;;

let declare_members t module_ (decl : Type_decl.t) ~name =
  let declares names table =
    List.iter names ~f:(fun ((member : Name.t), _) ->
      let key = Module_scope.canonical_name module_ member.txt in
      Hashtbl.add_exn table ~key ~data:name;
      Hashtbl.set t.declared_members ~key ~data:member.loc)
  in
  let rec declare_members : Type_decl.Kind.t -> unit = function
    | Alias _ | Abstract | Splice _ -> ()
    | Malformed kind -> declare_members kind
    | Variant constructors -> declares constructors t.constructor_types
    | Record fields -> declares fields t.field_types
  in
  declare_members decl.kind
;;

let declare
  ?(lazily = false)
  ?(wrap_error = Fn.id)
  t
  module_
  (decl : Type_decl.t)
  =
  let name = Module_scope.canonical_name module_ decl.name.txt in
  let params =
    lazy
      (List.map decl.params ~f:(fun param ->
         match param.txt with
         | Ty _ -> Type.ATy (TVar (Type.Var.create (Fresh.id t.fresh) Generic))
         | Row _ -> ARow (RVar (Type.Var.create (Fresh.id t.fresh) Generic))
         | Mode _ -> AMode (Var (Mode.Var.create_generic (Fresh.modes t.fresh)))))
  in
  if not lazily then ignore (force params : Type.arg list);
  Hashtbl.set t.declared_types ~key:name ~data:decl.name.loc;
  record_type_reference t name ~loc:decl.name.loc;
  Hashtbl.add_exn
    t.types
    ~key:name
    ~data:
      { Decl.decl
      ; name
      ; module_ =
          (if decl.nonrec_
           then Module_scope.without module_ Type decl.name.txt
           else module_)
      ; params
      ; wrap_error
      ; used_while_defined = false
      ; pending_layouts = None
      ; prepared = None
      ; value_parameters = None
      ; variances = None
      ; checked = None
      ; failed =
          (match decl.kind with
           | Malformed _ -> true
           | Variant _ | Record _ | Alias _ | Abstract | Splice _ -> false)
      ; reflected = None
      };
  declare_members t module_ decl ~name
;;

(* Reported, as the type's definition, once its splice is evaluated. *)
let set_definition t name (kind : Type_decl.Kind.t) ~wrap_error ~loc =
  let decl = Hashtbl.find_exn t.types name in
  let error kind =
    decl.failed <- true;
    t.report (decl.wrap_error (Typecheck_error.create ~loc kind))
  in
  match kind with
  | Alias _ when decl.used_while_defined ->
    error (Type_definition_cycle { name; splice = loc })
  | _ when kind_has_splice kind -> error Nested_type_splice
  | Alias _ | Variant _ | Record _ | Abstract | Malformed _ | Splice _ ->
    decl.decl <- { decl.decl with kind };
    decl.wrap_error <- Fn.compose decl.wrap_error wrap_error;
    declare_members t decl.module_ decl.decl ~name
;;

let declare_exception
  ?(wrap_error = Fn.id)
  t
  module_
  (name : Name.t)
  args
  ~malformed
  =
  let key = Module_scope.canonical_name module_ name.txt in
  Hashtbl.set t.declared_members ~key ~data:name.loc;
  Hashtbl.add_exn
    t.exceptions
    ~key
    ~data:
      { Exception_decl.args; malformed; module_; wrap_error; checked = None }
;;

(* Its arguments have no type variables, which nothing would bind. *)
let check_exception t name =
  let decl = Hashtbl.find_exn t.exceptions name in
  match decl.checked with
  | Some checked -> Ok checked
  | None when decl.malformed -> Error Typecheck_error.already_reported
  | None ->
    Result.map_error ~f:decl.wrap_error
    @@
    let%map.Result args =
      map_result decl.args ~f:(fun arg ->
        elaborate'
          t
          decl.module_
          (Params String.Map.empty)
          ~splices:true
          ~expansions:(String.Table.create ())
          arg
        |> Result.map ~f:fst)
    in
    let { Typed.Constructor_ref.type_name; index; name = _ } =
      Typed.Constructor_ref.exception_ name
    in
    let checked =
      { Constructor.name
      ; type_name
      ; index
      ; args
      ; result = Type.exn
      ; modalities = List.map args ~f:(fun _ -> Member_modality.Id)
      }
    in
    decl.checked <- Some checked;
    checked
;;

let prepare_kind t (decl : Decl.t) =
  let expansions = String.Table.create () in
  let elaborate =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:(Decl.params decl)))
      ~splices:true
      ~expansions
  in
  (* A member's type, with its modality. *)
  let member (ty : Type_expr.t) =
    match ty.txt with
    | Modality (inner, mode) ->
      let%bind.Result modality =
        match mode.txt with
        | Atoms atoms
          when List.exists atoms ~f:(fun (atom : Mode_expr.Atom.t) ->
                 match atom with
                 | Regional | Local -> true
                 | Global
                 | Const
                 | Portable
                 | Nonportable
                 | Static
                 | Gc
                 | External -> false) ->
          Error
            (Typecheck_error.create ~loc:mode.loc Relative_locality_modality)
        | Atoms atoms ->
          Mode_atoms.declared ~default:Mode.Const.max atoms ~loc:mode.loc
          |> Result.map ~f:Member_modality.of_atoms
        | Var_at _ ->
          Error
            (Typecheck_error.create
               ~loc:mode.loc
               (Invalid_mode
                  "a modality is a mode parameter or modes, not a locality \
                   with a mode parameter"))
        | Var name ->
          (match
             List.findi decl.decl.params ~f:(fun _ param ->
               String.equal (Type_decl.Param.name param.txt) name)
           with
           | Some (index, { txt = Mode _; _ }) ->
             Ok (Member_modality.Param index)
           | Some (_, { txt = Ty _ | Row _; _ }) ->
             Error
               (Typecheck_error.create
                  ~loc:mode.loc
                  (Type_variable_kind_mismatch name))
           | None ->
             Error
               (Typecheck_error.create
                  ~loc:mode.loc
                  (Mode_variable_in_declaration name)))
      in
      let%map.Result elaborated, syntax = elaborate inner in
      elaborated, { ty with txt = Type_expr.Modality (syntax, mode) }, modality
    | _ ->
      let%map.Result elaborated, syntax = elaborate ty in
      elaborated, syntax, Member_modality.Id
  in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  match decl.decl.kind with
  | Alias body ->
    let%map.Result body, syntax =
      expand t decl ~args:(Decl.params decl) ~expansions body
    in
    Signature.Type_decl.Kind.Alias body, Some (Reflected_decl.Kind.Alias syntax)
  | Abstract -> Ok (Signature.Type_decl.Kind.Abstract, None)
  | Splice _ ->
    raise_s [%message "Type_env.define: definition not computed" decl.name]
  | Malformed _ ->
    raise_s [%message "Type_env.define: malformed" (decl.name : string)]
  | Variant constructors ->
    let%map.Result constructors =
      map_result constructors ~f:(fun ((name : Name.t), args) ->
        let%map.Result args = map_result args ~f:member in
        let args, syntax, modalities = List.unzip3 args in
        ( { Signature.Constructor.name = name.txt; args; modalities }
        , (canonical_name name.txt, syntax) ))
    in
    let constructors, reflected = List.unzip constructors in
    ( Signature.Type_decl.Kind.Variant constructors
    , Some (Reflected_decl.Kind.Variant reflected) )
  | Record fields ->
    let%map.Result fields =
      map_result fields ~f:(fun ((name : Name.t), ty) ->
        let%map.Result ty, syntax, modality = member ty in
        ( { Signature.Field.name = name.txt; ty; modality }
        , (canonical_name name.txt, syntax) ))
    in
    let fields, reflected = List.unzip fields in
    ( Signature.Type_decl.Kind.Record fields
    , Some (Reflected_decl.Kind.Record reflected) )
;;

(* What is cached of a declaration is computed by [Undo.permanent]: it is the
   declaration's, whatever attempt first needed it. *)
let prepare t (decl : Decl.t) =
  match decl.prepared with
  | Some prepared -> Ok prepared
  | None ->
    Undo.permanent (fun () ->
      let%map.Result prepared = prepare_kind t decl in
      decl.prepared <- Some prepared;
      prepared)
;;

let value_parameters t (root : Decl.t) =
  Undo.permanent
  @@ fun () ->
  let visiting = String.Hash_set.create () in
  (* A generated definition that is being evaluated is still opaque. Do not
     cache any summaries that could depend (even indirectly) on its layout. *)
  let complete = ref true in
  let rec visit (decl : Decl.t) =
    let infinite () =
      Error
        (Typecheck_error.create
           ~loc:decl.decl.name.loc
           (Infinite_type_layout decl.name))
    in
    match decl.value_parameters with
    | Some params -> Ok (Some params)
    | None when Hash_set.mem visiting decl.name -> infinite ()
    | None ->
      let%bind.Result kind = definition decl ~loc:decl.decl.name.loc in
      (match kind with
       | Splice _ ->
         complete := false;
         Ok None
       | Abstract | Malformed _ -> Ok None
       | Record _ | Variant _ | Alias _ ->
         Hash_set.add visiting decl.name;
         let%bind.Result kind, _ = prepare t decl in
         let roots : Type.arg list =
           match kind with
           | Alias ty -> [ ATy ty ]
           | Record fields ->
             List.map fields ~f:(fun (field : Signature.Field.t) ->
               Type.ATy field.ty)
           | Variant constructors ->
             List.concat_map
               constructors
               ~f:(fun (ctor : Signature.Constructor.t) ->
                 List.map ctor.args ~f:(fun ty -> Type.ATy ty))
           | Abstract -> assert false
         in
         let params =
           List.filter_mapi (Decl.params decl) ~f:(fun index -> function
             | ATy (TVar v) -> Some (v.id, index)
             | ARow (RVar v) -> Some (v.id, index)
             | AMode _ -> None
             | _ -> assert false)
           |> Int.Map.of_alist_exn
         in
         let stored = ref Int.Set.empty in
         let%map.Result () =
           Value_layout.iter
             ~con:(fun name ->
               match Hashtbl.find t.types name with
               | None -> Ok None
               | Some target when target.failed -> Ok None
               | Some target -> visit target)
             ~var:(fun id ->
               Option.iter (Map.find params id) ~f:(fun index ->
                 stored := Set.add !stored index);
               Ok ())
             ~cycle:infinite
             roots
         in
         Hash_set.remove visiting decl.name;
         if !complete then decl.value_parameters <- Some !stored;
         Some !stored)
  in
  visit root
;;

(* A recursive group is solved by iterating from [Bivariant] until nothing
   changes. Only the outermost declaration's result is cached, as the others'
   depend on assumptions about it. A definition its splice is still computing
   isn't known: what depends on it takes it to be invariant, and isn't cached. *)
let variances t =
  let assumed = String.Table.create () in
  let complete = ref true in
  let rec lookup name =
    match name with
    | name when String.equal name Type.ptr_name ->
      Some [ Variance.Invariant; Invariant ]
    | name when String.equal name Type.box_name -> Some [ Variance.Covariant ]
    | _ -> Option.bind (Hashtbl.find t.types name) ~f:of_decl
  and of_decl (decl : Decl.t) =
    match decl.variances with
    | Some variances -> Some variances
    | None ->
      (match Hashtbl.find assumed decl.name with
       | Some variances -> Some variances
       | None ->
         Undo.permanent
         @@ fun () ->
         (match
            match decl.decl.kind with
            | Splice _ -> Error `Splice
            | _ -> Result.map_error (prepare t decl) ~f:(fun _ -> `Failed)
          with
          | Error `Splice ->
            complete := false;
            None
          | Error `Failed ->
            Some (List.map (Decl.params decl) ~f:(fun _ -> Variance.Invariant))
          | Ok (kind, _) ->
            let outermost = Hashtbl.is_empty assumed in
            if outermost then complete := true;
            let rec solve assumption =
              Hashtbl.set assumed ~key:decl.name ~data:assumption;
              let variances =
                Param_variance.compute
                  ~con:lookup
                  ~params:(Decl.params decl)
                  kind
              in
              if [%equal: Variance.t list] variances assumption
              then variances
              else solve variances
            in
            let variances =
              solve
                (List.map (Decl.params decl) ~f:(fun _ -> Variance.Bivariant))
            in
            Hashtbl.remove assumed decl.name;
            if outermost && !complete then decl.variances <- Some variances;
            Some variances))
  in
  lookup
;;

let define t (decl : Decl.t) =
  let%bind.Result kind, reflected = prepare t decl in
  let%map.Result (_ : Int.Set.t option) = value_parameters t decl in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  decl.reflected
  <- Option.map reflected ~f:(fun kind ->
       { Reflected_decl.params = decl.decl.params; kind });
  let checked =
    { Signature.Type_decl.name = decl.decl.name.txt
    ; params = Decl.params decl
    ; kind
    }
  in
  decl.checked <- Some checked;
  (match kind with
   | Alias _ | Abstract -> ()
   | Variant constructors ->
     let result = Type.TCon (decl.name, Decl.params decl) in
     List.iteri constructors ~f:(fun index { name; args; modalities } ->
       let name = canonical_name name in
       Hashtbl.add_exn
         t.constructors
         ~key:name
         ~data:
           { Constructor.name
           ; type_name = decl.name
           ; index
           ; args
           ; result
           ; modalities
           })
   | Record fields ->
     let record_type =
       { Typed.Record_type.name = decl.name
       ; fields =
           Array.of_list_map fields ~f:(fun (field : Signature.Field.t) ->
             canonical_name field.name)
       }
     in
     List.iteri fields ~f:(fun index { name = _; ty; modality } ->
       Hashtbl.add_exn
         t.fields
         ~key:record_type.fields.(index)
         ~data:
           { Field.record_type
           ; index
           ; record = TCon (decl.name, Decl.params decl)
           ; ty
           ; modality
           }));
  checked
;;

let layouts t = Set_once.get_exn t.layouts

(* Each part of a type the typechecker looks through. *)
let work = Work.create "typecheck.types"

(* Whether [root] is pointer-free, taking the layouts not known yet, which it
   collects, to be: [`Nominal] is a declared type of arguments known to be
   pointer-free or not. *)
let pointer_free' t ~check root =
  let collected = ref [] in
  let unknown layout =
    collected := layout :: !collected;
    true
  in
  (* The variables whose layouts are collected, and whether a declaration whose
     splice is being computed was reached, since the innermost [Link] being
     found. *)
  let reached = ref [] in
  let reached_pending = ref false in
  let unknown_var var =
    reached := var :: !reached;
    unknown (Var_layout.of_ty (layouts t) var)
  in
  (* What [f] collects, set apart. *)
  let collecting f =
    let outer = !collected in
    collected := [];
    let result = f () in
    let mine =
      List.dedup_and_sort !collected ~compare:(fun a b ->
        Int.compare (Mode.Layout.id a) (Mode.Layout.id b))
    in
    collected := outer;
    result, mine
  in
  let visiting = String.Hash_set.create () in
  let visiting_ids = Int.Hash_set.create () in
  (* Shared aliases and nodes are visited once: a DAG of them is linear. *)
  let nominals = Hashtbl.Poly.create () in
  let nodes = Int.Table.create () in
  let links = Int.Table.create () in
  let visit id ~f =
    (not (Hash_set.mem visiting_ids id))
    &&
    (Hash_set.add visiting_ids id;
     let result = f () in
     Hash_set.remove visiting_ids id;
     result)
  in
  let node ?(table = nodes) params id ~f =
    if not (Map.is_empty params)
    then visit id ~f
    else (
      match Hashtbl.find table id with
      | Some (result, mine) ->
        collected := mine @ !collected;
        result
      | None ->
        let ((result, mine) as data) = collecting (fun () -> visit id ~f) in
        Hashtbl.set table ~key:id ~data;
        collected := mine @ !collected;
        result)
  in
  let rec ty_ params (ty : Type.ty) =
    Work.incr work;
    match ty with
    | TVar { id; state = Generic; _ } when Map.mem params id ->
      Map.find_exn params id
    | TVar ({ id; state = Link target; _ } as var) ->
      let compute () =
        node ~table:links params id ~f:(fun () -> ty_ params target)
      in
      (match Type.frontier var with
       | Some frontier when Map.is_empty params ->
         link var ~target ~frontier ~compute
       | Some _ | None -> compute ())
    | _ ->
      (match Type.canonical_ty ty with
       | TVar ({ state = Unbound _; _ } as var) -> unknown_var var
       | TVar _ | TArrow _ | TCode _ -> false
       | TCon (("int" | "bool" | "char" | "unit"), []) -> true
       | TCon (name, _) when Type.is_pointer_name name -> false
       | TCon (name, args) ->
         (match Hashtbl.find t.types name with
          | Some ({ decl = { kind = Splice _; _ }; checked = None; _ } as decl)
            -> pending params decl args
          | _ -> nominal name (List.map args ~f:(arg params)))
       | TTuple tys -> List.for_all tys ~f:(ty_ params)
       | TRecord { id; row } | TVariant { id; row } ->
         node params id ~f:(fun () -> row_ params row))
  and row_ params (row : Type.row) =
    match row with
    | RVar { id; state = Generic; _ } when Map.mem params id ->
      Map.find_exn params id
    | RVar { id; state = Link target; _ } ->
      node ~table:links params id ~f:(fun () -> row_ params target)
    | _ ->
      (match Type.canonical_row row with
       | REmpty -> true
       | RVar ({ state = Unbound _; _ } as var) ->
         unknown (Var_layout.of_row (layouts t) var)
       | RVar _ -> false
       | RExtend (_, presence, _, entry, rest) ->
         (match Type.canonical_presence presence with
          | Absent -> true
          | Present -> ty_ params entry
          | PVar var ->
            let presence () = Var_layout.of_presence (layouts t) var in
            (match collecting (fun () -> ty_ params entry) with
             | false, _ -> unknown (presence ())
             | true, [] -> true
             | true, entry ->
               (* Has pointers once present with an entry that has. *)
               unknown (Var_layout.both (layouts t) (presence ()) entry)))
         && row_ params rest)
  and arg params : Type.arg -> bool = function
    | ATy entry -> ty_ params entry
    | ARow entry -> row_ params entry
    | AMode _ -> true
  (* Its definition may reach one a splice is computing. *)
  and nominal name args =
    match Hashtbl.find nominals (name, args) with
    | Some (result, mine) ->
      collected := mine @ !collected;
      result
    | None ->
      let ((result, mine) as data) =
        collecting (fun () -> nominal' name args)
      in
      if not (Hash_set.mem visiting name)
      then Hashtbl.set nominals ~key:(name, args) ~data;
      collected := mine @ !collected;
      result
  and link var ~target ~frontier ~compute =
    match Link_layout.find t.link_layouts var ~target ~frontier with
    | Some { pointer_free; depends_on; _ } ->
      List.iter depends_on ~f:(fun var -> ignore (unknown_var var : bool));
      pointer_free
    | None ->
      let outer = !reached
      and outer_pending = !reached_pending in
      reached := [];
      reached_pending := false;
      let pointer_free, mine = collecting compute in
      let depends_on = !reached
      and pending = !reached_pending in
      reached := depends_on @ outer;
      reached_pending := pending || outer_pending;
      collected := mine @ !collected;
      let ids layouts = List.map layouts ~f:Mode.Layout.id |> Int.Set.of_list in
      (* Only what is found again from the variables is kept. *)
      if (not pending)
         && Mode.Private.Undo.cacheable ()
         && Set.equal
              (ids mine)
              (ids (List.map depends_on ~f:(Var_layout.of_ty (layouts t))))
      then
        Link_layout.set
          t.link_layouts
          var
          { target
          ; frontier
          ; generic = Link_layout.generic frontier
          ; pointer_free
          ; depends_on =
              List.dedup_and_sort depends_on ~compare:(fun a b ->
                Int.compare a.Type.Var.id b.id)
          };
      pointer_free
  (* Has pointers once its definition does, or an argument that has pointers
     where the definition gives it pointers. *)
  and pending params (decl : Decl.t) args =
    reached_pending := true;
    let own =
      match decl.pending_layouts with
      | Some layouts -> layouts
      | None ->
        let fresh () = Var_layout.above (layouts t) [] in
        let layouts =
          { Pending_layouts.whole = fresh ()
          ; args =
              List.map (Decl.params decl) ~f:(function
                | ATy _ | ARow _ -> Some (fresh ())
                | AMode _ -> None)
          }
        in
        decl.pending_layouts <- Some layouts;
        layouts
    in
    unknown own.whole
    && List.for_alli args ~f:(fun index arg' ->
      match arg' with
      | Type.AMode _ -> true
      | ATy _ | ARow _ ->
        let layout = Option.value_exn (List.nth_exn own.args index) in
        (match collecting (fun () -> arg params arg') with
         | false, _ -> unknown layout
         | true, [] -> true
         | true, lowers -> unknown (Var_layout.both (layouts t) layout lowers)))
  and nominal' name args =
    (not (Hash_set.mem visiting name))
    &&
    match
      (* A type computed by a splice isn't forced: that may need what is being
         checked. *)
      match Hashtbl.find t.types name with
      | None -> Error ()
      | Some { checked = Some checked; _ } -> Ok checked
      | Some { decl = { kind = Splice _; _ }; _ } -> Error ()
      | Some _ -> Result.map_error (check name) ~f:ignore
    with
    | Error () -> false
    | Ok decl ->
      Hash_set.add visiting name;
      let params =
        List.zip_exn decl.params args
        |> List.filter_map ~f:(fun (param, pointer_free) ->
          match param with
          | ATy (TVar v) -> Some (v.id, pointer_free)
          | ARow (RVar v) -> Some (v.id, pointer_free)
          | _ -> None)
        |> Int.Map.of_alist_exn
      in
      let result =
        match decl.kind with
        | Abstract -> false
        | Alias body -> ty_ params body
        | Record fields ->
          List.for_all fields ~f:(fun (field : Signature.Field.t) ->
            ty_ params field.ty)
        | Variant constructors ->
          List.for_all constructors ~f:(fun (ctor : Signature.Constructor.t) ->
            List.for_all ctor.args ~f:(ty_ params))
      in
      Hash_set.remove visiting name;
      result
  in
  collecting (fun () ->
    match root with
    | `Ty ty -> ty_ Int.Map.empty ty
    | `Nominal (name, args) -> nominal name args)
;;

let layout_of' t ~check root : Var_layout.Layout_of.t =
  match pointer_free' t ~check root with
  | false, _ -> Has_pointers
  | true, unknown ->
    (match Mode.Layout.roots unknown with
     | [] -> Pointer_free
     | layouts -> Depends layouts)
;;

(* Once its splice has computed its definition, what was taken of it while it
   was being computed. *)
let decide_pending t (decl : Decl.t) ~check =
  Option.iter decl.pending_layouts ~f:(fun { whole; args } ->
    (* What depends on them may be in an attempt, which mustn't keep them. *)
    Undo.when_settled
    @@ fun () ->
    decl.pending_layouts <- None;
    let reason why =
      Mode.Reason.layout
        ~loc:decl.decl.name.loc
        (sprintf "its type turned out to be %s, %s" decl.decl.name.txt why)
    in
    let decide ~why layout pointer_free =
      match layout_of' t ~check (`Nominal (decl.name, pointer_free)) with
      | Pointer_free ->
        Mode.Layout.pointer_free layout;
        Ok ()
      | Has_pointers -> Mode.Layout.raise layout ~because:[ reason why ]
      | Depends lowers ->
        List.fold_result lowers ~init:() ~f:(fun () lower ->
          Mode.Layout.add_below ~lower layout)
    in
    let all = List.map args ~f:(fun _ -> true) in
    decide ~why:"whose definition has pointers" whole all
    :: List.filter_mapi args ~f:(fun index layout ->
      Option.map layout ~f:(fun layout ->
        decide
          ~why:"which has pointers when an argument that has them is"
          layout
          (List.mapi all ~f:(fun index' _ -> index <> index'))))
    |> List.iter ~f:(function
      | Ok () -> ()
      | Error error ->
        t.report
          (Typecheck_error.create ~loc:decl.decl.name.loc (Mode_error error))))
;;

let rec check t name =
  let decl = Hashtbl.find_exn t.types name in
  match decl.checked with
  | Some checked -> Ok checked
  | None when decl.failed -> Error Typecheck_error.already_reported
  | None ->
    Undo.permanent
    @@ fun () ->
    (match decl.decl.kind with
     | Splice _ ->
       Module_scope.force_definition
         decl.module_
         decl.decl.name.txt
         ~loc:decl.decl.name.loc
       |> Result.map_error ~f:decl.wrap_error
     | Variant _ | Record _ | Alias _ | Abstract | Malformed _ -> Ok ())
    |> Result.bind ~f:(fun () ->
      if decl.failed then Error Typecheck_error.already_reported else Ok ())
    |> Result.bind ~f:(fun () -> define t decl)
    |> Result.map ~f:(fun checked ->
      decide_pending t decl ~check:(check t);
      checked)
    |> Result.map_error ~f:(fun error ->
      decl.failed <- true;
      (* Reported here, as the code that first used the declaration may be
         abandoned, and later uses are [Already_reported]. *)
      if not (Typecheck_error.is_already_reported error)
      then t.report (decl.wrap_error error);
      Typecheck_error.already_reported)
;;

let members t type_name ~kind ~members =
  match Hashtbl.find t.types type_name with
  | None -> Ok None
  | Some decl ->
    let%map.Result checked = check t type_name in
    kind checked.kind
    |> Option.map ~f:(fun names ->
      List.map names ~f:(fun name ->
        Hashtbl.find_exn members (Module_scope.canonical_name decl.module_ name)))
;;

let record_fields t type_name =
  members t type_name ~members:t.fields ~kind:(function
    | Record fields ->
      Some (List.map fields ~f:(fun (field : Signature.Field.t) -> field.name))
    | Variant _ | Alias _ | Abstract -> None)
;;

let record_type t type_name =
  let%map.Result fields = record_fields t type_name in
  match fields with
  | Some (field :: _) -> field.record_type
  | Some [] | None ->
    raise_s [%message "Not a record type" (type_name : string)]
;;

let variant_constructors t type_name =
  members t type_name ~members:t.constructors ~kind:(function
    | Variant constructors ->
      Some
        (List.map
           constructors
           ~f:(fun (constructor : Signature.Constructor.t) -> constructor.name))
    | Record _ | Alias _ | Abstract -> None)
;;

let is_empty_variant t name =
  let%map.Result checked = check t name in
  match checked.kind with
  | Variant [] -> true
  | Variant (_ :: _) | Record _ | Alias _ | Abstract -> false
;;

(* The entry of the constructor or field of canonical name [name], checking its
   type. *)
let find_member t ~types ~members name ~loc ~unbound =
  match Hashtbl.find types name with
  | None ->
    Error
      (Typecheck_error.create
         ~loc
         (unbound (Decl_path.Path (Path.of_string name))))
  | Some type_name ->
    let%map.Result (_ : Signature.Type_decl.t) = check t type_name in
    Hashtbl.find_exn members name
;;

let constructor t name ~loc =
  if Hashtbl.mem t.exceptions name
  then check_exception t name
  else
    find_member
      t
      ~types:t.constructor_types
      ~members:t.constructors
      name
      ~loc
      ~unbound:(fun path -> Unbound_constructor path)
;;

let field t name ~loc =
  find_member
    t
    ~types:t.field_types
    ~members:t.fields
    name
    ~loc
    ~unbound:(fun path -> Unbound_field path)
;;

let find_constructor t module_ ({ txt = path; loc } : Decl_path.t Located.t) =
  match path with
  | Resolved name -> constructor t name ~loc
  | Path _ | Root _ ->
    let%map.Result constructor =
      find_resolved
        t
        module_
        Constructor
        path
        ~loc
        ~unbound:(fun path -> Unbound_constructor path)
        ~find:constructor
    in
    record_member_reference t constructor.name ~loc;
    constructor
;;

let find_field t module_ ({ txt = path; loc } : Decl_path.t Located.t) =
  match path with
  | Resolved name -> field t name ~loc
  | Path _ | Root _ ->
    let%map.Result field =
      find_resolved
        t
        module_
        Field
        path
        ~loc
        ~unbound:(fun path -> Unbound_field path)
        ~find:field
    in
    record_member_reference t (Field.name field) ~loc;
    field
;;

let reflect t name =
  let%map.Result (_ : Signature.Type_decl.t) = check t name in
  (Hashtbl.find_exn t.types name).reflected
;;

let modality (modality : Member_modality.t) ~(of_ : Type.ty) : Modality.t =
  match modality with
  | Id -> Modality.id
  | Meet const -> Modality.of_step (Meet const)
  | Param index ->
    (match Type.canonical_ty of_ with
     | TCon (_, args) ->
       (match List.nth_exn args index with
        | AMode mode -> Modality.of_step (Param mode)
        | ATy _ | ARow _ ->
          raise_s
            [%message "Type_env.modality: not a mode argument" (index : int)])
     | _ -> raise_s [%message "Type_env.modality: not of a type constructor"])
;;

let field_modality t name ~record =
  modality (Hashtbl.find_exn t.fields name).modality ~of_:record
;;

let layout_of t ty : Var_layout.Layout_of.t =
  layout_of' t ~check:(check t) (`Ty ty)
;;

let create ?(record_references = false) fresh ~report ~evaluate_splice =
  let t = create_tables fresh ~record_references ~report ~evaluate_splice in
  Fresh.set_cons
    fresh
    { value_parameters =
        (fun name ->
          Option.bind (Hashtbl.find t.types name) ~f:(fun decl ->
            match value_parameters t decl with
            | Ok params -> params
            | Error _ -> None))
    ; variances = variances t
    };
  Set_once.set_exn t.layouts (Var_layout.create ~layout_of:(layout_of t));
  t
;;
