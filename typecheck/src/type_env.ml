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

module Decl = struct
  type t =
    { mutable decl : Type_decl.t (* Its definition once computed, if spliced. *)
    ; name : string
    ; module_ : Module_scope.t
    ; params : Type.arg list Lazy.t
    ; mutable wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable used_while_defined : bool
    ; mutable prepared :
        (Signature.Type_decl.Kind.t * Reflected_decl.Kind.t option) option
    ; mutable value_parameters : Int.Set.t option
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

type t =
  { fresh : Fresh.t
  ; mutable evaluate_splice :
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
  ; report : Typecheck_error.t -> unit
  }

let create_tables fresh ~report =
  { fresh
  ; evaluate_splice =
      (fun _ _ ~loc:_ ->
        raise_s [%message "Type_env: no type splice evaluator"])
  ; type_splices = Loc.Table.create ()
  ; report
  ; types = String.Table.create ()
  ; exceptions = String.Table.create ()
  ; constructors = String.Table.create ()
  ; fields = String.Table.create ()
  ; constructor_types = String.Table.create ()
  ; field_types = String.Table.create ()
  }
;;

let builtins =
  List.map
    [ "int"; "bool"; "string"; "char"; "unit"; Type.exn_name; Type.error_name ]
    ~f:(fun name ->
      { Type_decl.name = Located.create name; params = []; kind = Abstract })
  @ [ { Type_decl.name = Located.create "ptr"
      ; params =
          [ Located.create (Type_decl.Param.Ty "a")
          ; Located.create (Type_decl.Param.Mode "p")
          ]
      ; kind = Abstract
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

let set_evaluate_splice t f = t.evaluate_splice <- f

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

let find_type t module_ path ~loc =
  let%bind.Result name = resolve_type module_ path ~loc in
  match Hashtbl.find t.types name with
  | Some decl -> Ok decl
  | None ->
    Error
      (Typecheck_error.create ~loc (Unbound_type (Path (Path.of_string name))))
;;

module Annotation_vars = struct
  type t =
    { fresh : Fresh.t
    ; rank : Type.rank
    ; tys : Type.ty String.Table.t
    ; rows : Type.row String.Table.t
    ; modes : Mode.Var.t String.Table.t
    ; located : String.Hash_set.t
    ; mode_args : String.Hash_set.t
    }

  let create fresh ~rank =
    { fresh
    ; rank
    ; tys = String.Table.create ()
    ; rows = String.Table.create ()
    ; modes = String.Table.create ()
    ; located = String.Hash_set.create ()
    ; mode_args = String.Hash_set.create ()
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

  (* [`Mode_arg] for a mode argument, which has no locality, [`Located] where it
     has, and [`Related] in a constraint, which relates its axes. *)
  let var t name ~loc ~(use : [ `Located | `Mode_arg | `Related ]) =
    (match use with
     | `Located -> Hash_set.add t.located name
     | `Mode_arg -> Hash_set.add t.mode_args name
     | `Related -> ());
    only_kind
      name
      ~loc
      ~in_:t.modes
      ~others:[ Hashtbl.mem t.tys; Hashtbl.mem t.rows ]
      ~default:(fun () -> fresh_mode t)
  ;;

  (* A variable only of mode arguments has no locality. *)
  let mode t name ~loc = var t name ~loc ~use:`Located

  let has_locality t name =
    Hash_set.mem t.located name || not (Hash_set.mem t.mode_args name)
  ;;
end

module Mode_atoms = struct
  (* The constant of a declaration's arrow: the axes not written are
     [default]'s, but a [const] one is [static]. *)
  let declared ~(default : Mode.Const.t) (atoms : Syntax.Mode_expr.Atom.t list) =
    let const : Mode.Const.t =
      List.fold atoms ~init:default ~f:(fun const atom ->
        match atom with
        | Const -> { const with stage = Const; provenance = Static }
        | Portable -> { const with stage = Portable }
        | Nonportable -> { const with stage = Nonportable }
        | Global -> { const with locality = Global }
        | Regional -> { const with locality = Regional }
        | Local -> { const with locality = Local }
        | Static -> { const with provenance = Static }
        | Gc -> { const with provenance = Gc }
        | External -> { const with provenance = External })
    in
    const
  ;;

  (* Fixes the axes written. *)
  let constrain var (atoms : Syntax.Mode_expr.Atom.t list) ~loc =
    let reason =
      Mode.Reason.create
        ~loc
        (sprintf
           "it is annotated %s"
           (List.map atoms ~f:Syntax.Mode_expr.Atom.to_string
            |> String.concat ~sep:" "))
    in
    let fix axis point =
      let%bind.Result () = Mode.Var.at_least var axis point reason in
      Mode.Var.at_most var axis point reason
    in
    List.fold_result
      atoms
      ~init:()
      ~f:(fun () (atom : Syntax.Mode_expr.Atom.t) ->
        match atom with
        | Const -> fix Mode.Axis.Stage Mode.Stage.Const
        | Portable -> fix Stage Portable
        | Nonportable -> fix Stage Nonportable
        | Global -> fix Locality Global
        | Regional -> fix Locality Regional
        | Local -> fix Locality Local
        | Static -> fix Provenance Static
        | Gc -> fix Provenance Gc
        | External -> fix Provenance External)
    |> Result.map_error ~f:(fun (_ : Mode.Error.t) ->
      Typecheck_error.create ~loc (Invalid_mode "its modes are inconsistent"))
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

  (* An arrow side written without a mode: in an annotation, any, so as not to
     restrict the modes of what it annotates; in a declaration, the default. *)
  let unannotated_side t ~default : Arrow_modes.Side.t =
    match t with
    | Annotation vars -> Var (Annotation_vars.fresh_mode vars)
    | Params _ -> Fixed default
  ;;

  let unannotated_curry : t -> Arrow_modes.Side.t = function
    | Annotation vars -> Var (Annotation_vars.fresh_mode vars)
    | Params _ -> Arrow_modes.default_curry
  ;;

  let mode_side t ({ txt; loc } : Syntax.Mode_expr.t) ~default
    : (Arrow_modes.Side.t, Typecheck_error.t) Result.t
    =
    match t, txt with
    | Annotation vars, Var name ->
      Result.map (Annotation_vars.mode vars name ~loc) ~f:(fun var ->
        Arrow_modes.Side.Var var)
    | Params _, Var name -> error ~loc (Mode_variable_in_declaration name)
    | Annotation vars, Atoms atoms ->
      let var = Annotation_vars.fresh_mode vars in
      let%map.Result () = Mode_atoms.constrain var atoms ~loc in
      Arrow_modes.Side.Var var
    | Params _, Atoms atoms ->
      Ok (Arrow_modes.Side.Fixed (Mode_atoms.declared atoms ~default))
  ;;

  (* A mode argument: of a type's mode parameter, or what the cell of a
     [ptr[t @@ mode]] may hold, which are on stage and provenance only. *)
  let mode_arg t ({ txt; loc } : Syntax.Mode_expr.t)
    : (Mode_arg.t, Typecheck_error.t) Result.t
    =
    match t, txt with
    | Annotation vars, Var name ->
      Result.map
        (Annotation_vars.var vars name ~loc ~use:`Mode_arg)
        ~f:(fun var -> Mode_arg.Var var)
    | Params params, Var name ->
      (match Map.find params name with
       | Some (AMode mode) -> Ok mode
       | Some (ATy _ | ARow _) -> error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Mode_variable_in_declaration name))
    | _, Atoms atoms ->
      if List.exists atoms ~f:(fun (atom : Syntax.Mode_expr.Atom.t) ->
           match atom with
           | Global | Regional | Local -> true
           | Const | Portable | Nonportable | Static | Gc | External -> false)
      then error ~loc Locality_in_mode_argument
      else Ok (Fixed (Mode_atoms.declared ~default:Mode_arg.default atoms))
  ;;

  let rec term_loc : Syntax.Mode_expr.Term.t -> Loc.t = function
    | Mode mode | Axis (_, mode) -> mode.loc
    | Meet (t, _) | Release t | Static_if_const t | Const_if_static t ->
      term_loc t
  ;;

  let rec term_var : Syntax.Mode_expr.Term.t -> string option = function
    | Mode { txt = Var name; _ } | Axis (_, { txt = Var name; _ }) -> Some name
    | Mode { txt = Atoms _; _ } | Axis (_, { txt = Atoms _; _ }) -> None
    | Meet (t, _) | Release t | Static_if_const t | Const_if_static t ->
      term_var t
  ;;

  let no_locality name ~loc =
    Typecheck_error.create
      ~loc
      (Invalid_mode
         (sprintf "'%s is only a mode argument, which has no locality" name))
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
    let point (atom : Syntax.Mode_expr.Atom.t) =
      match atom with
      | Const -> 0
      | Portable -> 1
      | Nonportable -> 2
      | Static -> 0
      | Gc -> 1
      | External -> 2
      | Global -> 0
      | Regional -> 1
      | Local -> 2
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
      let%bind var = Annotation_vars.var vars name ~loc ~use:`Related in
      (match on with
       | Locality when not (Annotation_vars.has_locality vars name) ->
         Error (no_locality name ~loc)
       | Stage | Provenance | Locality ->
         Ok (var, Mode.Morphism.identity (axis on)))
    | Meet (t, atom) ->
      under
        t
        (Mode.Morphism.meet_with
           (axis (Syntax.Mode_expr.Atom.axis atom))
           (point atom))
        ~left:true
    | Release t -> under t Mode.Morphism.release ~left:true
    | Static_if_const t -> under t Mode.Morphism.const_is_static ~left:false
    | Const_if_static t -> under t Mode.Morphism.runtime_is_gc ~left:false
    | Mode _ | Axis (_, { txt = Atoms _; _ }) ->
      Error
        (Typecheck_error.create
           ~loc
           (Invalid_mode "a constraint on one axis relates two mode variables"))
  ;;

  (* [lower <= upper]: between variables, on every axis or the one named, or
     bounding a variable by the atoms, on their axes. *)
  let mode_constraint
    t
    ({ lower; upper } as constraint_ : Syntax.Mode_expr.Constraint.t)
    ~loc
    : (unit, Typecheck_error.t) Result.t
    =
    let reason =
      Mode.Reason.create
        ~loc
        (sprintf
           "it is annotated %s"
           (Syntax.Mode_expr.Constraint.to_string constraint_))
    in
    let bound (atoms : Syntax.Mode_expr.Atom.t list) ~(f : Bound.t) =
      List.fold_result atoms ~init:() ~f:(fun () atom ->
        match atom with
        | Const -> f.f Stage Mode.Stage.Const
        | Portable -> f.f Stage Portable
        | Nonportable -> f.f Stage Nonportable
        | Static -> f.f Provenance Static
        | Gc -> f.f Provenance Gc
        | External -> f.f Provenance External
        | Global -> f.f Locality Global
        | Regional -> f.f Locality Regional
        | Local -> f.f Locality Local)
    in
    let checked result =
      Result.map_error result ~f:(fun error ->
        Typecheck_error.create ~loc (Mode_error error))
    in
    let locality_atoms name (atoms : Syntax.Mode_expr.Atom.t list) =
      match t with
      | Annotation vars
        when (not (Annotation_vars.has_locality vars name))
             && List.exists atoms ~f:(fun atom ->
               match Syntax.Mode_expr.Atom.axis atom with
               | Locality -> true
               | Stage | Provenance -> false) -> Error (no_locality name ~loc)
      | Annotation _ | Params _ -> Ok ()
    in
    match t, lower, upper with
    | Params _, _, _ ->
      (match Option.first_some (term_var lower) (term_var upper) with
       | Some name -> error ~loc (Mode_variable_in_declaration name)
       | None ->
         error ~loc (Invalid_mode "a constraint must be on a mode variable"))
    | Annotation vars, Mode lower, Mode upper ->
      (match lower.txt, upper.txt with
       | Atoms _, Atoms _ ->
         error ~loc (Invalid_mode "a constraint must be on a mode variable")
       | Var lower_name, Var upper_name ->
         let%bind.Result lower =
           Annotation_vars.var vars lower_name ~loc ~use:`Related
         in
         let%bind.Result upper =
           Annotation_vars.var vars upper_name ~loc ~use:`Related
         in
         if Annotation_vars.has_locality vars lower_name
            && Annotation_vars.has_locality vars upper_name
         then checked (Mode.Var.submode lower upper reason)
         else
           checked
             (let%bind.Result () =
                Mode.Var.submode_on lower upper Stage reason
              in
              Mode.Var.submode_on lower upper Provenance reason)
       | Var lower, Atoms atoms ->
         let%bind.Result () = locality_atoms lower atoms in
         let%bind.Result var =
           Annotation_vars.var vars lower ~loc ~use:`Related
         in
         checked
           (bound
              atoms
              ~f:
                { f = (fun axis point -> Mode.Var.at_most var axis point reason)
                })
       | Atoms atoms, Var upper ->
         let%bind.Result () = locality_atoms upper atoms in
         let%bind.Result var =
           Annotation_vars.var vars upper ~loc ~use:`Related
         in
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
  | Var var ->
    Mode.Var.at_least
      var
      Stage
      Portable
      (Mode.Reason.create ~loc "it is the result of a function")
    |> Result.map_error ~f:(fun (_ : Mode.Error.t) -> never_const)
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
          elaborated, Vars.unannotated_side vars ~default, syntax
      in
      let default = Arrow_modes.default_const in
      let%bind.Result ty1, side1, syntax1 = side ty1 ~default ~elaborate in
      (* A curried declaration's partial applications hold its arguments so far,
         so are as local as they are, as in OxCaml. *)
      let%bind.Result ty2, side2, syntax2 =
        match ty2.txt, side1 with
        | Arrow _, Fixed { locality; _ } ->
          let curried = Mode.Locality.join curried locality in
          side
            ty2
            ~default:{ default with locality = curried }
            ~elaborate:(elaborate' ~curried t module_ vars ~splices ~expansions)
        | _ -> side ty2 ~default ~elaborate
      in
      let%map.Result () = result_floor side2 ~loc:syntax2.loc in
      ( Type.TArrow
          ( ty1
          , ty2
          , { arg = side1; res = side2; curry = Vars.unannotated_curry vars } )
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
              (Loc.merge
                 (Vars.term_loc constraint_.lower)
                 (Vars.term_loc constraint_.upper)))
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
    knot.state <- Link body;
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
      Hashtbl.add_exn
        table
        ~key:(Module_scope.canonical_name module_ member.txt)
        ~data:name)
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
         | Mode _ ->
           let var = Mode.Var.create (Fresh.modes t.fresh) ~rank:1 in
           Mode.Var.generalize [ var ] ~rank:0;
           AMode (Var var)))
  in
  if not lazily then ignore (force params : Type.arg list);
  Hashtbl.add_exn
    t.types
    ~key:name
    ~data:
      { Decl.decl
      ; name
      ; module_
      ; params
      ; wrap_error
      ; used_while_defined = false
      ; prepared = None
      ; value_parameters = None
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
  Hashtbl.add_exn
    t.exceptions
    ~key:(Module_scope.canonical_name module_ name.txt)
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
          Ok
            (Member_modality.of_atoms
               (Mode_atoms.declared ~default:Mode.Const.max atoms))
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

let prepare t (decl : Decl.t) =
  match decl.prepared with
  | Some prepared -> Ok prepared
  | None ->
    let%map.Result prepared = prepare_kind t decl in
    decl.prepared <- Some prepared;
    prepared
;;

let value_parameters t (root : Decl.t) =
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

let create fresh ~report =
  let t = create_tables fresh ~report in
  Fresh.set_value_parameters fresh (fun name ->
    Option.bind (Hashtbl.find t.types name) ~f:(fun decl ->
      match value_parameters t decl with
      | Ok params -> params
      | Error _ -> None));
  t
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

let check t name =
  let decl = Hashtbl.find_exn t.types name in
  match decl.checked with
  | Some checked -> Ok checked
  | None when decl.failed -> Error Typecheck_error.already_reported
  | None ->
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
    |> Result.map_error ~f:(fun error ->
      decl.failed <- true;
      (* Reported here, as the code that first used the declaration may be
         abandoned, and later uses are [Already_reported]. *)
      if not (Typecheck_error.is_already_reported error)
      then t.report (decl.wrap_error error);
      Typecheck_error.already_reported)
;;

let find_decl_exn t name =
  Option.value_exn (Hashtbl.find_exn t.types name).checked
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
    find_resolved
      t
      module_
      Constructor
      path
      ~loc
      ~unbound:(fun path -> Unbound_constructor path)
      ~find:constructor
;;

let find_field t module_ ({ txt = path; loc } : Decl_path.t Located.t) =
  match path with
  | Resolved name -> field t name ~loc
  | Path _ | Root _ ->
    find_resolved
      t
      module_
      Field
      path
      ~loc
      ~unbound:(fun path -> Unbound_field path)
      ~find:field
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

let pointer_free t ty =
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
      | Some result -> result
      | None ->
        let result = visit id ~f in
        Hashtbl.set table ~key:id ~data:result;
        result)
  in
  let rec ty_ params (ty : Type.ty) =
    match ty with
    | TVar { id; state = Generic; _ } when Map.mem params id ->
      Map.find_exn params id
    | TVar { id; state = Link target; _ } ->
      node ~table:links params id ~f:(fun () -> ty_ params target)
    | _ ->
      (match Type.canonical_ty ty with
       | TVar _ | TArrow _ | TCode _ -> false
       | TCon (("int" | "bool" | "char" | "unit"), []) -> true
       | TCon (name, _) when String.equal name Type.ptr_name -> false
       | TCon (name, args) -> nominal name (List.map args ~f:(arg params))
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
       | RVar _ -> false
       | RExtend (_, presence, _, entry, rest) ->
         (match Type.canonical_presence presence with
          | Absent -> true
          | Present | PVar _ -> ty_ params entry)
         && row_ params rest)
  and arg params : Type.arg -> bool = function
    | ATy entry -> ty_ params entry
    | ARow entry -> row_ params entry
    | AMode _ -> true
  and nominal name args =
    match Hashtbl.find nominals (name, args) with
    | Some result -> result
    | None ->
      let result = nominal' name args in
      if not (Hash_set.mem visiting name)
      then Hashtbl.set nominals ~key:(name, args) ~data:result;
      result
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
      | Some _ -> Result.map_error (check t name) ~f:ignore
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
  ty_ Int.Map.empty ty
;;
