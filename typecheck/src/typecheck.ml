open! Import

type t =
  { fresh : Fresh.t
  ; modes : Mode.Id_source.t
  ; stamps : Stamp_counter.t
  ; interpreter : Interpreter.t
  ; const_toplevels : bool
  ; compile_time_c_calls : Compile_time_c_calls.t
  ; prelude : Syntax.Toplevel.t list Lazy.t
  ; record_references : bool
  }

let create
  ~stamps
  ~interpreter
  ?(const_toplevels = false)
  ?(compile_time_c_calls = Compile_time_c_calls.none)
  ?(record_references = false)
  ?prelude
  ()
  =
  let fresh = Fresh.create () in
  { fresh
  ; modes = Fresh.modes fresh
  ; stamps
  ; interpreter
  ; const_toplevels
  ; compile_time_c_calls
  ; prelude =
      (match prelude with
       | Some prelude -> Lazy.from_val prelude
       | None -> Prelude.toplevels)
  ; record_references
  }
;;

let lift = Typecheck_error.lift
let error = Typecheck_error.error

let is_syntax ty =
  match Type.canonical_ty ty with
  | TCon (name, []) -> String.equal name (Prelude_path.canonical "Stag.Expr.t")
  | _ -> false
;;

let expr_node ~loc ?(made = Typed.Expr.Made.none) desc ty =
  { Typed.Expr.desc; ty; loc; made }
;;

let expr_fail (scope : Scope.t) ~loc ?(children = []) error =
  Scope.report scope error;
  expr_node ~loc (Error children) Type.error
;;

let rec var_type t (scope : Scope.t) ~at ~(path : Syntax.Decl_path.t) ~loc =
  let resolve ~name entry =
    Uses.resolution_type
      t
      scope
      ~at
      ~name
      ~loc
      (Resolution.of_entry scope ~name entry)
  in
  let local : (string * Env.Lookup.t) option =
    match path with
    | Path path ->
      (match (path :> string list) with
       | [ name ] -> Some (name, Env.lookup scope.locals name ~level:scope.level)
       | _ -> None)
    | Root _ | Resolved _ -> None
  in
  match local with
  | Some (name, Bound entry) -> resolve ~name (Some entry)
  | Some (name, Forgotten) ->
    (match scope.quote with
     | Some _ -> resolve ~name None
     | None -> error ~loc (Forgotten name))
  | Some (_, Unbound) | None ->
    (match%bind.Result Module_scope.find_value scope.module_ path ~loc with
     | Toplevel name ->
       toplevel_type t scope ~at (Program_env.toplevel scope.program name) ~loc
     | Prim prim -> Ok (Prim_types.prim_type scope prim, Typed.Var_ref.Prim prim)
     | Intrinsic intrinsic ->
       Ok
         ( Prim_types.intrinsic_type t scope intrinsic
         , Typed.Var_ref.Intrinsic intrinsic )
     | Unbound ->
       (match scope.quote with
        | Some _ -> resolve ~name:(Syntax.Decl_path.to_string path) None
        | None -> Error (Module_scope.unbound_value scope.module_ path ~loc)))

and toplevel_type t scope ~at (toplevel : Toplevel_binding.t) ~loc =
  let%bind.Result entry = toplevel_entry t scope toplevel ~loc in
  let name = toplevel.ident.name in
  Uses.resolution_type
    t
    scope
    ~at
    ~name
    ~loc
    (Resolution.of_entry scope ~name (Some entry))

(* A [Stag.Value_ref.t]'s canonical name, resolved where the code it was erased
   from was checked, so not looked up in [scope]. *)
and resolved_value_type t (scope : Scope.t) ~at name ~loc =
  match
    Prim.of_canonical_name name, Program_env.find_toplevel scope.program name
  with
  | Some prim, _ -> Ok (Prim_types.prim_type scope prim, Typed.Var_ref.Prim prim)
  | None, Some toplevel -> toplevel_type t scope ~at toplevel ~loc
  | None, None -> error ~loc (Not_in_scope (Path (Syntax.Path.of_string name)))

(* Generated code's variables are already resolved, so are found by binder
   rather than by name, which may be shadowed. *)
and var_ref_type t (scope : Scope.t) ~at (var_ref : Typed.Var_ref.t) ~loc =
  let resolve ~name entry =
    Uses.resolution_type
      t
      scope
      ~at
      ~name
      ~loc
      (Resolution.of_entry scope ~name (Some entry))
  in
  let not_in_scope name =
    error ~loc (Not_in_scope (Path (Syntax.Path.of_string name)))
  in
  match var_ref with
  | Prim prim -> Ok (Prim_types.prim_type scope prim, var_ref)
  | Intrinsic intrinsic ->
    Ok (Prim_types.intrinsic_type t scope intrinsic, var_ref)
  | Free name ->
    (match scope.quote with
     | Some quote ->
       Uses.resolution_type
         t
         scope
         ~at
         ~name
         ~loc
         (Outside_quote { quote; usable = None; unusable = None })
     | None -> not_in_scope name)
  | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
    let%bind.Result entry =
      toplevel_entry
        t
        scope
        (Program_env.toplevel scope.program ident.name)
        ~loc
    in
    resolve ~name:ident.name entry
  | Bound (Local ident as binder) | Persistent (Local ident as binder) ->
    (match Env.find_binder scope.locals binder ~name:ident.name with
     | Some entry -> resolve ~name:ident.name entry
     | None ->
       (match Program_env.embedded scope.program ident with
        | Some info ->
          Uses.use_entry
            scope
            ~at
            { scheme = info.ty
            ; level = 0
            ; quote_id = None
            ; mode =
                Mode_rules.Binding.embedded
                  (Scope.modes scope)
                  ~name:ident.name
                  ~loc
            ; held = None
            ; shape = None
            ; locks = []
            ; binder
            ; defining = false
            }
            ~name:ident.name
            ~loc;
          Ok
            ( Scope.instantiate ~at:loc t scope info.ty
            , Typed.Var_ref.Persistent binder )
        | None -> not_in_scope ident.name))

(* Toplevels may be used from code run at compile time, as if bound at its
   level, once they can be evaluated. *)
and toplevel_entry t (scope : Scope.t) (toplevel : Toplevel_binding.t) ~loc =
  let at_scope_level (entry : Env.Entry.t) =
    { entry with level = Int.min entry.level scope.level }
  in
  match toplevel.state with
  | Done { component; _ }
    when Component.failed component
         && scope.level < 0
         && Option.is_none scope.quote ->
    (* Code that did not check is not run. *)
    Component.fail scope.scc.component;
    Error Typecheck_error.already_reported
  | Done { runtime_only = Some global; _ }
    when scope.level < 0 && Option.is_none scope.quote ->
    error
      ~loc
      (Compile_time_use_of_mutable
         { toplevel = toplevel.ident.name; mutable_ = global })
  | Done { entry; component; typed = _; runtime_only = _; awaiting = _ } ->
    if Component.failed component then Component.fail scope.scc.component;
    Ok (at_scope_level entry)
  | In_progress in_progress ->
    let entry ?(defining = false) ?(scheme = in_progress.ty) mode =
      Env.Entry.toplevel
        ~defining
        ~ident:toplevel.ident
        ~scheme
        ~mode
        ~held:in_progress.held
    in
    (match scope.quote with
     | Some _
       when (not (Toplevel_binding.is_mut toplevel))
            && in_progress.index = scope.scc.id ->
       (* By symbol, in its own code: a recursive use, of its type. *)
       in_progress.recursive <- true;
       Ok
         (at_scope_level
            (entry
               (Mode_rules.Binding.being_checked
                  (Scope.modes scope)
                  ~name:toplevel.ident.name
                  ~loc)))
     | Some _ when not (Toplevel_binding.is_mut toplevel) ->
       (* Its mode isn't known yet, so the code refers to it by symbol: the
          quote's value doesn't depend on [toplevel], so neither does its
          component, which may be finished first. So the use is of a type of its
          own, generalized only with [toplevel]'s component. *)
       let ty = Uses.quoted_use t scope toplevel ~loc in
       Ok
         (at_scope_level
            (entry
               ~scheme:ty
               (Mode_rules.Binding.being_checked
                  (Scope.modes scope)
                  ~name:toplevel.ident.name
                  ~loc)))
     | None when scope.level < 0 ->
       (match Stack.top scope.program.splice_starts with
        | Some start when in_progress.index < start || scope.scc.lowlink < start
          -> raise Program_env.Uses_unfinished
        | Some _ | None ->
          error
            ~loc
            (Compile_time_use_of_toplevel_being_checked toplevel.ident.name))
     | Some _ | None ->
       (* [toplevel] is in the component being checked, so this is a recursive
          reference. A function captures nothing (toplevels are referred to by
          symbol), so its mode is already known. *)
       in_progress.recursive <- true;
       Scc_frame.depends_on scope.scc ~lowlink:in_progress.index;
       Ok
         (entry
            ~defining:(not (Toplevel_binding.is_function toplevel))
            in_progress.mode))
  | Unchecked ->
    Scc_frame.absorb scope.scc ~child:(check_toplevel t scope.program toplevel);
    toplevel_entry t scope toplevel ~loc

and check_toplevel t program (toplevel : Toplevel_binding.t) : Scc_frame.t =
  let index = Fresh.id t in
  Var_layout.within (Type_env.layouts program.type_env) ~first_id:index
  @@ fun layouts ->
  let scc = Scc_frame.create index Toplevel in
  Scc_frame.add_pending scc toplevel;
  Stack.push program.checking scc;
  Exn.protect
    ~finally:(fun () -> ignore (Stack.pop_exn program.checking : Scc_frame.t))
    ~f:(fun () -> check_toplevel_in t program toplevel scc ~index ~layouts)

and check_toplevel_in
  t
  program
  (toplevel : Toplevel_binding.t)
  scc
  ~index
  ~layouts
  =
  let unfinished = Stack.length program.unfinished in
  let reported = program.reported in
  let rank = Scc_frame.toplevel_rank in
  let sites =
    Allocation_sites.create toplevel.ident ~fresh:(fun () ->
      Program_env.allocation_id program)
  in
  let scope =
    { Scope.program
    ; level = 0
    ; rank
    ; locals = Env.create ()
    ; quote = None
    ; module_ = toplevel.module_
    ; scc
    ; annotation_vars = Type_env.Annotation_vars.create program.type_env ~rank
    ; in_expect_test = false
    ; generated = toplevel.generated
    ; syntax_splice = None
    ; locks = []
    ; params = []
    ; allocations = Some sites
    ; owner = None
    ; uses = Uses.of_code program scc
    }
  in
  let modes = Scope.modes scope in
  let fn =
    Function_type.create
      t
      scope
      ~args:(List.map (Toplevel_binding.args toplevel) ~f:Source.Expr.binding)
      ~annot:toplevel.annot
      ~res_mode:
        (if Toplevel_binding.is_function toplevel then toplevel.mode else None)
      ~body:(Source.Expr.Syntax toplevel.expr)
      ~loc:toplevel.expr.loc
  in
  let { Function_type.args; res_ty; ty; _ } = fn in
  match toplevel.state with
  | In_progress _ | Done _ ->
    (* Checked while elaborating its annotations, through a type splice that
       uses it: the cycle is reported. *)
    Scc_frame.create index Toplevel
  | Unchecked ->
    Stack.push program.unfinished ty;
    let loc = toplevel.expr.loc in
    let name = toplevel.ident.name in
    let rules =
      Mode_rules.Toplevel.create
        modes
        ~is_function:(Toplevel_binding.is_function toplevel)
        ~mut:(Toplevel_binding.is_mut toplevel)
        toplevel.mode
        ~var:(Scope.mode_var scope)
        ~name
        ~loc
    in
    Toplevel_binding.set_state
      toplevel
      (In_progress
         { index
         ; ty
         ; args = List.map args ~f:fst
         ; mode = rules.binding
         ; init_mode = rules.value
         ; held = rules.held
         ; body = None
         ; partials = []
         ; recursive = false
         ; sites
         });
    let body =
      match
        match toplevel.kind with
        | External external_ -> check_external t scope external_ ~ty:res_ty
        | Function _ ->
          let body, partials =
            check_body t scope ~closure:rules.value ~fn ~loc:toplevel.expr.loc
          in
          (match toplevel.state with
           | In_progress in_progress -> in_progress.partials <- partials
           | Unchecked | Done _ -> ());
          body
        | Value | Mut ->
          check_expr
            t
            scope
            ~at:rules.expected
            ~expr:(Source.Expr.Syntax toplevel.expr)
            ~ty:res_ty
      with
      | body -> body
      | exception Program_env.Uses_unfinished ->
        abandon program scc ~unfinished;
        raise Program_env.Uses_unfinished
    in
    Uses.code_checked t scope;
    Mode_rules.Toplevel.outlives modes rules ~ty ~name ~loc;
    if program.reported > reported then Component.fail scc.component;
    (match toplevel.state with
     | In_progress in_progress -> in_progress.body <- Some body
     | Unchecked | Done _ ->
       raise_s
         [%message
           "check_toplevel: not in progress" (toplevel : Toplevel_binding.t)]);
    if scc.lowlink = index
    then (
      let members = scc.pending in
      Scc_finish.finish_scc
        t
        program
        members
        ~layouts
        ~component:scc.component
        ~quoting:scc.quoting
        ~reopened:scc.reopened;
      Scc_frame.clear scc;
      while Stack.length program.unfinished > unfinished do
        Unify.lower_ty ~rank:0 (Stack.pop_exn program.unfinished)
      done;
      Scc_finish.settle t program members);
    scc

(* Its modes are as declared: C code is trusted to keep to them. *)
and check_external
  t
  (scope : Scope.t)
  ({ ty = declared; symbol; at_compile_time } : Toplevel_binding.External.t)
  ~ty
  : Typed.Expr.t
  =
  let loc = Loc.merge declared.loc symbol.loc in
  let error error =
    Scope.report scope error;
    ignore (Scope.unifies t scope ~ty1:ty ~ty2:Type.error ~loc : bool);
    { Typed.Expr.desc = Error []; ty; loc; made = Typed.Expr.Made.none }
  in
  match
    Type_env.elaborate_declared scope.program.type_env scope.module_ declared
  with
  | Error e -> error e
  | Ok _ when String.is_empty symbol.txt || String.mem symbol.txt '\000' ->
    error
      (Typecheck_error.create
         ~loc:symbol.loc
         (Invalid_external_symbol symbol.txt))
  | Ok declared_ty ->
    (match C_signature.of_type declared_ty with
     | Error problem ->
       error
         (Typecheck_error.create
            ~loc:declared.loc
            (Invalid_external { symbol = symbol.txt; problem }))
     | Ok signature ->
       ignore (Scope.unifies t scope ~ty1:ty ~ty2:declared_ty ~loc : bool);
       { desc = External { symbol = symbol.txt; signature; at_compile_time }
       ; ty
       ; loc
       ; made = Typed.Expr.Made.none
       })

(* Abandons the toplevels of [scc] with the splice whose code reached them, to
   be checked anew. *)
and abandon (program : Program_env.t) (scc : Scc_frame.t) ~unfinished =
  Component.abandon scc.component;
  List.iter scc.pending ~f:(fun (toplevel : Toplevel_binding.t) ->
    Toplevel_binding.set_state toplevel Unchecked);
  Scc_frame.clear scc;
  while Stack.length program.unfinished > unfinished do
    ignore (Stack.pop_exn program.unfinished : Type.ty)
  done

(* A function's body, in a closure of mode [closure]. Its parameters are of the
   modes of its arrows' arguments, and its value of the last one's result. The
   closure of a partial application holds what the function captures and the
   arguments given so far. With the sites of those, as
   [Typed.Expr.Made.partials]. *)
and check_body t (scope : Scope.t) ~closure ~(fn : Function_type.t) ~loc =
  let partials =
    Mode_rules.Function.partial_applications
      (Scope.modes scope)
      ~closure
      ~arg_modes:fn.arg_modes
      ~partial_modes:fn.partial_modes
      ~loc
  in
  let fn_params = Function_type.params fn in
  let scope =
    { (Scope.enter_closure t scope closure) with
      params =
        List.map2_exn fn_params fn.arg_modes ~f:(fun (_, ty) mode ->
          { Scope.Param.ty; mode })
        @ scope.params
    }
  in
  let modes = Scope.modes scope in
  let body =
    Mode_rules.Function.body
      modes
      ~res_mode:fn.res_mode
      ~ty:fn.res_ty
      (fun at ->
         let params =
           List.map2_exn
             fn_params
             fn.arg_modes
             ~f:(fun ((ident : Typed.Ident.t), ty) arg_mode ->
               let mode =
                 Mode_rules.Function.param modes ~arg_mode ~name:ident.name ~loc
               in
               ident.name, Scope.entry scope ~ident ~scheme:ty ~mode)
         in
         Env.with_bindings scope.locals params ~f:(fun () ->
           check_expr t scope ~at ~expr:fn.body ~ty:fn.res_ty))
  in
  ( List.fold_right
      fn.inner
      ~init:body
      ~f:(fun { Function_type.Level.args; annots; ty; loc } body ->
        { Typed.Expr.desc =
            Fun
              { args = Nonempty_list.of_list_exn (List.map args ~f:fst)
              ; annots
              ; body
              }
        ; ty
        ; loc
        ; made = Typed.Expr.Made.none
        })
  , partials )

and with_monomorphic_bindings (scope : Scope.t) bindings ~f =
  Env.with_bindings
    scope.locals
    (List.map bindings ~f:(fun ((ident : Typed.Ident.t), ty, mode) ->
       ident.name, Scope.entry scope ~ident ~scheme:ty ~mode))
    ~f

(* What fails to check is reported, and becomes [Error] of its parts that did,
   of type [Type.error]. Unless [hint] is [Required], [expected] is only what
   [expr] is likely to be, so it isn't unified with what may turn out to be
   something else. *)
and expr_type
  ?expected
  ?(hint = Expectation.Required)
  ?(applied = false)
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~(expr : Source.Expr.t)
  : Typed.Expr.t
  =
  let modes = Scope.modes scope in
  let loc = Source.Expr.loc expr in
  let node = expr_node ~loc in
  let fail = expr_fail scope ~loc in
  let discarded = discarded t scope in
  let infer exprs = List.map exprs ~f:discarded in
  let part ?ty modality check = Mode_rules.part ?ty modes at modality check in
  let tys = List.map ~f:(fun (expr : Typed.Expr.t) -> expr.ty) in
  let labelled_tys =
    List.map ~f:(fun (label, (expr : Typed.Expr.t)) -> label, expr.ty)
  in
  match Source.Expr.view expr with
  | Lit l -> node (Lit l) (Syntax.Lit.ty l)
  | Error ->
    Program_env.mark_syntax_errors scope.program;
    node (Error []) Type.error
  | Expect expected ->
    (* Only where it runs as part of the test: not at compile time nor in code. *)
    if Option.is_some scope.syntax_splice
    then fail (Typecheck_error.create ~loc Expect_in_syntax_splice)
    else if scope.in_expect_test
            && scope.level = 0
            && Option.is_none scope.quote
    then node (Expect expected) Type.unit
    else fail (Typecheck_error.create ~loc Expect_outside_expect_test)
  | Struct_quote toplevels ->
    (match Struct_quote.expand toplevels ~loc with
     | Ok expanded ->
       expr_type ?expected ~hint t scope ~at ~expr:(Source.Expr.Syntax expanded)
     | Error (loc, message) ->
       fail (Typecheck_error.create ~loc (Malformed_struct_quote message)))
  | Type ty ->
    (match
       Type_env.elaborate
         scope.program.type_env
         scope.module_
         (Type_env.Annotation_vars.create
            scope.program.type_env
            ~rank:scope.rank)
         ty
         ~splices:(Scope.splices scope)
     with
     | Error error -> fail error
     | Ok (elaborated, resolved) ->
       node
         (Type resolved)
         (TCon (Prelude_path.canonical "Stag.Type.t", [ ATy elaborated ])))
  | Var (Path (path, annot)) ->
    (match
       Mode_rules.annotated_use
         modes
         at
         ~annot:annot.mode
         ~var:(Scope.mode_var scope)
         ~loc
         (fun at ->
            let%map.Result ty, var_ref =
              match path with
              | Path _ | Root _ -> var_type t scope ~at ~path ~loc
              | Resolved name -> resolved_value_type t scope ~at name ~loc
            in
            node (Var var_ref) ty)
     with
     | Error error -> fail error
     | Ok var ->
       let var = if applied then var else Prim_types.value_sites scope var in
       annotate t scope var annot ~loc)
  | Marked (Nontail, inner) ->
    let inner =
      expr_type
        ?expected
        ~hint
        t
        scope
        ~at:(Mode_rules.Expected.nontail at)
        ~expr:inner
    in
    node (Marked (Nontail, inner)) inner.ty
  | Marked (Stack, inner) ->
    (match
       Mode_rules.Alloc.stack modes at ~loc (fun at ->
         expr_type ?expected ~hint t scope ~at ~expr:inner)
     with
     | Ok inner -> node (Marked (Stack, inner)) inner.ty
     | Error inner ->
       fail
         ~children:[ inner ]
         (Typecheck_error.create ~loc Stack_not_on_allocation))
  | Marked (Region, inner) ->
    let inner =
      Mode_rules.region modes at ~loc (fun at ->
        expr_type
          ?expected
          ~hint
          t
          { scope with
            locks = { Env.Lock.id = Fresh.id t; kind = Region } :: scope.locks
          }
          ~at
          ~expr:inner)
    in
    node (Marked (Region, inner)) inner.ty
  | Marked (Exclave, inner) ->
    (match
       Mode_rules.exclave modes at ~loc (fun at ->
         expr_type
           ?expected
           ~hint
           t
           { scope with
             locks = { Env.Lock.id = Fresh.id t; kind = Exclave } :: scope.locks
           }
           ~at
           ~expr:inner)
     with
     | Some inner -> node (Marked (Exclave, inner)) inner.ty
     | None ->
       fail
         ~children:[ discarded inner ]
         (Typecheck_error.create ~loc Exclave_not_in_tail_position))
  | Annotated (var, annot) ->
    let var =
      Mode_rules.annotated_use
        modes
        at
        ~annot:annot.mode
        ~var:(Scope.mode_var scope)
        ~loc
        (fun at -> Ok (expr_type t scope ~at ~expr:var))
      |> Result.ok_exn
    in
    annotate t scope var annot ~loc
  | Forget (names, body) ->
    Duplicates.report scope names ~error:(fun name -> Duplicate_forget name);
    Env.forgetting
      scope.locals
      (List.map names ~f:(fun (name : Syntax.Name.t) -> name.txt))
      ~level:scope.level
      ~f:(fun () -> expr_type t scope ~at ~expr:body)
  | Var (Ref var_ref) ->
    (match var_ref_type t scope ~at var_ref ~loc with
     | Error error -> fail error
     | Ok (ty, var_ref) ->
       let var = node (Var var_ref) ty in
       if applied then var else Prim_types.value_sites scope var)
  | App (f, x) -> application_type ?expected ~hint ~applied t scope ~at ~loc f x
  | Let { binder; args; annot; mode = mode_annot; body; in_ } ->
    let_type
      ?expected
      ~hint
      t
      scope
      ~at
      ~loc
      ~binder
      ~args
      ~annot
      ~mode_annot
      ~body
      ~in_
  | Seq (first, second) ->
    let first, (_ : Mode.Var.t) =
      Mode_rules.evaluate modes at ~loc ~why:"it is sequenced" (fun first_at ->
        check_expr t scope ~at:first_at ~expr:first ~ty:(Scope.fresh_ty t scope))
    in
    let second =
      expr_type
        t
        scope
        ~at:(Mode_rules.Expected.whole at)
        ~expr:second
        ?expected
        ~hint
    in
    node (Seq (first, second)) second.ty
  | Let_mut { binder; annot; mode = held_annot; body; in_ } ->
    let_mut_type
      ?expected
      ~hint
      t
      scope
      ~at
      ~loc
      ~binder
      ~annot
      ~held_annot
      ~body
      ~in_
  | Assign (var, value) -> assign_type t scope ~at ~loc var value
  | Fun { args; body } ->
    let fn =
      Function_type.create t scope ~args ~annot:None ~res_mode:None ~body ~loc
    in
    let { Function_type.args; annots; ty; _ } = fn in
    (* The parameters' types are known before the body is checked where they are
       expected, so that what crosses modes by its type does: [f (fun x -> ...)]
       with [f]'s type known. Used at it by variance, as the caller will: made
       equal, a later arm would be bound to an earlier one's modes. A mismatch
       is left to the caller to report. *)
    (match
       Option.bind expected ~f:(fun expected ->
         Pattern_check.either_known expected None)
     with
     | Some expected
       when Expectation.is_required hint && not (Unify.clash expected ty) ->
       ignore (Unify.sub_ty t ~lower:ty ~upper:expected : (unit, _) Result.t)
     | Some _ | None -> ());
    let (body, partials), made =
      Mode_rules.Alloc.closure modes at ~loc (fun closure ->
        check_body t scope ~closure ~fn ~loc)
    in
    node
      ~made:{ Typed.Expr.Made.none with made; partials }
      (Fun
         { args = Nonempty_list.of_list_exn (List.map args ~f:fst)
         ; annots
         ; body
         })
      ty
  | Quote expr -> quote_type ?expected ~hint t scope ~at ~loc expr
  | Splice { code; subs } ->
    splice_type ?expected ~hint t scope ~at ~loc ~expr ~code ~subs
  | Deref pointer ->
    (* What a pointer points to is global, as it is stored global. A box's
       contents are a part of it, read at most at its mode. *)
    let pointee, pointer, held =
      Mode_rules.project modes at ~loc ~why:"it is dereferenced" (fun at ->
        let fresh_held = Mode_rules.fresh_held modes in
        let pointer =
          expr_type
            t
            scope
            ~at
            ~expr:pointer
            ?expected:(Option.map expected ~f:(Type.ptr ~held:fresh_held))
            ~hint:Hint
        in
        match Type.of_box pointer.ty with
        | Some contents -> (contents, pointer, None), Modality.id, None
        | None ->
          let held =
            Option.value_map (Type.of_ptr pointer.ty) ~default:fresh_held ~f:snd
          in
          let pointee =
            match expected with
            | Some expected when Expectation.is_required hint -> expected
            | Some _ | None -> Scope.fresh_ty t scope
          in
          (pointee, pointer, Some held), Modality.of_cell held, None)
    in
    (match held with
     | None -> node (Deref pointer) pointee
     | Some held ->
       if Scope.unifies
            t
            scope
            ~ty1:pointer.ty
            ~ty2:(Type.ptr ~held pointee)
            ~loc
       then node (Deref pointer) pointee
       else node (Error [ pointer ]) Type.error)
  | Store (target, value) ->
    let rec under_deref (expr : Source.Expr.t) =
      match Source.Expr.view expr with
      | Deref _ -> true
      | Field (inner, _) | Poly_field (inner, _) -> under_deref inner
      | _ -> false
    in
    if not (under_deref target)
    then
      fail
        ~children:[ discarded target ]
        (Typecheck_error.create ~loc Store_not_through_pointer)
    else (
      (* The cell stored into is that of the pointer dereferenced last, then the
         fields of its value stored into. *)
      let rec stored (expr : Typed.Expr.t) =
        match expr.desc with
        | Deref pointer ->
          Modality.of_cell
            (Option.value_map
               (Type.of_ptr pointer.ty)
               ~default:Mode_arg.gc
               ~f:snd)
        | Field (inner, field) ->
          Modality.compose
            (stored inner)
            (Uses.field_modality scope field ~record:inner.ty)
        | Poly_field (inner, _) -> stored inner
        | _ -> Modality.of_cell Mode_arg.gc
      in
      let target, value =
        Mode_rules.store
          modes
          at
          ~loc
          ~target:(fun at ->
            let target = expr_type t scope ~at ~expr:target in
            let rec through_box (expr : Typed.Expr.t) =
              match expr.desc with
              | Deref pointer -> Option.is_some (Type.of_box pointer.ty)
              | Field (inner, _) | Poly_field (inner, _) -> through_box inner
              | _ -> false
            in
            if through_box target
            then Scope.report scope (Typecheck_error.create ~loc Store_into_box);
            target, stored target)
          ~value:(fun at ~ty -> check_expr t scope ~at ~expr:value ~ty)
      in
      node (Store (target, value)) Type.unit)
  | Tuple exprs ->
    let parts = Mode_rules.Parts.create modes at in
    let exprs =
      List.zip_exn
        exprs
        (Pattern_check.tuple_components expected (List.length exprs))
      |> List.mapi ~f:(fun index (expr, expected) ->
        Mode_rules.Parts.part parts (Index index) Modality.id (fun at ->
          expr_type t scope ~at ~expr ?expected ~hint))
    in
    Mode_rules.Parts.finish parts;
    node (Tuple exprs) (Type.TTuple (tys exprs))
  | Construct (constructor, args) ->
    (match
       Members.constructor_types
         t
         scope
         constructor
         ~args
         ~tuple:Source.Expr.tuple
         ~expected
         ~loc
     with
     | Error error -> fail error ~children:(infer args)
     | Ok (constructor, args, result) ->
       let parts = Mode_rules.Parts.create modes at in
       let args =
         List.mapi args ~f:(fun index (expr, ty, modality) ->
           Mode_rules.Parts.part
             parts
             (Arg (constructor.name, index))
             modality
             ~ty
             (fun at -> check_expr t scope ~at ~expr ~ty))
       in
       Mode_rules.Parts.finish parts;
       node (Construct (constructor, args)) result)
  | Record fields ->
    (match
       Members.record_fields t scope fields ~all_required:true ~expected ~loc
     with
     | Error error -> fail error ~children:(infer (List.map fields ~f:snd))
     | Ok (record, record_type, fields, complete) ->
       let parts = Mode_rules.Parts.create modes at in
       let exprs =
         List.map fields ~f:(fun ((field : Typed.Field_ref.t), (expr, ty)) ->
           Mode_rules.Parts.part
             parts
             (Field (Typed.Field_ref.name field))
             (Uses.field_modality scope field ~record)
             ~ty
             (fun at -> check_expr t scope ~at ~expr ~ty))
       in
       if complete then Mode_rules.Parts.finish parts;
       if complete
       then
         node
           (Record
              { record = record_type
              ; fields = List.zip_exn (List.map fields ~f:fst) exprs
              })
           record
       else node (Error exprs) record)
  | Field (expr, field)
    when Option.is_some (Members.Known.unqualified_field field) ->
    let expr, field =
      Mode_rules.project modes at ~loc ~why:"its field is read" (fun at ->
        let expr = expr_type t scope ~at ~expr in
        match Members.field_type t scope field ~record:(Some expr.ty) ~loc with
        | Error error -> (expr, Error error), Modality.id, None
        | Ok ((field, record, _) as typed) ->
          ( (expr, Ok typed)
          , Uses.field_modality scope field ~record
          , Some (Mode_shape.Step.Field (Typed.Field_ref.name field)) ))
    in
    (match field with
     | Error error -> fail error ~children:[ expr ]
     | Ok (field, record, field_ty) ->
       let field = node (Field (expr, field)) field_ty in
       if Scope.unifies t scope ~ty1:record ~ty2:expr.ty ~loc:expr.loc
       then field
       else node (Error [ field ]) Type.error)
  | Field (expr, field) ->
    (match Members.field_type t scope field ~record:None ~loc with
     | Error error -> fail error ~children:(infer [ expr ])
     | Ok (field, record, field_ty) ->
       let expr =
         Mode_rules.project modes at ~loc ~why:"its field is read" (fun at ->
           ( check_expr t scope ~at ~expr ~ty:record
           , Uses.field_modality scope field ~record
           , Some (Mode_shape.Step.Field (Typed.Field_ref.name field)) ))
       in
       node (Field (expr, field)) field_ty)
  | Record_update (expr, fields) ->
    record_update_type ?expected t scope ~at ~loc expr fields
  | Match { scrutinee = expr; cases; exception_cases } ->
    match_type
      ?expected
      ~hint
      t
      scope
      ~at
      ~loc
      ~scrutinee:expr
      ~cases
      ~exception_cases
  | Poly_record fields ->
    let parts = Mode_rules.Parts.create modes at in
    let fields = labelled_types t scope ~parts fields ~expected ~hint in
    Mode_rules.Parts.finish parts;
    node
      (Poly_record fields)
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:REmpty))
  | Poly_field (expr, label) ->
    let ty = Scope.fresh_ty t scope in
    let row, (_ : Type.row) = Poly_row.open_ t scope [ label, ty ] in
    let expr =
      Mode_rules.project modes at ~loc ~why:"its field is read" (fun at ->
        ( check_expr t scope ~at ~expr ~ty:(Fresh.record t row)
        , Modality.id
        , Some (Mode_shape.Step.Label label) ))
    in
    node (Poly_field (expr, label)) ty
  | Poly_extend (fields, expr) ->
    let fields =
      labelled_types
        t
        scope
        ~parts:(Mode_rules.Parts.create modes at)
        fields
        ~expected
        ~hint
    in
    let rest = Scope.fresh_row t scope in
    let expr =
      part Modality.id (fun at ->
        check_expr t scope ~at ~expr ~ty:(Fresh.record t rest))
    in
    node
      (Poly_extend (fields, expr))
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:rest))
  | Poly_restrict (expr, label) ->
    let row, rest = Poly_row.open_ t scope [ label, Scope.fresh_ty t scope ] in
    let expr =
      part Modality.id (fun at ->
        check_expr t scope ~at ~expr ~ty:(Fresh.record t row))
    in
    node (Poly_restrict (expr, label)) (Fresh.record t rest)
  | Poly_update (expr, fields) ->
    let fields =
      labelled_types
        t
        scope
        ~parts:(Mode_rules.Parts.create modes at)
        fields
        ~expected
        ~hint
    in
    let row, rest =
      Poly_row.open_
        t
        scope
        (List.map fields ~f:(fun (label, _) -> label, Scope.fresh_ty t scope))
    in
    let expr =
      part Modality.id (fun at ->
        check_expr t scope ~at ~expr ~ty:(Fresh.record t row))
    in
    node
      (Poly_update (expr, fields))
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:rest))
  | Poly_variant (tag, arg) ->
    let parts = Mode_rules.Parts.create modes at in
    let arg =
      Option.map arg ~f:(fun expr ->
        Mode_rules.Parts.part parts (Tag tag) Modality.id (fun at ->
          expr_type
            t
            scope
            ~at
            ~expr
            ?expected:(Pattern_check.row_entry expected `Variant ~label:tag)
            ~hint))
    in
    Mode_rules.Parts.finish parts;
    let arg_ty =
      Option.value_map arg ~default:Type.unit ~f:(fun (expr : Typed.Expr.t) ->
        expr.ty)
    in
    node (Poly_variant (tag, arg)) (Poly_row.variant t scope ~tag ~arg:arg_ty)

(* Only checked for its errors, as what it is part of did not check. *)
and discarded t scope expr =
  expr_type t scope ~at:(Mode_rules.Expected.fresh (Scope.modes scope)) ~expr

and application_type
  ?expected
  ~hint
  ~applied
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  f
  x
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  Mode_rules.runs modes at Portable ~loc ~why:"it applies a function";
  let a = Scope.fresh_ty t scope in
  let b = Scope.fresh_ty t scope in
  let arg_mode = Scope.arrow_mode scope in
  let res_mode = Scope.arrow_mode scope in
  let curry = Scope.arrow_mode scope in
  let curried =
    match Source.Expr.view f with
    | App _ -> true
    | _ -> false
  in
  (* The expected result is known before the argument, which may need it:
     [Ptr.make (C x)] resolves [C] in the expected pointee type. A clash is left
     to whoever expects the result to report. A hint is only taken once the
     result is known to be of its form, as a variable may become another type: a
     toplevel splice's code may be syntax. *)
  let result_matches_expected () =
    match expected with
    | Some expected
      when (not (Unify.clash expected b))
           && not
                ((not (Expectation.is_required hint))
                 &&
                 match Type.canonical_ty b with
                 | TVar _ -> true
                 | _ -> false) ->
      Scope.subtypes t scope ~lower:b ~upper:expected ~loc
    | Some _ | None -> true
  in
  let result_matches = ref true in
  let f, x, allocation =
    Mode_rules.apply
      modes
      at
      ~loc
      ~curried
      ~arg_mode
      ~res_mode
      ~curry
      ~arg_ty:a
      ~res_ty:b
      ~callee:(fun at ->
        let (f : Typed.Expr.t) =
          check_expr
            ~applied:true
            t
            scope
            ~at
            ~expr:f
            ~ty:
              (TArrow
                 ( a
                 , b
                 , { arg = Var arg_mode; res = Var res_mode; curry = Var curry }
                 ))
        in
        let allocation : Typed.Allocation.Kind.t option =
          match f.desc, Prim_types.prim_head f with
          | _, Some (prim, _, args)
            when (not applied) && args + 1 < Prim.arity prim ->
            Some Partial_application
          | Var (Prim _), _ -> None
          | Var var, _ -> Prim_types.value_allocation scope var
          | _ -> None
        in
        f, allocation)
      ~arg:(fun allocation x_at ->
        result_matches := result_matches_expected ();
        let x_at =
          match allocation with
          | Some Box -> Mode_rules.Expected.boxed x_at ~whole:at
          | Some _ | None -> x_at
        in
        check_expr t scope ~at:x_at ~expr:x ~ty:a)
  in
  let made =
    match allocation, Prim_types.prim_head f with
    | Some Partial_application, Some (_, head, args) ->
      Option.bind
        (Prim_types.prim_partial head.ty ~args:(args + 1))
        ~f:(fun partial ->
          Mode_rules.Alloc.pointer
            modes
            at
            ~loc
            ~kind:Partial_application
            partial)
    | Some ((Box | Pointer) as kind), _ ->
      Mode_rules.Alloc.pointer modes at ~loc ~kind res_mode
    | (Some _ | None), _ -> None
  in
  if !result_matches
  then node ~made:{ Typed.Expr.Made.none with made } (App (f, x)) b
  else node (Error [ f; x ]) Type.error

and let_type
  ?expected
  ~hint
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  ~binder
  ~args
  ~annot
  ~mode_annot
  ~body
  ~in_
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let inner =
    { scope with rank = scope.rank + 1; uses = Local_uses.enter scope.uses }
  in
  let is_function = not (List.is_empty args) in
  let name = Source.Binder.name binder in
  let frame = ref None in
  Var_layout.within
    (Type_env.layouts scope.program.type_env)
    ~first_id:(Fresh.next_id t)
  @@ fun layouts ->
  let (fn, ident, body, closure, made), shape =
    Mode_rules.evaluate_shaped
      modes
      at
      ~inner:(Scope.modes inner)
      ~loc
      ~why:(sprintf "%s is bound" name)
      (fun inner_at ->
         let fn =
           Function_type.create
             t
             inner
             ~args
             ~annot
             ~res_mode:(if is_function then mode_annot else None)
             ~body
             ~loc
         in
         let ident =
           Program_env.bind scope.program binder fn.ty ~level:scope.level
         in
         (* Its function's sites, or its value's, are its own if it is
            generalized. *)
         frame
         := Option.map scope.allocations ~f:(fun sites ->
              Allocation_sites.frame
                sites
                ~parent:scope.owner
                ~name
                ~loc
                ~ty:fn.ty);
         let own =
           { inner with owner = Option.first_some !frame inner.owner }
         in
         let defining mode ~f =
           Env.with_bindings
             scope.locals
             [ name, Scope.entry scope ~defining:true ~ident ~scheme:fn.ty ~mode
             ]
             ~f
         in
         (* A closure may refer to itself, but a value referring to itself is
            not known yet, as a [let rec]'s isn't. *)
         match fn.args with
         | [] ->
           let body =
             defining
               (Mode_rules.Binding.defining modes inner_at ~name ~loc)
               ~f:(fun () ->
                 check_expr t own ~at:inner_at ~expr:body ~ty:fn.res_ty)
           in
           fn, ident, body, None, Typed.Expr.Made.none
         | _ :: _ ->
           let (body, partials, closure), made =
             Mode_rules.Alloc.closure
               (Scope.modes inner)
               inner_at
               ~loc
               (fun closure ->
                  let body, partials =
                    defining (Mode_rules.Binding.closure closure) ~f:(fun () ->
                      check_body t own ~closure ~fn ~loc)
                  in
                  body, partials, Mode_rules.Binding.closure closure)
           in
           ( fn
           , ident
           , body
           , Some closure
           , { Typed.Expr.Made.none with made; partials } ))
  in
  let mode = shape.whole in
  let { Function_type.args = arg_types; annots; result_annot; ty; _ } = fn in
  let args = List.map arg_types ~f:fst in
  let generalized = Uses.generalizes scope mode ~is_function ~loc in
  let decided errors =
    List.iter errors ~f:(fun error ->
      ignore (Scope.mode_holds scope ~loc (Error error) : bool))
  in
  (* A value's binding has the modes of [shape], which are of this rank, so a
     closure capturing it and generalized keeps their constraints with it. A
     function's is its closure's. *)
  if Option.is_none closure then Mode_shape.lower_rank shape ~rank:scope.rank;
  (* The uses its definition made of toplevels being checked are generalized
     with it, each instance having its own (see [Local_uses]). *)
  Local_uses.leave
    inner.uses
    ~rank:scope.rank
    ~ident
    ~ty
    ~generalize:
      (Option.some_if generalized (fun tys ->
         Option.iter !frame ~f:Allocation_sites.Frame.set_generalized;
         List.iter
           (Scheme.generalize_watchers t ~rank:scope.rank tys)
           ~f:(fun error ->
             Scope.report scope (Typecheck_error.unification_error ~loc error));
         decided
           (Var_layout.decide layouts ~rank:scope.rank ~finality:Provisional);
         Scheme.generalize t ~rank:scope.rank tys));
  if not generalized
  then (
    Unify.lower_ty ~rank:scope.rank ty;
    decided (Var_layout.decide layouts ~rank:scope.rank ~finality:Provisional));
  let in_ =
    Env.with_bindings
      scope.locals
      [ ( name
        , Scope.entry
            scope
            ~ident
            ~scheme:ty
            ?shape:
              (match closure, mode_annot with
               | None, None -> Some shape
               | Some _, _ | _, Some _ -> None)
            ~mode:
              (match closure with
               | Some closure -> closure
               | None ->
                 Mode_rules.Binding.value
                   modes
                   mode
                   mode_annot
                   ~var:(Scope.mode_var scope)) )
      ]
      ~f:(fun () ->
        expr_type
          t
          scope
          ~at:(Mode_rules.Expected.whole at)
          ~expr:in_
          ?expected
          ~hint)
  in
  node
    ~made:
      { made with
        owns =
          (if generalized
           then Option.map !frame ~f:Allocation_sites.Frame.id
           else None)
      }
    (Let
       { ident
       ; args
       ; annots
       ; result_annot
       ; mode = mode_annot
       ; body
       ; in_
       ; generalized
       })
    in_.ty

and let_mut_type
  ?expected
  ~hint
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  ~binder
  ~annot
  ~held_annot
  ~body
  ~in_
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let ty, annot =
    match annot with
    | None -> Scope.fresh_ty t scope, None
    | Some annot ->
      (match Scope.elaborate_resolved scope annot with
       | Ok (ty, resolved) -> ty, Some resolved
       | Error error ->
         Scope.report scope error;
         Scope.fresh_ty t scope, None)
  in
  let name = Source.Binder.name binder in
  let (body, ident), value =
    Mode_rules.evaluate
      modes
      at
      ~loc
      ~why:(sprintf "%s is bound" name)
      (fun body_at ->
         let body = check_expr t scope ~at:body_at ~expr:body ~ty in
         let ident =
           Program_env.bind scope.program binder ty ~level:scope.level
         in
         body, ident)
  in
  Hash_set.add scope.program.mutable_idents ident;
  let mode, held =
    Mode_rules.Binding.mutable_
      ~ty
      modes
      ~value
      held_annot
      ~var:(Scope.mode_var scope)
      ~name
      ~loc
  in
  let made = Mode_rules.Alloc.cell modes ~loc (mode :> Mode.Var.t) in
  let in_ =
    Env.with_bindings
      scope.locals
      [ name, Scope.entry scope ~ident ~scheme:ty ~mode ~held ]
      ~f:(fun () ->
        expr_type
          t
          scope
          ~at:(Mode_rules.Expected.whole at)
          ~expr:in_
          ?expected
          ~hint)
  in
  node
    ~made:{ Typed.Expr.Made.none with made }
    (Let_mut { ident; annot; mode = held_annot; body; in_ })
    in_.ty

and assign_type t (scope : Scope.t) ~(at : Mode_rules.Expected.t) ~loc var value
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let fail = expr_fail scope ~loc in
  let discarded = discarded t scope in
  Mode_rules.runs modes at Nonportable ~loc ~why:"it assigns a variable";
  let target, (_ : Mode.Var.t) =
    Mode_rules.evaluate modes at ~loc ~why:"it is assigned to" (fun at ->
      match var with
      | Path (path, _) ->
        (match path with
         | Path _ | Root _ -> var_type t scope ~at ~path ~loc
         | Resolved name -> resolved_value_type t scope ~at name ~loc)
      | Ref var_ref -> var_ref_type t scope ~at var_ref ~loc)
  in
  let name =
    match var with
    | Path (path, _) -> Syntax.Decl_path.to_string path
    | Ref (Bound (Local ident | Toplevel ident))
    | Ref (Persistent (Local ident | Toplevel ident)) -> ident.name
    | Ref (Free name) -> name
    | Ref (Prim prim) -> Prim.canonical_name prim
    | Ref (Intrinsic intrinsic) -> Intrinsic.public_name intrinsic
  in
  let assignable : Typed.Var_ref.t -> _ = function
    | (Bound binder | Persistent binder) as var_ref ->
      let (Local ident | Toplevel ident) = binder in
      if Program_env.is_mutable scope.program ident
      then Ok var_ref
      else error ~loc (Assign_to_immutable name)
    | Free free_name as var_ref ->
      (match scope.quote with
       | None -> error ~loc (Assign_to_immutable name)
       | Some quote ->
         let%map.Result (_ : Type.ty) =
           Unify.present_entry
             ~assigned:true
             t
             ~row:quote.context
             ~name:free_name
             ~rank:quote.outside.rank
           |> lift ~loc
         in
         var_ref)
    | Prim _ | Intrinsic _ -> error ~loc (Assign_to_immutable name)
  in
  match
    Result.bind target ~f:(fun (ty, var_ref) ->
      Result.map (assignable var_ref) ~f:(fun var_ref -> ty, var_ref))
  with
  | Error error -> fail ~children:[ discarded value ] error
  | Ok (ty, var_ref) ->
    let assigned, (held, crossed) =
      match var_ref with
      | Bound (Local ident) | Persistent (Local ident) ->
        (match Env.find_binder scope.locals (Local ident) ~name:ident.name with
         | Some entry ->
           ident.name, (entry.held, Env.Entry.crossed entry ~locks:scope.locks)
         | None ->
           raise_s
             [%message "Assigned local not in scope" (ident : Typed.Ident.t)])
      | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
        ( ident.name
        , ( (match (Program_env.toplevel scope.program ident.name).state with
             | In_progress { held; _ } -> held
             | Done { entry; _ } -> entry.held
             | Unchecked ->
               raise_s
                 [%message
                   "Assigned toplevel not checked" (ident : Typed.Ident.t)])
          , [] ) )
      | Free _ -> name, (None, [])
      | Prim _ | Intrinsic _ ->
        raise_s [%message "Assigned to an immutable" name]
    in
    let value =
      Mode_rules.assign
        modes
        at
        ~held
        ~crossed
        ~ty
        ~name:assigned
        ~loc
        (fun at -> check_expr t scope ~at ~expr:value ~ty)
    in
    node (Assign (var_ref, value)) Type.unit

and quote_type
  ?expected
  ~hint
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  expr
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let mode, made = Mode_rules.Alloc.code modes at ~loc in
  let quote =
    { Scope.id = Fresh.id t
    ; context = Scope.fresh_row t scope
    ; outside = scope
    ; mode
    }
  in
  let expected =
    match Option.map expected ~f:Type.canonical_ty with
    | Some (TCode { inner; context = _ }) -> Some inner
    | Some _ | None -> None
  in
  let inner =
    expr_type
      t
      { scope with
        level = scope.level + 1
      ; quote = Some quote
      ; locks = { id = Fresh.id t; kind = Quote quote.mode } :: scope.locks
      }
      ~at:(Mode_rules.Expected.quoted modes)
      ~expr
      ?expected
      ~hint:(Expectation.quoted hint)
  in
  node
    ~made:{ Typed.Expr.Made.none with made }
    (Quote inner)
    (TCode { inner = inner.ty; context = quote.context })

and splice_type
  ?expected
  ~hint
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  ~(expr : Source.Expr.t)
  ~code
  ~subs
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let fail = expr_fail scope ~loc in
  let source_subs = subs in
  (match scope.quote, expr with
   | None, Generated _ ->
     raise_s [%message "Generated code has a splice outside any quote"]
   | None, Syntax _ | Some _, _ -> ());
  let (outside : Scope.t), check_code =
    match scope.quote with
    | Some quote ->
      Mode_rules.runs
        modes
        at
        Nonportable
        ~loc
        ~why:"the mode of spliced code is not known";
      ( { quote.outside with
          rank = scope.rank
        ; locks = { id = Fresh.id t; kind = Splice } :: scope.locks
        }
      , fun check -> Mode_rules.Splice.code ~code:quote.mode check )
    | None ->
      ( { scope with level = scope.level - 1 }
      , fun check -> check (Mode_rules.Expected.fresh modes) )
  in
  let reported = scope.program.reported in
  let inner = Scope.fresh_ty t scope in
  let context = Scope.fresh_row t scope in
  let code_expected =
    Option.map expected ~f:(fun inner ->
      Type.TCode { inner; context = Scope.fresh_row t outside })
  in
  let substitutions () =
    match subs with
    | Implicit ->
      let%map.Result captures =
        substitute_from_scope
          t
          scope
          ~at:(Mode_rules.Splice.captured at)
          ~context
          ~loc
          ~may_capture:(fun _ _ -> true)
      in
      Typed.Expr.Subs.Implicit captures, String.Map.empty
    | Providers providers ->
      let%map.Result captures =
        substitute_from_scope
          t
          scope
          ~at:(Mode_rules.Splice.captured at)
          ~context
          ~loc
          ~may_capture:(fun name binder ->
            [%equal: Typed.Binder.t option]
              (Map.find providers name)
              (Some binder))
      in
      Typed.Expr.Subs.Implicit captures, String.Map.empty
    | Explicit subs ->
      let%map.Result subs, bindings =
        substitute_explicitly t scope ~at ~context ~subs ~loc
      in
      Typed.Expr.Subs.Explicit subs, bindings
  in
  let failed code =
    Var_layout.check_waiting (Type_env.layouts scope.program.type_env);
    scope.program.reported > reported || Typed.Expr.has_error code
  in
  match scope.quote with
  | Some _ ->
    let code =
      check_code (fun at ->
        check_expr
          t
          outside
          ~at
          ~expr:code
          ~ty:(TCode { inner; context })
          ?expected:code_expected
          ~hint)
    in
    (match substitutions () with
     | Error error -> fail error ~children:[ code ]
     | Ok (subs, (_ : Binding.t String.Map.t)) ->
       node (Splice { code; subs }) inner)
  | None ->
    let code =
      check_code (fun at ->
        expr_type
          t
          outside
          ~at
          ~expr:code
          ?expected:code_expected
          ~hint:(Code_hint hint))
    in
    if is_syntax code.ty
    then (
      match subs with
      | Explicit _ ->
        fail
          (Typecheck_error.create ~loc Syntax_splice_with_substitutions)
          ~children:[ code ]
      | Implicit | Providers _ ->
        if failed code
        then node (Error [ code ]) Type.error
        else (
          match splice_syntax t scope ~at ~code ~loc ?expected ~hint with
          | Error error -> fail error ~children:[ code ]
          | Ok expansion ->
            node (Toplevel_splice { code; expansion }) expansion.ty))
    else (
      match Unify.unify_ty t ~ty1:(TCode { inner; context }) ~ty2:code.ty with
      | Error _ ->
        fail
          (Typecheck_error.create
             ~loc:code.loc
             (Toplevel_splice_of_neither
                (Type.Snapshot.ty
                   (Type.Snapshot.create ())
                   (Type.canonical_ty code.ty))))
          ~children:[ code ]
      | Ok () ->
        let listed = listed_entries context in
        (match substitutions () with
         | Error error -> fail error ~children:[ code ]
         | Ok (subs, bindings) ->
           if failed code
           then
             (* Code that did not check is not run. *)
             node
               (Error
                  (code
                   ::
                   (match subs with
                    | Explicit subs -> List.map subs ~f:snd
                    | Implicit _ -> [])))
               Type.error
           else (
             let name_locs =
               match source_subs with
               | Explicit source_subs ->
                 String.Map.of_alist_reduce
                   (List.map source_subs ~f:(fun ({ Located.txt; loc }, _) ->
                      txt, loc))
                   ~f:(fun first _ -> first)
               | Implicit | Providers _ -> String.Map.empty
             in
             match
               expand
                 t
                 scope
                 ~at
                 ~code
                 ~subs
                 ~bindings
                 ~listed
                 ~name_locs
                 ~ty:inner
                 ~loc
             with
             | Error error -> fail error ~children:[ code ]
             | Ok expansion -> node (Toplevel_splice { code; expansion }) inner)))

and record_update_type
  ?expected
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  expr
  fields
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let fail = expr_fail scope ~loc in
  let discarded = discarded t scope in
  let infer exprs = List.map exprs ~f:discarded in
  let part ?ty modality check = Mode_rules.part ?ty modes at modality check in
  (* As in OCaml, the result's type parameters are those of [expr]'s type but
     for the ones only in the updated fields' types. *)
  match Members.Known.record scope expected with
  | Error error -> fail error ~children:(infer (expr :: List.map fields ~f:snd))
  | Ok known_expected ->
    (* What it does not update is copied. *)
    let expr =
      part Modality.id (fun at ->
        expr_type
          t
          scope
          ~at
          ~expr
          ?expected:
            (Option.bind known_expected ~f:(fun (known : _ Members.Known.t) ->
               List.hd known.members
               |> Option.map ~f:(fun (member : Type_env.Field.t) ->
                 Scope.instantiate t scope member.record))))
    in
    (match
       Members.resolve_record_fields
         scope
         fields
         ~all_required:false
         ~expected:(Pattern_check.either_known expr.ty expected)
         ~loc
     with
     | Error error ->
       fail error ~children:(expr :: infer (List.map fields ~f:snd))
     | Ok { known; declared; record_type; fields; complete = _ } ->
       let inst_in = Scope.instantiate t scope in
       let inst_out = Scope.instantiate t scope in
       let unified =
         Scope.unifies
           t
           scope
           ~ty1:(inst_in declared)
           ~ty2:expr.ty
           ~loc:expr.loc
       in
       let record = inst_out declared in
       let unified =
         match Members.unify_known t ~expected record ~loc with
         | Ok () -> unified
         | Error error ->
           Scope.report scope error;
           false
       in
       let unified =
         List.filter known.members ~f:(Fn.non (Members.is_given fields))
         |> List.fold ~init:unified ~f:(fun unified member ->
           Scope.unifies
             t
             scope
             ~ty1:(inst_in member.ty)
             ~ty2:(inst_out member.ty)
             ~loc
           && unified)
       in
       (* So are the mode parameters of their modalities. *)
       let unified =
         let kept =
           List.filter_map known.members ~f:(fun member ->
             match member.modality with
             | Param index when not (Members.is_given fields member) ->
               Some index
             | Param _ | Id | Meet _ -> None)
         in
         match
           Type.canonical_ty (inst_in declared), Type.canonical_ty record
         with
         | TCon (name, args_in), TCon (_, args_out)
           when not (List.is_empty kept) ->
           Scope.unifies
             t
             scope
             ~ty1:
               (TCon
                  ( name
                  , List.mapi args_out ~f:(fun index arg ->
                      if List.mem kept index ~equal:Int.equal
                      then List.nth_exn args_in index
                      else arg) ))
             ~ty2:record
             ~loc
           && unified
         | _ -> unified
       in
       let exprs =
         List.map fields ~f:(fun (field, _, expr) ->
           let ty = inst_out field.ty in
           part
             (Uses.field_modality scope (Members.field_ref field) ~record)
             ~ty
             (fun at -> check_expr t scope ~at ~expr ~ty))
       in
       let update =
         node
           (Record_update
              { record = record_type
              ; expr
              ; fields =
                  List.zip_exn
                    (List.map fields ~f:(fun (field, _, _) ->
                       Members.field_ref field))
                    exprs
              })
           record
       in
       if unified then update else node (Error [ update ]) Type.error)

and match_type
  ?expected
  ~hint
  t
  (scope : Scope.t)
  ~(at : Mode_rules.Expected.t)
  ~loc
  ~scrutinee:expr
  ~cases
  ~exception_cases
  =
  let modes = Scope.modes scope in
  let node = expr_node ~loc in
  let scrutinee, scrutinee_shape =
    Mode_rules.evaluate_shaped modes at ~loc ~why:"it is matched on" (fun at ->
      expr_type t scope ~at ~expr)
  in
  let result = Scope.join_ty t scope in
  (* Closing the scrutinee to patterns in error would only add errors. *)
  let patterns_failed = ref false in
  let counting_errors f =
    let before = scope.program.reported in
    let result = f () in
    if scope.program.reported > before then patterns_failed := true;
    result
  in
  (* A case of [pattern], typed with its [bindings], of type [ty], and whether
     the pattern is of [ty]. *)
  let arms = Mode_rules.Arms.create modes at in
  let case ~ty ~shape ((pattern : Typed.Pattern.t), bindings) body =
    let pattern, unified =
      counting_errors (fun () ->
        let before = scope.program.reported in
        Duplicates.report
          scope
          (List.map bindings ~f:(fun (var : Pattern_var.t) ->
             { Located.txt = var.ident.name; loc = var.loc }))
          ~error:(fun name -> Duplicate_pattern_variable name);
        let unified =
          Scope.unifies t scope ~ty1:ty ~ty2:pattern.ty ~loc:pattern.loc
        in
        ( (if unified && scope.program.reported = before
           then pattern
           else Typed.Pattern.error [ pattern ] ~loc:pattern.loc)
        , unified ))
    in
    let body =
      Env.with_bindings
        scope.locals
        (List.map bindings ~f:(fun (var : Pattern_var.t) ->
           let mode, shape =
             Mode_rules.Binding.component modes shape var.path ~loc:var.loc
           in
           ( var.ident.name
           , Scope.entry scope ~ident:var.ident ~scheme:var.ty ?shape ~mode )))
        ~f:(fun () ->
          (* Typed on its own, as the expected type would make it the result's,
             then used at the result: the join of the cases. *)
          let body =
            check_expr
              t
              scope
              ~at:(Mode_rules.Arms.arm arms)
              ~expr:body
              ~ty:(Scope.fresh_ty t scope)
              ?expected:(Pattern_check.either_known result expected)
              ~hint:
                (match Type.canonical_ty result with
                 | TVar _ -> hint
                 | _ ->
                   (* An earlier case's type, which this one must have. *)
                   Required)
          in
          if Type.is_error body.ty
             || Scope.subtypes
                  t
                  scope
                  ~lower:body.ty
                  ~upper:result
                  ~loc:body.loc
          then body
          else
            { desc = Error [ body ]
            ; ty = Type.error
            ; loc = body.loc
            ; made = Typed.Expr.Made.none
            })
    in
    (pattern, body), unified
  in
  (match cases, exception_cases with
   | [], _ :: _ ->
     patterns_failed := true;
     Scope.report scope (Typecheck_error.create ~loc Match_without_value_cases)
   | _ -> ());
  let (_ : Variant_match.t), (_ : Type.ty), cases =
    List.fold
      cases
      ~init:(Variant_match.empty, scrutinee.ty, [])
      ~f:(fun (handled, ty, typed_cases) (pattern, body) ->
        let typed_pattern =
          counting_errors (fun () ->
            Variant_match.pattern_type t scope handled ~pattern ~scrutinee:ty)
        in
        let ((pattern, _) as typed_case), unified =
          case ~ty ~shape:scrutinee_shape typed_pattern body
        in
        (* So that later cases don't report the same mismatch. *)
        let ty = if unified then ty else Scope.fresh_ty t scope in
        Variant_match.add_case handled pattern, ty, typed_case :: typed_cases)
  in
  let cases = List.rev cases in
  let exception_cases =
    List.map exception_cases ~f:(fun (pattern, body) ->
      fst
        (case
           ~ty:Type.exn
           ~shape:(Mode_shape.opaque (Mode_rules.raised modes ~loc))
           (Pattern_check.pattern_type t scope ~pattern ~expected:Type.exn)
           body))
  in
  Mode_rules.Arms.finish arms ~loc;
  (match expr, cases with
   | _ when !patterns_failed -> ()
   | Generated _, [] ->
     (* Its scrutinee was closed, unless of an empty nominal type, which may
        have been known only from an annotation. *)
     ()
   | _ ->
     Variant_match.close
       t
       scope
       ~scrutinee:scrutinee.ty
       ~loc
       (List.map cases ~f:fst)
     |> Result.iter_error ~f:(fun error ->
       patterns_failed := true;
       Scope.report scope error));
  let match_ = node (Match { scrutinee; cases; exception_cases }) result in
  if !patterns_failed then node (Error [ match_ ]) result else match_

(* [var] was checked at its mode, by [Mode_rules.annotated_use]. *)
and annotate
  t
  (scope : Scope.t)
  (var : Typed.Expr.t)
  (annot : Syntax.Annot.t)
  ~loc
  : Typed.Expr.t
  =
  match annot.ty with
  | None when Option.is_none annot.mode -> var
  | None ->
    { var with desc = Annotated (var, annot); loc; made = Typed.Expr.Made.none }
  | Some ty ->
    (match Scope.elaborate_resolved scope ty with
     | Ok (annot_ty, resolved) ->
       let annotated =
         { Typed.Expr.desc = Annotated (var, { annot with ty = Some resolved })
         ; ty = (if Type.is_error var.ty then var.ty else annot_ty)
         ; loc
         ; made = Typed.Expr.Made.none
         }
       in
       if Scope.subtypes t scope ~lower:var.ty ~upper:annot_ty ~loc
       then annotated
       else
         { desc = Error [ annotated ]
         ; ty = Type.error
         ; loc
         ; made = Typed.Expr.Made.none
         }
     | Error error ->
       Scope.report scope error;
       var)

(* Runs the toplevel splice [code] of a [Stag.Expr.t], then infers the
   expression it gives here, checking it against [expected] so that a mismatch
   is reported as the expression's. *)
and splice_syntax ?expected ~hint t (scope : Scope.t) ~at ~code ~loc =
  let program = scope.program in
  let%bind.Result value =
    Program_env.at_compile_time
      ~loc
      program
      (fun ~reflect ->
        Interpreter.run
          program.interpreter
          code
          ~level:(scope.level - 1)
          ~reflect)
      ~error:(fun error ->
        Typecheck_error.create ~loc (Splice_evaluation_failed { code; error }))
  in
  let%map.Result generated =
    Interpreter.Value.to_syntax value Generated_syntax.Expr ~loc
    |> Result.map_error ~f:(fun error ->
      Typecheck_error.create ~loc (Malformed_syntax_splice error))
  in
  let scope = { scope with syntax_splice = Some { splice = loc; generated } } in
  let expr = Source.Expr.Syntax generated in
  match expected with
  | Some ty when Expectation.is_required hint ->
    check_expr t scope ~at ~expr ~ty
  | Some _ | None -> expr_type t scope ~at ~expr ?expected ~hint

(* The names [row] has an entry for that may be present. *)
and listed_entries (row : Type.row) =
  match Type.canonical_row row with
  | REmpty | RVar _ -> String.Set.empty
  | RExtend (name, presence, _, _, rest) ->
    (match Type.canonical_presence presence with
     | Present | PVar _ -> Set.add (listed_entries rest) name
     | Absent -> listed_entries rest)

(* Runs the toplevel splice [code], then infers the code it generates here, with
   its explicit substitutions bound by lets around it. An explicit substitution
   of a name not [listed] in the code's context must be of a free variable of
   the code generated. *)
and expand
  t
  (scope : Scope.t)
  ~at
  ~code
  ~subs
  ~bindings:modes
  ~listed
  ~name_locs
  ~ty
  ~loc
  =
  let program = scope.program in
  let bindings, entry =
    match (subs : Typed.Expr.t Typed.Expr.Subs.t) with
    | Implicit providers ->
      ( []
      , fun name ->
          Map.find providers name
          |> Option.map ~f:(fun binder -> Typed.Var_ref.Bound binder) )
    | Explicit subs ->
      (* As [Interpreter_intf.S.splice] does: a variable is substituted as
         itself, so that the code may assign it. *)
      let subs =
        List.map subs ~f:(fun (name, (sub : Typed.Expr.t)) ->
          match sub.desc with
          | Var ((Bound _ | Persistent _) as var_ref) -> name, First var_ref
          | _ ->
            ( name
            , Second
                ( Program_env.bind
                    program
                    (Name (Located.create ~loc name))
                    sub.ty
                    ~level:scope.level
                , sub ) ))
      in
      let entries =
        String.Map.of_alist_reduce
          (List.map subs ~f:(fun (name, sub) ->
             ( name
             , match sub with
               | First var_ref -> var_ref
               | Second ((ident : Typed.Ident.t), _) ->
                 Typed.Var_ref.Bound (Local ident) )))
          ~f:(fun first _ -> first)
      in
      ( List.filter_map subs ~f:(fun (_, sub) -> Either.Second.to_option sub)
      , Map.find entries )
  in
  let used = String.Hash_set.create () in
  let entry name =
    Hash_set.add used name;
    entry name
  in
  let%bind.Result generated =
    Program_env.at_compile_time
      ~loc
      program
      (fun ~reflect ->
        Interpreter.splice
          program.interpreter
          ~code
          ~level:(scope.level - 1)
          ~entry
          ~reflect)
      ~error:(fun error ->
        Typecheck_error.create ~loc (Splice_evaluation_failed { code; error }))
  in
  let%map.Result () =
    match subs with
    | Implicit _ -> Ok ()
    | Explicit subs ->
      List.find subs ~f:(fun (name, _) ->
        not (Set.mem listed name || Hash_set.mem used name))
      |> Option.value_map
           ~default:(Ok ())
           ~f:(fun (name, (sub : Typed.Expr.t)) ->
             error
               ~loc:(Map.find name_locs name |> Option.value ~default:sub.loc)
               (Unused_substitution name))
  in
  let expansion =
    with_monomorphic_bindings
      scope
      (List.map
         bindings
         ~f:(fun ((ident : Typed.Ident.t), (sub : Typed.Expr.t)) ->
           ident, sub.ty, Map.find_exn modes ident.name))
      ~f:(fun () ->
        (* Each expansion's annotations name their own type variables. *)
        let scope =
          { scope with
            annotation_vars =
              Type_env.Annotation_vars.create
                scope.program.type_env
                ~rank:scope.rank
          }
        in
        check_expr t scope ~at ~expr:(Source.Expr.Generated generated) ~ty)
  in
  List.fold_right bindings ~init:expansion ~f:(fun (ident, body) in_ ->
    { Typed.Expr.desc =
        Let
          { ident
          ; args = []
          ; annots = []
          ; result_annot = None
          ; mode = None
          ; generalized = false
          ; body
          ; in_
          }
    ; ty = in_.ty
    ; loc
    ; made = Typed.Expr.Made.none
    })

and labelled_types t scope ~parts fields ~expected ~hint =
  Poly_row.report_duplicate_labels scope fields;
  List.map fields ~f:(fun ((label : Syntax.Name.t), expr) ->
    ( label.txt
    , Mode_rules.Parts.part parts (Label label.txt) Modality.id (fun at ->
        expr_type
          t
          scope
          ~at
          ~expr
          ?expected:(Pattern_check.row_entry expected `Record ~label:label.txt)
          ~hint) ))

(* Checks [expr] has type [ty], [expected] (by default [ty]) being what it is
   known to be. *)
and check_expr ?expected ?hint ?applied t scope ~at ~expr ~ty =
  let expr =
    expr_type
      ?applied
      t
      scope
      ~at
      ~expr
      ~expected:(Option.value expected ~default:ty)
      ?hint
  in
  if Scope.subtypes t scope ~lower:expr.ty ~upper:ty ~loc:expr.loc
  then expr
  else
    { desc = Error [ expr ]
    ; ty = Type.error
    ; loc = expr.loc
    ; made = Typed.Expr.Made.none
    }

(* Returns the binder each entry that may be captured is captured by: entries
   bound in the quote may be captured by their binders if [may_capture]. *)
and substitute_from_scope t (scope : Scope.t) ~at ~context ~loc ~may_capture =
  let rec go context ~seen ~captures =
    match Type.canonical_row context with
    | REmpty -> Ok captures
    | RExtend (name, presence, assigned, ty, rest) ->
      let%bind.Result captured =
        match Type.canonical_presence presence with
        | _ when Set.mem seen name -> Ok None
        | Absent -> Ok None
        | Present | PVar _ ->
          substitute_entry
            t
            scope
            ~at
            ~name
            ~presence
            ~assigned
            ~ty
            ~loc
            ~may_capture
      in
      let captures =
        Option.fold captured ~init:captures ~f:(fun captures binder ->
          (name, binder) :: captures)
      in
      go rest ~seen:(Set.add seen name) ~captures
    | RVar _ as tail ->
      (match scope.quote with
       | None ->
         let%map.Result () =
           Unify.unify_row t ~row1:tail ~row2:REmpty |> lift ~loc
         in
         captures
       | Some quote ->
         let%map.Result captured =
           substitute_tail t scope ~at quote ~tail ~seen ~loc ~may_capture
         in
         captured @ captures)
  in
  let%map.Result captures = go context ~seen:String.Set.empty ~captures:[] in
  String.Map.of_alist_exn captures

and substitute_entry
  t
  (scope : Scope.t)
  ~at
  ~name
  ~presence
  ~assigned
  ~ty
  ~loc
  ~may_capture
  =
  let%bind.Result resolution =
    match Env.lookup scope.locals name ~level:scope.level, scope.quote with
    | Bound entry, _ ->
      Ok (Resolution.of_entry ~spliced:true scope ~name (Some entry))
    | Forgotten, None -> Ok (Resolution.Unusable (Forgotten name))
    (* Within a quote, a name no local binds stays a free variable of it. *)
    | (Forgotten | Unbound), Some _ ->
      Ok (Resolution.of_entry ~spliced:true scope ~name None)
    | Unbound, None ->
      (* The code is checked and runs now, so if it may use [name], it doesn't.
         Providing it anyway could add a dependency on its toplevel, and would
         depend on whether that is checked yet. *)
      let%map.Result entry =
        match
          ( Program_env.find_toplevel scope.program name
          , Type.canonical_presence presence )
        with
        | Some toplevel, Present ->
          toplevel_entry t scope toplevel ~loc |> Result.map ~f:Option.some
        | None, _ | Some _, (Absent | PVar _) -> Ok None
      in
      Resolution.of_entry ~spliced:true scope ~name entry
  in
  match resolution with
  | By_symbol _ ->
    raise_s [%message "Spliced code's free variable by symbol" name]
  | Unusable kind ->
    (* Code that may use [name] can't, so doesn't. *)
    let kind : Typecheck_error.Kind.t =
      match kind, scope.quote with
      | Not_in_scope _, None ->
        (match Unusable_in_quotes.find ~entry:ty ~name with
         | Some unusable -> Free_in_spliced_code { name; unusable }
         | None -> kind)
      | _ -> kind
    in
    let%map.Result () =
      Unify.make_absent t presence ~error:(Typecheck_error.create ~loc kind)
    in
    None
  | Local entry when may_capture name entry.binder ->
    (* Whether the code uses it may not be known yet: assume it does. *)
    Uses.use_entry scope ~at entry ~name ~loc;
    let%bind.Result () =
      provides_assigned
        t
        ~name
        ~assigned
        ~mutable_:(binder_is_mutable scope entry.binder)
        ~loc
    in
    let%map.Result () =
      Unify.unify_if_present
        t
        ~presence
        ~ty1:ty
        ~ty2:(Uses.instantiate_entry t scope entry ~loc)
      |> lift ~loc
    in
    Some entry.binder
  | Local _ ->
    let%map.Result () =
      Unify.unify_row
        t
        ~row1:(RExtend (name, presence, assigned, ty, REmpty))
        ~row2:(RExtend (name, Absent, assigned, ty, REmpty))
      |> lift ~loc
    in
    None
  | Outside_quote { quote; _ } ->
    (match Type.canonical_presence presence with
     | Absent -> ()
     | Present | PVar _ ->
       Uses.use_free
         scope
         ~at
         ~why:(sprintf "%s is a free variable of the quote" name)
         ~loc);
    let%map.Result () =
      Unify.constrain_row
        t
        ~row:(RExtend (name, presence, assigned, ty, REmpty))
        ~bound:quote.context
        ~shadowed:String.Set.empty
      |> lift ~loc
    in
    None

(* The code's remaining free variables are not known yet, so give the ones that
   would be captured here explicit entries, absent unless [may_capture]. The
   rest must be free variables of the quote. Returns the binders of the ones
   that may be captured. *)
and substitute_tail
  t
  scope
  ~at
  (quote : Scope.quote)
  ~tail
  ~seen
  ~loc
  ~may_capture
  =
  let captured =
    Env.visible scope.locals ~level:scope.level
    |> List.filter_map ~f:(fun (name, entry) ->
      if Set.mem seen name
      then None
      else (
        match Resolution.of_entry ~spliced:true scope ~name (Some entry) with
        | Local entry when may_capture name entry.binder ->
          (* Whether the code uses it is not known: assume it does. *)
          Uses.use_entry scope ~at entry ~name ~loc;
          Some
            ( name
            , Scope.fresh_presence t scope
            , may_be_assigned t scope entry.binder
            , Uses.instantiate_entry t scope entry ~loc
            , Some entry.binder )
        | Local _ | Unusable _ ->
          Some (name, Absent, Absent, Scope.fresh_ty t scope, None)
        | Outside_quote _ -> None
        | By_symbol _ ->
          raise_s [%message "Spliced code's free variable by symbol" name]))
  in
  let rest = Scope.fresh_row t scope in
  Uses.use_free
    scope
    ~at
    ~why:"the spliced code may use free variables of the quote"
    ~loc;
  let%bind.Result () =
    Unify.unify_row
      t
      ~row1:tail
      ~row2:
        (List.fold_right
           captured
           ~init:rest
           ~f:(fun (name, presence, assigned, ty, _) rest ->
             Type.RExtend (name, presence, assigned, ty, rest)))
    |> lift ~loc
  in
  let%map.Result () =
    Unify.constrain_row
      t
      ~row:rest
      ~bound:quote.context
      ~shadowed:
        (List.fold captured ~init:seen ~f:(fun seen (name, _, _, _, _) ->
           Set.add seen name))
    |> lift ~loc
  in
  List.filter_map captured ~f:(fun (name, _, _, _, binder) ->
    Option.map binder ~f:(fun binder -> name, binder))

and substitute_explicitly t scope ~at ~context ~subs ~loc =
  let%bind.Result () =
    Duplicates.check (List.map subs ~f:fst) ~error:(fun name ->
      Duplicate_substitution name)
  in
  let rec lacks (row : Type.row) name =
    match Type.canonical_row row with
    | REmpty -> true
    | RVar _ -> false
    | RExtend (entry, presence, _, _, rest) ->
      if String.equal entry name
      then (
        match Type.canonical_presence presence with
        | Absent -> true
        | Present | PVar _ -> false)
      else lacks rest name
  in
  let%bind.Result () =
    List.find_map subs ~f:(fun ({ txt = name; loc }, _) ->
      Option.some_if (lacks context name) (loc, name))
    |> Option.value_map ~default:(Ok ()) ~f:(fun (loc, name) ->
      error ~loc (Unused_substitution name))
  in
  let bindings = ref String.Map.empty in
  let%bind.Result provided =
    List.fold_result
      subs
      ~init:[]
      ~f:(fun acc ({ txt = name; loc = name_loc }, expr) ->
        let%map.Result expr =
          match expr with
          | None ->
            let%map.Result ty, var_ref =
              var_type
                t
                scope
                ~at:(Mode_rules.Splice.captured at)
                ~path:(Path (Syntax.Path.of_string name))
                ~loc:name_loc
            in
            { Typed.Expr.desc = Var var_ref
            ; ty
            ; loc = name_loc
            ; made = Typed.Expr.Made.none
            }
          | Some expr ->
            let expr, binding =
              Mode_rules.Splice.substitution
                (Scope.modes scope)
                at
                ~loc:name_loc
                (fun at -> expr_type t scope ~at ~expr)
            in
            bindings := Map.set !bindings ~key:name ~data:binding;
            Ok expr
        in
        (name, expr) :: acc)
  in
  (* Toplevels, free by canonical name, which no sub can name. *)
  let rec toplevels (row : Type.row) =
    match Type.canonical_row row with
    | RExtend (name, presence, _, _, rest) ->
      (match Type.canonical_presence presence with
       | (Present | PVar _) when String.mem name '.' -> name :: toplevels rest
       | Present | PVar _ | Absent -> toplevels rest)
    | REmpty | RVar _ -> []
  in
  let%bind.Result provided =
    List.fold_result (toplevels context) ~init:provided ~f:(fun acc name ->
      let%map.Result ty, var_ref =
        resolved_value_type
          t
          scope
          ~at:(Mode_rules.Splice.captured at)
          name
          ~loc
      in
      ( name
      , { Typed.Expr.desc = Var var_ref; ty; loc; made = Typed.Expr.Made.none }
      )
      :: acc)
  in
  (* Only a variable that is mutable can be assigned, as itself. *)
  let assigned (expr : Typed.Expr.t) : Type.presence =
    match expr.desc with
    | Var (Bound binder | Persistent binder) -> may_be_assigned t scope binder
    | _ -> Absent
  in
  let row =
    List.fold provided ~init:Type.REmpty ~f:(fun rest (name, expr) ->
      Type.RExtend
        (name, Scope.fresh_presence t scope, assigned expr, expr.ty, rest))
  in
  let%map.Result () =
    Unify.unify_row t ~row1:context ~row2:row
    |> Result.map_error ~f:(fun (error : Unification_error.t) ->
      match error with
      | Row_mismatch (_, _, Assigned name) ->
        Typecheck_error.create ~loc (Spliced_code_assigns_immutable name)
      | error -> Typecheck_error.unification_error ~loc error)
  in
  List.rev provided, !bindings

and binder_is_mutable (scope : Scope.t) (binder : Typed.Binder.t) =
  let (Local ident | Toplevel ident) = binder in
  Program_env.is_mutable scope.program ident

(* The assignedness of an entry provided by [binder]: code may assign it only if
   it is mutable. *)
and may_be_assigned t scope binder : Type.presence =
  if binder_is_mutable scope binder
  then Scope.fresh_presence t scope
  else Absent

and provides_assigned t ~name ~assigned ~mutable_ ~loc =
  if mutable_
  then Ok ()
  else
    Unify.make_absent
      t
      assigned
      ~error:(Typecheck_error.create ~loc (Spliced_code_assigns_immutable name))
;;

module Checked = struct
  type t =
    { signature : Signature.t
    ; program : Typed.Program.t
    ; expansions : Expansion.t list
    ; references : (Loc.t * Name_reference.t) list option
    }
end

module Failed = struct
  type t =
    { errors : Typecheck_error.t list
    ; partial : Checked.t
    }
end

(* Checks the code of [scope] (see [Uses.code_checked]). *)
let checking_code t scope f =
  (* Even if [f] raises: the uses it handed on are not left generalized. *)
  Exn.protect ~f ~finally:(fun () -> Uses.code_checked t scope)
;;

let toplevel_scope ?generated t program module_ ~level ~in_expect_test =
  let rank = 1 in
  let scc =
    Scc_frame.create
      (Fresh.id t)
      (Code { within = Stack.top program.Program_env.checking })
  in
  { Scope.program
  ; level
  ; rank
  ; locals = Env.create ()
  ; quote = None
  ; module_
  ; scc
  ; annotation_vars = Type_env.Annotation_vars.create program.type_env ~rank
  ; in_expect_test
  ; generated
  ; syntax_splice = None
  ; locks = []
  ; params = []
  ; allocations = None
  ; owner = None
  ; uses = Uses.of_code program scc
  }
;;

let check_expect_test t program module_ ~name ~body ~loc ~generated =
  let scope =
    toplevel_scope ?generated t program module_ ~level:0 ~in_expect_test:true
  in
  let at = Mode_rules.Expected.fresh (Scope.modes scope) in
  let body =
    checking_code t scope (fun () ->
      check_expr t scope ~at ~expr:(Source.Expr.Syntax body) ~ty:Type.unit)
  in
  { Typed.Expect_test.name; body; loc; generated = Option.is_some generated }
;;

let check_bench t program module_ ~name ~param ~body ~loc ~generated =
  let scope =
    toplevel_scope ?generated t program module_ ~level:0 ~in_expect_test:false
  in
  let at = Mode_rules.Expected.fresh (Scope.modes scope) in
  checking_code t scope
  @@ fun () ->
  match (param : Syntax.Bench_param.t option) with
  | None ->
    let body =
      check_expr
        t
        scope
        ~at
        ~expr:(Source.Expr.Syntax body)
        ~ty:(Scope.fresh_ty t scope)
    in
    { Typed.Bench.name
    ; param = None
    ; body
    ; loc
    ; generated = Option.is_some generated
    }
  | Some { name = param; args } ->
    let fn =
      Function_type.create
        ~merge:false
        t
        scope
        ~args:[ Source.Binder.Name param, Syntax.Annot.none ]
        ~annot:None
        ~res_mode:None
        ~body:(Source.Expr.Syntax body)
        ~loc
    in
    let { Function_type.args = params; res_ty; _ } = fn in
    let ident, param_ty = List.hd_exn params in
    let args =
      List.map args ~f:(fun arg ->
        check_expr t scope ~at ~expr:(Source.Expr.Syntax arg) ~ty:param_ty)
    in
    let ( (body, (_ : Typed.Allocation.Id.t list))
        , (_ : Typed.Allocation.Id.t option) )
      =
      Mode_rules.Alloc.closure (Scope.modes scope) at ~loc (fun closure ->
        check_body t scope ~closure ~fn ~loc)
    in
    { Typed.Bench.name
    ; param = Some (ident, args)
    ; body
    ; loc
    ; generated = Option.is_some generated
    }
;;

let declarations_type =
  Type.TCon
    ( Prelude_path.canonical "list"
    , [ ATy (TCon (Prelude_path.canonical "Stag.Toplevel.t", [])) ] )
;;

(* The expansion of [splice], if it has one, and whether it is [generated]. *)
let expansion_status splice generated =
  let toplevel = Module_scope.Splice.toplevel splice in
  match toplevel.txt with
  | Include (_, Some written) | Module_splice (_, _, Some written) ->
    Some
      ( written
      , Expansion.status
          ~written:written.txt
          generated
          ~column:toplevel.loc.start.#col
          ~path:(fun namespace ~modules ~locals path ->
            Module_scope.path_from_splice
              splice
              generated
              (match namespace with
               | Type -> Type
               | Constructor -> Constructor
               | Field -> Field
               | Value -> Value)
              ~modules
              ~locals
              path) )
  | _ -> None
;;

let expansions root =
  Module_scope.splices root
  |> List.filter_map ~f:(fun splice ->
    let%bind.Option generated = Module_scope.Splice.generated splice in
    let%map.Option expansion, status = expansion_status splice generated in
    { Expansion.loc = Module_scope.Splice.loc splice; expansion; status })
  |> List.sort ~compare:(fun (a : Expansion.t) b -> Loc.compare a.loc b.loc)
;;

let stag_type name = Type.TCon (Prelude_path.canonical name, [])

(* Checks [expr], written in [scope], and runs it at compile time, unless it did
   not check. *)
let run_at_compile_time t (program : Program_env.t) scope ~expr ~ty ~loc ~error =
  let reported = program.reported in
  let expr =
    checking_code t scope (fun () ->
      check_expr
        t
        scope
        ~at:(Mode_rules.Expected.fresh (Scope.modes scope))
        ~expr:(Source.Expr.Syntax expr)
        ~ty)
  in
  Scc_frame.hand_over scope.Scope.scc;
  Var_layout.check_waiting (Type_env.layouts program.type_env);
  if program.reported > reported
  then Error Typecheck_error.already_reported
  else (
    let%map.Result value =
      Program_env.at_compile_time
        ~loc
        program
        (fun ~reflect ->
          Interpreter.run program.interpreter expr ~level:(-1) ~reflect)
        ~error
    in
    Queue.enqueue program.declaration_splices expr;
    value)
;;

let compile_time_scope t program module_ =
  toplevel_scope t program module_ ~level:(-1) ~in_expect_test:false
;;

(* The type a type splice [.~expr] at [loc] gives. Not abandoned like a splice
   of declarations: a toplevel it uses that is being checked is a cycle, unless
   an enclosing splice of declarations is abandoned. *)
let evaluate_type_splice t program module_ expr ~loc =
  Result.map_error ~f:(Module_scope.within module_)
  @@
  let%bind.Result value =
    run_at_compile_time
      t
      program
      (compile_time_scope t program module_)
      ~expr
      ~ty:(stag_type "Stag.Type_expr.t")
      ~loc
      ~error:(fun error ->
        Typecheck_error.create ~loc (Type_splice_failed error))
  in
  Interpreter.Value.to_syntax value Generated_syntax.Type_expr ~loc
  |> Result.map_error ~f:(fun error ->
    Typecheck_error.create ~loc (Malformed_type_splice error))
;;

(* Runs [f] in [scope], for [splice]: if its code uses a toplevel being checked
   when it began, it is abandoned, to be forced again once that is finished. *)
let forcing (program : Program_env.t) (scope : Scope.t) splice f
  : (_, Module_scope.Force_error.t) Result.t
  =
  let unfinished = Stack.length program.unfinished in
  Stack.push program.splice_starts scope.scc.lowlink;
  match f () with
  | exception Program_env.Uses_unfinished ->
    ignore (Stack.pop_exn program.splice_starts : int);
    abandon program scope.scc ~unfinished;
    Error Uses_unfinished
  | result ->
    ignore (Stack.pop_exn program.splice_starts : int);
    Result.map_error result ~f:(fun error : Module_scope.Force_error.t ->
      Failed (Module_scope.within (Module_scope.Splice.module_ splice) error))
;;

(* The type declaration [type t = .~e] defined by what [e] evaluates to. *)
let define_type t (program : Program_env.t) splice =
  let loc = Module_scope.Splice.loc splice in
  let scope =
    compile_time_scope t program (Module_scope.Splice.module_ splice)
  in
  forcing program scope splice (fun () ->
    let%bind.Result value =
      run_at_compile_time
        t
        program
        scope
        ~expr:(Module_scope.Splice.expr splice)
        ~ty:(stag_type "Stag.Type_decl.Kind.t")
        ~loc
        ~error:(fun error ->
          Typecheck_error.create ~loc (Type_splice_failed error))
    in
    let%map.Result kind =
      Interpreter.Value.to_syntax value Generated_syntax.Type_decl_kind ~loc
      |> Result.map_error ~f:(fun error ->
        Typecheck_error.create ~loc (Malformed_type_splice error))
    in
    let toplevel = Module_scope.Splice.toplevel splice in
    match toplevel.txt with
    | Type decl ->
      [ { toplevel with txt = Syntax.Toplevel.Type { decl with kind } } ]
    | _ -> raise_s [%message "Typecheck.define_type: not a type declaration"])
;;

(* Checks a declaration splice's expression in the module it is written in, and
   runs it at compile time, to the declarations it generates. *)
let generate t (program : Program_env.t) splice
  : (Syntax.Toplevel.t list, Module_scope.Force_error.t) Result.t
  =
  if Module_scope.Splice.defines_type splice
  then define_type t program splice
  else (
    let loc = Module_scope.Splice.loc splice in
    let scope =
      compile_time_scope t program (Module_scope.Splice.module_ splice)
    in
    forcing program scope splice (fun () ->
      let%bind.Result value =
        run_at_compile_time
          t
          program
          scope
          ~expr:(Module_scope.Splice.expr splice)
          ~ty:declarations_type
          ~loc
          ~error:(fun error ->
            Typecheck_error.create ~loc (Declaration_splice_failed error))
      in
      let%map.Result generated =
        Interpreter.Value.to_syntax value Generated_syntax.Program ~loc
        |> Result.map_error ~f:(fun error ->
          Typecheck_error.create ~loc (Malformed_declarations error))
      in
      match expansion_status splice generated with
      | Some (written, Up_to_date) ->
        Option.value
          (Relocate.toplevels ~written:written.txt generated)
          ~default:generated
      | Some (_, Stale _) | None -> generated))
;;

(* Declares the types and toplevels of declarations as they are added. *)
let declare
  (program : Program_env.t)
  ~in_prelude
  module_
  (toplevel : Syntax.Toplevel.t)
  ~generated_by
  =
  let generated =
    Option.map generated_by ~f:(fun splice -> Generated.create splice toplevel)
  in
  let wrap_error error =
    Module_scope.within
      module_
      (match generated with
       | None -> error
       | Some generated -> Generated.wrap_error generated error)
  in
  let add_toplevel
    (kind : Toplevel_binding.Kind.t)
    (name : Syntax.Name.t)
    annot
    expr
    =
    let name = Module_scope.canonical_name module_ name.txt in
    let toplevel =
      lazy
        (let ident = Program_env.fresh_ident program name in
         (match kind with
          | Mut -> Hash_set.add program.mutable_idents ident
          | Value | Function _ | External _ -> ());
         Toplevel_binding.create
           ~ident
           ~module_
           ~kind
           ~annot:annot.Syntax.Annot.ty
           ~mode:annot.mode
           ~expr
           ~generated)
    in
    if not in_prelude then ignore (force toplevel : Toplevel_binding.t);
    Hashtbl.add_exn program.toplevels ~key:name ~data:toplevel
  in
  match toplevel.txt, generated_by with
  | Type decl, Some splice when Module_scope.Splice.defines_type splice ->
    Type_env.set_definition
      program.type_env
      (Module_scope.canonical_name module_ decl.name.txt)
      decl.kind
      ~loc:(Module_scope.Splice.loc splice)
      ~wrap_error:(fun error ->
        match generated with
        | None -> error
        | Some generated -> Generated.wrap_error generated error)
  | Type decl, _ ->
    Type_env.declare
      program.type_env
      module_
      decl
      ~lazily:in_prelude
      ~wrap_error
  | Exception (name, args), _ ->
    Type_env.declare_exception
      program.type_env
      module_
      name
      args
      ~malformed:false
      ~wrap_error
  | Malformed_exception (name, args), _ ->
    Type_env.declare_exception
      program.type_env
      module_
      name
      args
      ~malformed:true
      ~wrap_error
  | Let (name, args, annot, expr), _ ->
    let kind : Toplevel_binding.Kind.t =
      match Nonempty_list.of_list args with
      | None -> Value
      | Some args -> Function args
    in
    add_toplevel kind name annot expr
  | Let_mut (name, annot, expr), _ -> add_toplevel Mut name annot expr
  | External { name; ty; symbol }, _ ->
    let at_compile_time =
      Compile_time_c_calls.policy
        program.compile_time_c_calls
        ~external_:(Module_scope.canonical_name module_ name.txt)
    in
    add_toplevel
      (External { ty; symbol; at_compile_time })
      name
      Syntax.Annot.none
      (Located.create ~loc:toplevel.loc (Syntax.Expr.Lit Unit))
  | (Module _ | Module_splice _ | Include _ | Expect_test _ | Bench _), _ -> ()
;;

module Deferred = struct
  type t =
    | Expect_test of Syntax.Name.t * Syntax.Expr.t
    | Bench of Syntax.Name.t * Syntax.Bench_param.t option * Syntax.Expr.t
end

(* Checks the declarations of [module_] and its modules not yet checked on
   demand, depth first in order, returning the expect tests and benches to check
   after. *)
let check_declarations t (program : Program_env.t) module_ =
  let deferred = Queue.create () in
  let report_if_error = function
    | Ok _ -> ()
    | Error error -> Program_env.report program error
  in
  Module_scope.iter_declarations
    module_
    ~f:(fun module_ (toplevel : Syntax.Toplevel.t) ~generated_by ->
      let defer (kind : Deferred.t) =
        let generated =
          Option.map generated_by ~f:(fun splice ->
            Generated.create splice toplevel)
        in
        Queue.enqueue deferred (module_, kind, toplevel.loc, generated)
      in
      match toplevel.txt with
      | Type decl ->
        (match decl.kind with
         | Malformed _ -> Program_env.mark_syntax_errors program
         | Variant _ | Record _ | Alias _ | Abstract | Splice _ -> ());
        Type_env.check
          program.type_env
          (Module_scope.canonical_name module_ decl.name.txt)
        |> report_if_error
      | Malformed_exception _ -> Program_env.mark_syntax_errors program
      | Exception (name, _) ->
        Type_env.check_exception
          program.type_env
          (Module_scope.canonical_name module_ name.txt)
        |> report_if_error
      | Let (name, _, _, _) | Let_mut (name, _, _) | External { name; _ } ->
        let toplevel =
          Program_env.toplevel
            program
            (Module_scope.canonical_name module_ name.txt)
        in
        (match toplevel.state with
         | Unchecked -> ignore (check_toplevel t program toplevel : Scc_frame.t)
         | In_progress _ | Done _ -> ())
      | Expect_test { name; body } -> defer (Deferred.Expect_test (name, body))
      | Bench { name; param; body } ->
        defer (Deferred.Bench (name, param, body))
      | Module _ | Module_splice _ | Include _ -> ());
  Queue.to_list deferred
;;

let builtin_names =
  List.map Type_env.builtins ~f:(fun (decl : Syntax.Type_decl.t) ->
    decl.name.txt)
;;

let no_splices splice : (_, Module_scope.Force_error.t) Result.t =
  Error
    (Failed
       (Typecheck_error.create
          ~loc:(Module_scope.Splice.loc splice)
          (Declaration_splice_failed
             (Error.of_string "the prelude cannot have declaration splices"))))
;;

let rec items (program_env : Program_env.t) module_ =
  List.filter_map
    (Module_scope.declarations module_)
    ~f:(fun (toplevel : Syntax.Toplevel.t) : Typed.Program.Item.t option ->
      match toplevel.txt with
      | Let ({ txt = name; loc = _ }, _, _, _)
      | Let_mut ({ txt = name; loc = _ }, _, _)
      | External { name = { txt = name; loc = _ }; _ } ->
        Program_env.find_toplevel
          program_env
          (Module_scope.canonical_name module_ name)
        |> Option.map ~f:(fun (toplevel : Toplevel_binding.t) ->
          Typed.Program.Item.Toplevel toplevel.ident)
      | Module ({ txt = name; loc = _ }, _)
      | Module_splice ({ txt = name; loc = _ }, _, _) ->
        Some
          (Module (name, items program_env (Module_scope.enter module_ name)))
      | Type _
      | Exception _
      | Malformed_exception _
      | Expect_test _
      | Bench _
      | Include _ -> None)
;;

let invalid_prelude error =
  raise_s [%message "The prelude is invalid" (error : Typecheck_error.t)]
;;

(* The prelude, outside the program's root module, sees the builtin types as the
   program does. *)
let add_prelude (program : Program_env.t) toplevels =
  let builtins =
    Module_scope.create_root
      ~types:builtin_names
      ~declare:(fun _ _ ~generated_by:_ -> ())
      ~force:no_splices
      ~report:invalid_prelude
      ()
  in
  List.iter Type_env.builtins ~f:(Type_env.declare program.type_env builtins);
  let prelude =
    Module_scope.create_root
      ~enclosing:builtins
      ~path:[ Prelude_path.root ]
      ~types:[]
      ~declare:(declare program ~in_prelude:true)
      ~force:no_splices
      ~report:(Program_env.report program)
      ()
  in
  Module_scope.add prelude toplevels;
  prelude
;;

(* Without [complete], what did not check is left out. *)
let rec signature (program_env : Program_env.t) module_ ~complete =
  let canonical_name = Module_scope.canonical_name module_ in
  let missing what sexp =
    if complete
    then raise_s [%message "typecheck: not checked" what (sexp : Sexp.t)]
    else None
  in
  List.filter_map
    (Module_scope.declarations module_)
    ~f:(fun (toplevel : Syntax.Toplevel.t) : Signature.Item.t option ->
      match toplevel.txt with
      | Expect_test _ | Bench _ | Include _ -> None
      | Let ({ txt = name; loc = _ }, _, _, _)
      | Let_mut ({ txt = name; loc = _ }, _, _)
      | External { name = { txt = name; loc = _ }; _ } ->
        let toplevel = Program_env.toplevel program_env (canonical_name name) in
        (match toplevel.state with
         | Done { entry = { scheme; mode; held; _ }; _ } ->
           Some
             (Value
                { name
                ; scheme
                ; stage = (Mode.Var.lower (mode :> Mode.Var.t)).stage
                ; provenance =
                    (Mode.Var.lower
                       (Option.value_map
                          held
                          ~default:(mode :> Mode.Var.t)
                          ~f:(fun held -> (held :> Mode.Var.t))))
                      .provenance
                })
         | Unchecked | In_progress _ ->
           missing "toplevel" [%sexp (toplevel : Toplevel_binding.t)])
      | Type decl ->
        (match
           Type_env.check program_env.type_env (canonical_name decl.name.txt)
         with
         | Ok decl -> Some (Type decl)
         (* [Malformed], whose syntax error is the parser's. *)
         | Error error when Typecheck_error.is_already_reported error -> None
         | Error error -> missing "type" [%sexp (error : Typecheck_error.t)])
      | Exception ({ txt = name; loc = _ }, _)
      | Malformed_exception ({ txt = name; loc = _ }, _) ->
        (match
           Type_env.check_exception program_env.type_env (canonical_name name)
         with
         | Ok { args; _ } ->
           Some
             (Exception
                { name
                ; args
                ; modalities = List.map args ~f:(fun _ -> Member_modality.Id)
                })
         | Error error when Typecheck_error.is_already_reported error -> None
         | Error error ->
           missing "exception" [%sexp (error : Typecheck_error.t)])
      | Module ({ txt = name; loc = _ }, _)
      | Module_splice ({ txt = name; loc = _ }, _, _) ->
        Some
          (Module
             { name
             ; items =
                 signature
                   program_env
                   (Module_scope.enter module_ name)
                   ~complete
             }))
;;

let program_env
  t
  ~stamps
  ~modes
  ~const_toplevels
  ~compile_time_c_calls
  ?record_references
  interpreter
  =
  Program_env.create
    ~type_env:(fun program_env ->
      Type_env.create
        ?record_references
        t
        ~report:(fun error -> Program_env.report (force program_env) error)
        ~evaluate_splice:(fun module_ expr ~loc ->
          evaluate_type_splice t (force program_env) module_ expr ~loc))
    ~stamps
    ~modes
    ~const_toplevels
    ~compile_time_c_calls
    interpreter
;;

(* The program's root module, within the prelude. *)
let root t program_env ~enclosing =
  Module_scope.create_root
    ~enclosing
    ~types:builtin_names
    ~declare:(declare program_env ~in_prelude:false)
    ~force:(generate t program_env)
    ~report:(Program_env.report program_env)
    ()
;;

(* Each once, by location, then in the order reported. A splice blocking one
   name its code uses blocks the others too, and doesn't run, so the first is
   enough. *)
let sorted_errors errors ~root =
  let seen = Hash_set.Poly.create () in
  let blocking = Hash_set.Poly.create () in
  List.rev_map errors ~f:(Module_scope.within root)
  |> List.stable_sort ~compare:(fun (a : Typecheck_error.t) b ->
    Loc.compare a.loc b.loc)
  |> List.filter ~f:(fun (error : Typecheck_error.t) ->
    let blocked_again =
      match error.kind with
      | Maybe_generated_by_running_include { include_ = splice; error = _ }
      | Declaration_splice_cycle { splice; name = _; module_ = _ } ->
        Hash_set.strict_add blocking splice
        |> Hash_set.Ok_or_duplicate.is_duplicate
      | _ -> false
    in
    (not blocked_again)
    && Hash_set.strict_add seen (error.loc, Typecheck_error.message error)
       |> Hash_set.Ok_or_duplicate.is_ok)
;;

let prelude_and_root t program_env =
  let prelude = add_prelude program_env (force t.prelude) in
  prelude, root t.fresh program_env ~enclosing:prelude
;;

let prelude_signature ({ fresh; stamps; interpreter; _ } as t) =
  let program_env =
    program_env
      fresh
      ~stamps
      ~modes:t.modes
      ~const_toplevels:t.const_toplevels
      ~compile_time_c_calls:t.compile_time_c_calls
      interpreter
  in
  let prelude, (_ : Module_scope.t) = prelude_and_root t program_env in
  let (_ : _ list) = check_declarations fresh program_env prelude in
  match Program_env.errors program_env with
  | [] -> signature program_env prelude ~complete:true
  | error :: _ -> invalid_prelude error
;;

(* Checks [module_]'s declarations, then its expect tests and benches. *)
let check_module t (program_env : Program_env.t) module_
  : (Checked.t, Failed.t) Result.t
  =
  let deferred = check_declarations t program_env module_ in
  let checked =
    List.map deferred ~f:(fun (module_, (kind : Deferred.t), loc, generated) ->
      match kind with
      | Expect_test (name, body) ->
        Either.First
          (check_expect_test t program_env module_ ~name ~body ~loc ~generated)
      | Bench (name, param, body) ->
        Either.Second
          (check_bench t program_env module_ ~name ~param ~body ~loc ~generated))
  in
  let expect_tests, benches = List.partition_map checked ~f:Fn.id in
  let program =
    { Typed.Program.sccs = Queue.to_list program_env.sccs
    ; items = items program_env module_
    ; expect_tests
    ; benches
    ; declaration_splices = Queue.to_list program_env.declaration_splices
    ; ident_info = program_env.ident_info
    ; allocations = Queue.to_list program_env.allocations
    }
  in
  let errors =
    match sorted_errors (Program_env.errors program_env) ~root:module_ with
    | [] when (not program_env.syntax_errors) && Typed.Program.has_error program
      ->
      (* Something failed to check, so the program must not run, though its
         error was lost. *)
      [ Module_scope.within
          module_
          (Typecheck_error.create
             ~loc:Loc.none
             (Internal_error
                (Error.of_string
                   "code failed to check but no error was reported")))
      ]
    | errors -> errors
  in
  let checked =
    { Checked.signature =
        signature program_env module_ ~complete:(List.is_empty errors)
    ; program
    ; expansions = expansions module_
    ; references = Type_env.references program_env.type_env
    }
  in
  match errors with
  | [] -> Ok checked
  | errors -> Error { Failed.errors; partial = checked }
;;

let typecheck ({ fresh; stamps; interpreter; _ } as t) ~program =
  let program_env =
    program_env
      fresh
      ~stamps
      ~modes:t.modes
      ~const_toplevels:t.const_toplevels
      ~compile_time_c_calls:t.compile_time_c_calls
      ~record_references:t.record_references
      interpreter
  in
  let (_ : Module_scope.t), root = prelude_and_root t program_env in
  Module_scope.add root program;
  check_module fresh program_env root
;;

let check_prelude
  { fresh
  ; stamps
  ; interpreter
  ; prelude
  ; modes
  ; const_toplevels
  ; compile_time_c_calls
  ; record_references
  }
  =
  let program_env =
    program_env
      fresh
      ~stamps
      ~modes
      ~const_toplevels
      ~compile_time_c_calls
      ~record_references
      interpreter
  in
  let prelude = add_prelude program_env (force prelude) in
  check_module fresh program_env prelude
;;
