open! Import

let instance (toplevel : Toplevel_binding.t) ~scheme ~instantiate ~on_use =
  let ty = instantiate scheme in
  (match toplevel.state with
   | Done { awaiting = Some awaiting; _ } ->
     List.iter awaiting.uses ~f:(fun (use : Quoted_use.t) ->
       on_use use ~ty:(instantiate use.ty))
   | Done { awaiting = None; _ } | Unchecked | In_progress _ -> ());
  ty
;;

(* [use], by code of [scope]'s frame, of a toplevel being checked: in a quote of
   the code, or in an instance of a scheme awaiting it. Its type is of the
   frame's [use_rank]: what code in it generalizes doesn't generalize the use,
   which is unified with the toplevel's type once it is known. *)
let add_quoted_use (scope : Scope.t) (use : Quoted_use.t) =
  Unify.lower_ty ~rank:(Scc_frame.use_rank scope.scc) use.ty;
  match scope.scc.kind with
  | Toplevel -> Scc_frame.add_quoting scope.scc use
  | Code _ ->
    let awaited = Program_env.toplevel scope.program use.awaited.name in
    Toplevel_binding.add_pending_use awaited use
;;

let quoted_use t (scope : Scope.t) (toplevel : Toplevel_binding.t) ~loc =
  let ty = Scope.join_ty t scope in
  add_quoted_use
    scope
    { awaited = toplevel.ident
    ; ty
    ; loc
    ; via = []
    ; reporter = Scope.reporter scope
    };
  ty
;;

let instantiate_entry t (scope : Scope.t) (entry : Env.Entry.t) ~loc =
  let instantiate = Scope.instantiate ~at:loc t scope in
  match entry.binder with
  | Local _ -> instantiate entry.scheme
  | Toplevel ident ->
    instance
      (Program_env.toplevel scope.program ident.name)
      ~scheme:entry.scheme
      ~instantiate
      ~on_use:(fun use ~ty ->
        add_quoted_use
          scope
          (Quoted_use.instance
             use
             ~toplevel:ident.name
             ~ty
             ~loc
             ~via:[]
             ~reporter:(Scope.reporter scope)))
;;

let use_entry
  ?(by_symbol = false)
  ?embedding
  (scope : Scope.t)
  ~at
  (entry : Env.Entry.t)
  ~name
  ~loc
  =
  Mode_rules.use
    ?embedding
    (Scope.modes scope)
    at
    entry
    ~locks:scope.locks
    ~by_symbol
    ~toplevel:
      (match entry.binder with
       | Toplevel _ -> true
       | Local _ -> false)
    ~name
    ~loc
;;

let field_modality (scope : Scope.t) field ~record =
  Type_env.field_modality
    scope.program.type_env
    (Typed.Field_ref.name field)
    ~record
;;

let use_free (scope : Scope.t) ~at ~why ~loc =
  Mode_rules.use_free (Scope.modes scope) at ~locks:scope.locks ~loc ~why
;;

let generalizes (scope : Scope.t) mode ~is_function ~loc =
  is_function
  || (Mode.Stage.le (Scope.known_stage scope ~bounds:Live mode) Const
      && Mode_rules.const (Scope.modes scope) mode ~loc ~why:"it is generalized"
     )
;;

(* Whether a local of an earlier level may be embedded, if that isn't known yet:
   it is decided once it is, whatever the order those are found in, by the
   bounds of the parameters when it is embedded. Only then is it required to be
   [portable], as embedding it requires: were it not embeddable, that would only
   add errors. *)
let embedding (scope : Scope.t) (entry : Env.Entry.t) ~name ~loc =
  match entry.binder with
  | Local ident
    when entry.level < scope.level
         && not (Program_env.is_mutable scope.program ident) ->
    let known_stage = Scope.known_stage scope ~bounds:Snapshot in
    let embeddable finality =
      Scope.embeddable
        scope
        entry.scheme
        (entry.mode :> Mode.Var.t)
        ~known_stage
        ~finality
    in
    (match embeddable Provisional with
     | `Yes | `No -> None
     | `Wait ->
       Some
         (fun require ->
           Var_layout.wait
             (Type_env.layouts scope.program.type_env)
             (fun finality ->
                match embeddable finality with
                | `Yes ->
                  require ();
                  true
                | `Wait -> false
                | `No ->
                  Scope.report
                    scope
                    (Typecheck_error.create
                       ~loc
                       (Not_embeddable { name; why = Unknown }));
                  true)))
  | Local _ | Toplevel _ -> None
;;

let resolution_type t (scope : Scope.t) ~at ~name ~loc : Resolution.t -> _
  = function
  | Local entry ->
    let embedding = embedding scope entry ~name ~loc in
    use_entry ?embedding scope ~at entry ~name ~loc;
    Ok (instantiate_entry t scope entry ~loc, Typed.Var_ref.Bound entry.binder)
  | Outside_quote { usable = Some entry; _ } ->
    let embedding = embedding scope entry ~name ~loc in
    use_entry ?embedding scope ~at entry ~name ~loc;
    Ok (instantiate_entry t scope entry ~loc, Persistent entry.binder)
  | By_symbol entry ->
    use_entry scope ~at entry ~name ~loc ~by_symbol:true;
    Ok (instantiate_entry t scope entry ~loc, Persistent entry.binder)
  | Outside_quote { quote; usable = None; unusable } ->
    (* Only a local bound in another quote of this level: a toplevel is referred
       to by symbol. *)
    use_free
      scope
      ~at
      ~why:(sprintf "%s is a free variable of the quote" name)
      ~loc;
    let%map.Result ty =
      Unify.present_entry t ~row:quote.context ~name ~rank:quote.outside.rank
      |> Typecheck_error.lift ~loc
    in
    Option.iter unusable ~f:(fun entry ->
      Unusable_in_quotes.record
        ~entry:ty
        { loc; var_level = entry.level; used_at = scope.level });
    ty, Typed.Var_ref.Free name
  | Unusable kind -> Typecheck_error.error ~loc kind
;;
