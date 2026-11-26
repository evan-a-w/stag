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

let of_code (program : Program_env.t) (scc : Scc_frame.t) =
  Local_uses.create
    ~rank:(Scc_frame.use_rank scc)
    ~dispatch:(fun (use : Quoted_use.t) ->
      match scc.kind with
      | Toplevel -> Scc_frame.add_quoting scc use
      | Code _ ->
        Toplevel_binding.add_pending_use
          (Program_env.toplevel program use.awaited.name)
          use)
;;

let code_checked t (scope : Scope.t) =
  List.iter
    (Local_uses.take_generalized scope.uses)
    ~f:(fun ({ Local_uses.Generalized.scheme; templates } as generalized) ->
      (match scope.scc.kind with
       | Toplevel -> Scc_frame.add_reopened scope.scc generalized
       | Code _ -> ());
      match
        Scheme.reopen
          t
          ~rank:(Scc_frame.use_rank scope.scc)
          (scheme
           :: List.map
                (Nonempty_list.to_list templates)
                ~f:(fun (use : Quoted_use.t) -> use.ty))
      with
      | Ok () -> ()
      | Error error ->
        (* Each instance is wrong as its template is. *)
        Nonempty_list.iter templates ~f:Quoted_use.fail;
        Program_env.report_use scope.program (Nonempty_list.hd templates) error)
;;

let quoted_use t (scope : Scope.t) (toplevel : Toplevel_binding.t) ~loc =
  let ty = Scope.join_ty t scope in
  Local_uses.add
    scope.uses
    (Quoted_use.create
       ~awaited:toplevel.ident
       ~ty
       ~loc
       ~reporter:(Scope.reporter scope));
  ty
;;

let instantiate_entry t (scope : Scope.t) (entry : Env.Entry.t) ~loc =
  let instantiate = Scope.instantiate ~at:loc t scope in
  match entry.binder with
  | Local ident ->
    let ty = instantiate entry.scheme in
    List.iter (Local_uses.templates scope.uses ident) ~f:(fun use ->
      Local_uses.add
        scope.uses
        (Quoted_use.local_instance
           use
           ~ty:(instantiate use.ty)
           ~loc
           ~reporter:(Scope.reporter scope)));
    ty
  | Toplevel ident ->
    instance
      (Program_env.toplevel scope.program ident.name)
      ~scheme:entry.scheme
      ~instantiate
      ~on_use:(fun use ~ty ->
        Local_uses.add
          scope.uses
          (Quoted_use.instance
             use
             ~toplevel:ident.name
             ~ty
             ~loc
             ~via:[]
             ~reporter:(Scope.reporter scope)))
;;

let instantiate_captured t (scope : Scope.t) (entry : Env.Entry.t) ~loc =
  Scope.instantiate ~at:loc t scope entry.scheme
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
  || (* Raising parameters only raises the stage, so a mode already above
        [const] needn't be searched, through all the lets it comes from. *)
  (Mode.Stage.le (Mode.Var.lower mode).stage Const
   && Mode.Stage.le (Scope.known_stage scope ~bounds:Live mode) Const
   && Mode_rules.const (Scope.modes scope) mode ~loc ~why:"it is generalized")
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
        ~name
        { loc; var_level = entry.level; used_at = scope.level });
    ty, Typed.Var_ref.Free name
  | Unusable kind -> Typecheck_error.error ~loc kind
;;
