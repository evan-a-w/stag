open! Import

(* The mutable toplevel a component may use when run, if any. *)
let runtime_only (program : Program_env.t) (members : Toplevel_binding.t list) =
  let in_component (ident : Typed.Ident.t) =
    List.exists members ~f:(fun member -> Typed.Ident.equal member.ident ident)
  in
  List.find_map members ~f:(fun member ->
    if Toplevel_binding.is_mut member
    then Some member.ident.name
    else (
      match member.state with
      | In_progress { body = Some body; _ } ->
        With_return.with_return (fun { return } ->
          Init_check.run_toplevels body ~depth:0 ~f:(fun ident ->
            if not (in_component ident)
            then (
              match (Program_env.toplevel program ident.name).state with
              | Done { runtime_only = Some global; _ } -> return (Some global)
              | Done { runtime_only = None; _ } | Unchecked | In_progress _ ->
                ()));
          None)
      | In_progress { body = None; _ } | Unchecked | Done _ -> None))
;;

let kind (member : Toplevel_binding.t) ~args : Typed.Toplevel.Kind.t =
  match member.kind, args with
  | Mut, _ -> Mut
  | (Value | External _), _ | Function _, [] -> Value
  | Function _, arg :: args -> Function (arg :: args)
;;

let is_finished (toplevel : Toplevel_binding.t) =
  match toplevel.state with
  | Done _ -> true
  | Unchecked | In_progress _ -> false
;;

let has_failed (toplevel : Toplevel_binding.t) =
  match toplevel.state with
  | Done { component; _ } -> Component.failed component
  | Unchecked | In_progress _ -> false
;;

(* Relates [ty], the type of the toplevel [use] is of, to [use]'s type by
   variance, as a use of a finished toplevel is, reporting if they differ. In an
   instance of a scheme that failed, eg. as its own use of the toplevel differs,
   or of a local's whose use failed, the error was reported there. *)
let relate_use t (program : Program_env.t) (use : Quoted_use.t) ~ty =
  match Unify.sub_ty t ~lower:ty ~upper:use.ty with
  | Ok () -> true
  | Error error ->
    Quoted_use.fail use;
    if Quoted_use.template_failed use
       || List.exists use.via ~f:(fun via ->
         has_failed (Program_env.toplevel program via.toplevel))
    then Component.fail use.reporter.component
    else Program_env.report_use program use error;
    false
;;

(* [awaiting] has uses of [toplevel], which is not finished: it is refined once
   it is. *)
let await (awaiting : Awaiting.t) (toplevel : Toplevel_binding.t) =
  if is_finished toplevel
  then raise_s [%message "await: finished" (toplevel.ident : Typed.Ident.t)];
  Toplevel_binding.await toplevel awaiting
;;

let scheme (toplevel : Toplevel_binding.t) =
  match toplevel.state with
  | Done { entry; _ } -> entry.scheme
  | Unchecked | In_progress _ ->
    raise_s [%message "scheme: not finished" (toplevel : Toplevel_binding.t)]
;;

(* An instance of the scheme of [awaited], finished, for [use] of it: the uses
   its scheme awaits are of the code of [use], through [awaited]'s quote. *)
let instance_for_use
  t
  (awaited : Toplevel_binding.t)
  (use : Quoted_use.t)
  ~rank
  ~on_use
  =
  Uses.instance
    awaited
    ~scheme:(scheme awaited)
    ~instantiate:(Scheme.instantiate t ~rank)
    ~on_use:(fun added ~ty ->
      on_use
        (Quoted_use.instance
           added
           ~toplevel:awaited.ident.name
           ~ty
           ~loc:use.loc
           ~via:use.via
           ~reporter:use.reporter))
;;

(* Fixes the mode arguments of [ty], of toplevel [name] not generalized, each by
   where it occurs: [None] if in several ways. *)
let default_mode_args t ty ~name =
  let positions = Int.Table.create () in
  Mode_arg_position.iter
    ~con:(Fresh.cons t).variances
    ty
    ~f:(fun var position ->
      Hashtbl.update positions (Mode.Var.id var) ~f:(function
        | None -> var, Some position
        | Some (var, Some before) when Mode_arg_position.equal before position
          -> var, Some position
        | Some (var, _) -> var, None));
  Hashtbl.iter positions ~f:(fun (var, position) ->
    Mode_rules.default_mode_arg var position ~name)
;;

(* Refines [awaiting]'s schemes by the finished toplevels it uses. *)
let refine t (program : Program_env.t) (awaiting : Awaiting.t) =
  let awaited (use : Quoted_use.t) =
    Program_env.toplevel program use.awaited.name
  in
  let finished, remaining =
    List.partition_tf awaiting.uses ~f:(fun use -> is_finished (awaited use))
  in
  List.iter remaining ~f:(fun use ->
    if not (List.mem (awaited use).awaited_by awaiting ~equal:phys_equal)
    then raise_s [%message "refine: a use not awaited" (use : Quoted_use.t)]);
  if not (List.is_empty finished)
  then (
    let schemes = List.map awaiting.schemes ~f:snd @ awaiting.locals in
    (match
       Scheme.reopen
         t
         ~rank:Scc_frame.toplevel_rank
         (schemes
          @ List.map awaiting.uses ~f:(fun (use : Quoted_use.t) -> use.ty))
     with
     | Ok () -> ()
     | Error error ->
       (match finished with
        | use :: _ -> Program_env.report_use program use error
        | [] -> ()));
    let added = ref [] in
    List.iter finished ~f:(fun use ->
      let ty =
        instance_for_use
          t
          (awaited use)
          use
          ~rank:Scc_frame.toplevel_rank
          ~on_use:(fun added_use -> added := added_use :: !added)
      in
      ignore (relate_use t program use ~ty : bool));
    let uses = remaining @ List.rev !added in
    let errors =
      Scheme.generalize_watchers
        t
        ~rank:0
        (schemes @ List.map uses ~f:(fun (use : Quoted_use.t) -> use.ty))
    in
    (match finished with
     | use :: _ -> List.iter errors ~f:(Program_env.report_use program use)
     | [] -> ());
    Scheme.generalize
      t
      ~rank:0
      (schemes @ List.map uses ~f:(fun (use : Quoted_use.t) -> use.ty));
    List.iter awaiting.schemes ~f:(fun (name, ty) ->
      default_mode_args t ty ~name);
    awaiting.uses <- uses;
    List.iter uses ~f:(fun use -> await awaiting (awaited use)))
;;

let settle t (program : Program_env.t) (members : Toplevel_binding.t list) =
  List.iter members ~f:(fun member ->
    List.iter (Toplevel_binding.take_awaited_by member) ~f:(refine t program));
  List.iter members ~f:(fun member ->
    List.iter
      (List.rev (Toplevel_binding.take_pending_uses member))
      ~f:(fun (use : Quoted_use.t) ->
        if not (Component.is_abandoned use.reporter.component)
        then (
          let ty =
            instance_for_use t member use ~rank:0 ~on_use:(fun added ->
              let awaited = Program_env.toplevel program added.awaited.name in
              Toplevel_binding.add_pending_use awaited added)
          in
          ignore (relate_use t program use ~ty : bool))))
;;

let finish_scc
  t
  (program : Program_env.t)
  (members : Toplevel_binding.t list)
  ~layouts
  ~(component : Component.t)
  ~quoting
  ~reopened
  =
  let is_member (use : Quoted_use.t) =
    List.exists members ~f:(fun (member : Toplevel_binding.t) ->
      Typed.Ident.equal member.ident use.awaited)
  in
  let pend (use : Quoted_use.t) =
    Unify.lower_ty ~rank:0 use.ty;
    let awaited = Program_env.toplevel program use.awaited.name in
    Toplevel_binding.add_pending_use awaited use
  in
  let own, others = List.partition_tf (List.rev quoting) ~f:is_member in
  let own_instances, own = List.partition_tf own ~f:Quoted_use.in_instance in
  List.iter own_instances ~f:pend;
  let recursive_uses_unified =
    List.fold own ~init:true ~f:(fun unified (use : Quoted_use.t) ->
      let quoted = Program_env.toplevel program use.awaited.name in
      match quoted.state with
      | In_progress { ty; _ } -> relate_use t program use ~ty && unified
      | Unchecked | Done _ ->
        raise_s
          [%message
            "finish_scc: quoted member not checked"
              (quoted : Toplevel_binding.t)])
  in
  let failed =
    Component.failed component
    || (not recursive_uses_unified)
    || Init_check.check_initialization_cycles
         program
         (List.filter_map members ~f:(fun member ->
            match member.state with
            | In_progress { ty; args; body = Some body; partials; _ } ->
              Some
                { Typed.Toplevel.ident = member.ident
                ; kind = kind member ~args
                ; scheme = ty
                ; body
                ; partials
                }
            | In_progress { body = None; _ } | Unchecked | Done _ -> None))
  in
  let runtime_only = runtime_only program members in
  let init_mode (member : Toplevel_binding.t) =
    match member.state with
    | In_progress { init_mode; _ } -> init_mode
    | Unchecked | Done _ ->
      raise_s
        [%message
          "finish_scc: member not checked" (member : Toplevel_binding.t)]
  in
  let const (member : Toplevel_binding.t) ~why =
    Mode_rules.const
      (Mode_rules.Ctx.create
         ~modes:program.modes
         ~rank:0
         ~holds:(fun ~loc -> function
           | Ok () -> true
           | Error error ->
             Program_env.report
               program
               (Module_scope.within
                  member.module_
                  (Typecheck_error.create ~loc (Mode_error error)));
             false)
         ~crossing:(fun _ -> Mode.Crossing.none)
         ~report:(fun error ->
           Program_env.report program (Module_scope.within member.module_ error))
         ~allocated:(fun _ -> None)
         ~locks:[])
      (init_mode member)
      ~loc:member.expr.loc
      ~why:(why member.ident.name)
  in
  if program.const_toplevels
  then
    List.iter members ~f:(fun member ->
      ignore (const member ~why:(sprintf "%s is a toplevel") : bool));
  let all_functions = List.for_all members ~f:Toplevel_binding.is_function in
  let recursive =
    List.exists members ~f:(fun member ->
      match member.state with
      | In_progress { recursive; _ } -> recursive
      | Unchecked | Done _ -> false)
  in
  (* Like OCaml's [let rec], a recursive non-function is not generalized. *)
  let generalize =
    (all_functions || not recursive)
    && List.for_all members ~f:(fun member ->
      Toplevel_binding.is_function member
      || ((not (Toplevel_binding.is_mut member))
          && Mode.Stage.le (Mode.Var.lower (init_mode member)).stage Const))
    && List.for_all members ~f:(fun member ->
      Toplevel_binding.is_function member
      || const member ~why:(sprintf "%s is generalized"))
  in
  let tys =
    List.map members ~f:(fun member ->
      match member.state with
      | In_progress { ty; _ } -> ty
      | Unchecked | Done _ ->
        raise_s
          [%message
            "finish_scc: member not checked" (member : Toplevel_binding.t)])
  in
  let others_tys = List.map others ~f:(fun (use : Quoted_use.t) -> use.ty) in
  let reopened_tys =
    List.map reopened ~f:(fun (local : Local_uses.Generalized.t) ->
      local.scheme)
  in
  (* What generalizing does to the join variables and layouts, or, if [tys] are
     not generalized, so fixed, its equivalent. Before placing allocations, as
     they may add constraints. *)
  let settle_joins ty =
    if generalize
    then Scheme.generalize_watchers t ~rank:0 [ ty ]
    else Scheme.fix_joins t [ ty ]
  in
  List.iter2_exn members tys ~f:(fun member ty ->
    List.iter (settle_joins ty) ~f:(fun error ->
      Program_env.report
        program
        (Module_scope.within
           member.module_
           (Typecheck_error.unification_error ~loc:member.expr.loc error))));
  List.iter others ~f:(fun (use : Quoted_use.t) ->
    List.iter (settle_joins use.ty) ~f:(Program_env.report_use program use));
  List.iter reopened ~f:(fun (local : Local_uses.Generalized.t) ->
    List.iter
      (settle_joins local.scheme)
      ~f:(Program_env.report_use program (Nonempty_list.hd local.templates)));
  (* The layouts of what its types reach but it doesn't generalize are decided
     where they are bound. *)
  let except =
    if generalize
    then None
    else
      Some
        (Hash_set.mem
           (Scheme.reachable_ids ~rank:0 (tys @ others_tys @ reopened_tys)))
  in
  (match members with
   | [] -> ()
   | first :: _ ->
     List.iter
       (Var_layout.decide ?except layouts ~rank:0 ~finality:Final)
       ~f:(fun error ->
         Program_env.report
           program
           (Module_scope.within
              first.module_
              (Typecheck_error.create ~loc:first.expr.loc (Mode_error error)))));
  (* Once the join variables are settled, and before generalizing, while the
     result may still be constrained. *)
  List.iter
    (Allocation_sites.place
       ~rank:0
       (List.map2_exn members tys ~f:(fun member ty ->
          match member.state with
          | In_progress { sites; _ } -> sites, ty
          | Unchecked | Done _ ->
            raise_s
              [%message
                "finish_scc: member not checked" (member : Toplevel_binding.t)]))
       ~awaiting:(List.map others ~f:(fun (use : Quoted_use.t) -> use.ty)))
    ~f:(Queue.enqueue program.allocations);
  let awaiting =
    if generalize
    then (
      Scheme.generalize t ~rank:0 (tys @ others_tys @ reopened_tys);
      match others with
      | [] -> None
      | _ :: _ ->
        let awaiting =
          { Awaiting.schemes =
              List.map2_exn
                members
                tys
                ~f:(fun (member : Toplevel_binding.t) ty ->
                  member.ident.name, ty)
          ; locals = reopened_tys
          ; uses = others
          }
        in
        List.iter others ~f:(fun use ->
          await awaiting (Program_env.toplevel program use.awaited.name));
        Some awaiting)
    else (
      List.iter others ~f:pend;
      List.iter reopened_tys ~f:(Unify.lower_ty ~rank:0);
      None)
  in
  if failed then Component.fail component;
  Component.finish component;
  List.iter2_exn members tys ~f:(fun member ty ->
    default_mode_args t ty ~name:member.ident.name);
  let members =
    List.map2_exn members tys ~f:(fun member ty ->
      match member.state with
      | In_progress { index; args; mode; held; body = Some body; partials; _ }
        ->
        Program_env.add_info program member.ident ~ty ~level:0;
        let typed =
          { Typed.Toplevel.ident = member.ident
          ; kind = kind member ~args
          ; scheme = ty
          ; body
          ; partials
          }
        in
        Toplevel_binding.set_state
          member
          (Done
             { entry =
                 Env.Entry.toplevel
                   ~defining:false
                   ~ident:member.ident
                   ~scheme:ty
                   ~mode
                   ~held
             ; typed
             ; component
             ; runtime_only
             ; awaiting
             });
        index, typed
      | In_progress { body = None; _ } | Unchecked | Done _ ->
        raise_s
          [%message
            "finish_scc: member not checked" (member : Toplevel_binding.t)])
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
    |> List.map ~f:snd
  in
  let scc = { Typed.Scc.members } in
  Queue.enqueue program.sccs scc;
  Interpreter.add_scc program.interpreter scc
;;
