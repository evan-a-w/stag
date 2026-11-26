open! Import

module Report = struct
  type t =
    { error : Typecheck_error.t
    ; component : Component.t option
    (** Of the code it is in, if its checking may be abandoned. *)
    ; once : string option
    (** Only the first error reported with this key is shown. *)
    ; fallback : bool (** Shown only if no other error of [component] is. *)
    }
end

type t =
  { type_env : Type_env.t
  ; toplevels : Toplevel_binding.t Lazy.t String.Table.t
  ; stamps : Stamp_counter.t
  ; interpreter : Interpreter.t
  ; mutable ident_info : Typed.Ident_info.t Typed.Ident.Map.t
  ; sccs : Typed.Scc.t Queue.t
  ; declaration_splices : Typed.Expr.t Queue.t
  ; unfinished : Type.ty Stack.t
  ; mutable reports : Report.t list
  ; mutable reported : int
  ; mutable syntax_errors : bool
  ; splice_starts : int Stack.t
  ; mutable_idents : Typed.Ident.Hash_set.t
  ; modes : Mode.Id_source.t
  ; const_toplevels : bool
  ; compile_time_c_calls : Compile_time_c_calls.t
  ; parameter_modes : Mode.Var.t Int.Table.t
  ; checking : Scc_frame.t Stack.t
  ; allocations : Typed.Allocation.t Queue.t
  ; mutable allocation_ids : int
  }

let allocation_id t =
  t.allocation_ids <- t.allocation_ids + 1;
  Typed.Allocation.Id.of_int t.allocation_ids
;;

let report ?component ?once ?(fallback = false) t error =
  t.reported <- t.reported + 1;
  t.reports <- { Report.error; component; once; fallback } :: t.reports
;;

let report_by ?fallback t (reporter : Reporter.t) ?once error =
  let component = reporter.component in
  if Typecheck_error.is_already_reported error
  then report ?fallback t error ~component
  else report ?fallback t ?once ~component (Reporter.wrap_error reporter error)
;;

let report_use t (use : Quoted_use.t) error =
  let error = Quoted_use.error use error in
  report_by
    ~fallback:use.reporter.expansion
    t
    use.reporter
    ~once:
      (Sexp.to_string
         [%sexp
           (use.loc : Loc.t)
           , (Typecheck_error.message (Reporter.wrap_error use.reporter error)
              : string)])
    error;
  Component.fail use.reporter.component
;;

let errors t =
  let reports =
    List.rev t.reports
    |> List.filter ~f:(fun { Report.component; _ } ->
      not (Option.exists component ~f:Component.is_abandoned))
  in
  let explained =
    List.filter_map reports ~f:(fun { Report.error; component; fallback; _ } ->
      if fallback || Typecheck_error.is_already_reported error
      then None
      else component)
  in
  let shown = String.Hash_set.create () in
  reports
  |> List.filter ~f:(fun { Report.error; component; once; fallback } ->
    (not (Typecheck_error.is_already_reported error))
    && ((not fallback)
        || Option.for_all component ~f:(fun component ->
          not (List.exists explained ~f:(Component.same component))))
    && Option.for_all once ~f:(fun key ->
      Hash_set.strict_add shown key |> Hash_set.Ok_or_duplicate.is_ok))
  |> List.rev_map ~f:(fun { Report.error; _ } -> error)
;;

let fresh_ident t name = Typed.Ident.fresh t.stamps name
let find_toplevel t name = Hashtbl.find t.toplevels name |> Option.map ~f:force
let toplevel t name = force (Hashtbl.find_exn t.toplevels name)

let add_info t ident ~ty ~level =
  let info = { Typed.Ident_info.ty; level } in
  t.ident_info <- Map.add_exn t.ident_info ~key:ident ~data:info;
  Interpreter.add_ident_info t.interpreter ~ident ~info
;;

let bind t (binder : Source.Binder.t) ty ~level =
  let ident =
    match binder with
    | Name name -> fresh_ident t name.txt
    | Ident ident -> ident
  in
  add_info t ident ~ty ~level;
  Stack.push t.unfinished ty;
  ident
;;

let embedded t ident =
  Interpreter.global_info t.interpreter ~ident
  |> Option.map ~f:(fun info ->
    t.ident_info <- Map.set t.ident_info ~key:ident ~data:info;
    info)
;;

let is_mutable t ident = Hash_set.mem t.mutable_idents ident

exception Uses_unfinished

let at_compile_time (program : t) f ~loc ~error =
  let reflection_failed = ref None in
  let reflect name =
    let reflected =
      let%bind.Result decl = Type_env.reflect program.type_env name in
      let%map.Result decl_type =
        Type_env.record_type
          program.type_env
          (Prelude_path.canonical "Stag.Type.Decl.t")
      in
      { Reflected_decl.Reflection.decl; decl_type }
    in
    Result.map_error reflected ~f:(fun reflection_error ->
      reflection_failed := Some reflection_error;
      Error.of_string "Reflection failed")
  in
  match f ~reflect with
  | Ok _ as ok -> ok
  | Error runtime_error ->
    (match !reflection_failed, Runtime_error.of_error runtime_error with
     | Some reflection_error, _ -> Error reflection_error
     | None, Some { kind = Reached_error; loc = _ } ->
       Error Typecheck_error.already_reported
     | None, (Some { kind = Message _ | Raised _; loc = _ } | None) ->
       Error (error runtime_error))
  | exception Uses_unfinished -> raise Uses_unfinished
  | exception exn ->
    Error
      (Typecheck_error.create
         ~loc
         (Internal_error
            (Error.tag_s
               (Error.of_exn exn)
               ~tag:[%message "Evaluating at compile time raised"])))
;;

let create
  ~type_env
  ~stamps
  ~modes
  ~const_toplevels
  ~compile_time_c_calls
  interpreter
  =
  let rec t =
    lazy
      { type_env = type_env t
      ; toplevels = String.Table.create ()
      ; stamps
      ; interpreter
      ; ident_info = Typed.Ident.Map.empty
      ; sccs = Queue.create ()
      ; declaration_splices = Queue.create ()
      ; allocations = Queue.create ()
      ; allocation_ids = 0
      ; unfinished = Stack.create ()
      ; reports = []
      ; reported = 0
      ; syntax_errors = false
      ; splice_starts = Stack.create ()
      ; mutable_idents = Typed.Ident.Hash_set.create ()
      ; modes
      ; const_toplevels
      ; compile_time_c_calls
      ; parameter_modes = Int.Table.create ()
      ; checking = Stack.create ()
      }
  in
  Lazy.force t
;;

let mark_syntax_errors t = t.syntax_errors <- true
