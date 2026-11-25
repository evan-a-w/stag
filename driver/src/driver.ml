open! Import

module Error = struct
  type t =
    | Source of Source_file.Error.t
    | Typecheck of Typecheck_error.t
  [@@deriving sexp_of]

  let loc = function
    | Source error -> Source_file.Error.loc error
    | Typecheck error -> error.loc
  ;;
end

type t =
  { checked : Typecheck.Checked.t
  ; interpreter : Interpreter.t
  ; eval_order : Typed.Ident.t list
  }
[@@deriving fields ~getters]

let toplevels items = List.concat_map items ~f:Typed.Program.Item.toplevels

let create_typecheck
  ?(interpreter = Stag_treewalk.interpreter)
  ?compile_time_c_calls
  ~limits
  ~output
  ?prelude
  ()
  =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create interpreter ~stamps in
  Option.iter output ~f:(Interpreter.set_output interpreter);
  Interpreter.set_limits interpreter limits;
  ( Typecheck.create ~stamps ~interpreter ?compile_time_c_calls ?prelude ()
  , interpreter )
;;

let check
  ?interpreter
  ?compile_time_c_calls
  ?(limits = Limits.unlimited)
  ?output
  ?prelude
  program
  =
  let typecheck, interpreter =
    create_typecheck
      ?interpreter
      ?compile_time_c_calls
      ~limits
      ~output
      ?prelude
      ()
  in
  let%map.Result checked = Typecheck.typecheck typecheck ~program in
  { checked; interpreter; eval_order = toplevels checked.program.items }
;;

module Module_group = struct
  type t =
    | File of Syntax.Toplevel.t
    | Group of Syntax.Name.t * t list

  let name = function
    | File { txt = Module (name, _); _ } | Group (name, _) -> name.txt
    | File
        { txt =
            ( Let _
            | Let_mut _
            | Type _
            | Exception _
            | External _
            | Malformed_exception _
            | Module_splice _
            | Include _
            | Expect_test _
            | Bench _ )
        ; _
        } -> raise_s [%message "Driver.check_modules: a file is not a module"]
  ;;

  let rec to_toplevel = function
    | File toplevel -> toplevel
    | Group (name, members) ->
      Located.create
        ~loc:name.loc
        (Syntax.Toplevel.Module (name, List.map members ~f:to_toplevel))
  ;;
end

(* The toplevels each toplevel refers to. *)
let references (program : Typed.Program.t) =
  let references = Typed.Ident.Table.create () in
  List.iter program.sccs ~f:(fun scc ->
    List.iter scc.members ~f:(fun member ->
      let to_ = Queue.create () in
      ignore
        (Typed.Expr.refers_to member.body ~f:(function
           | Toplevel ident ->
             Queue.enqueue to_ ident;
             false
           | Local _ -> false)
         : bool);
      Hashtbl.set references ~key:member.ident ~data:(Queue.to_list to_)));
  references
;;

(* The members of a group depend on those their toplevels refer to. Components
   of members come after those they depend on, and otherwise in [members] order,
   as do members within a component. *)
let rec group_order
  ~references
  (items : Typed.Program.Item.t list)
  (members : Module_group.t list)
  =
  let members = Array.of_list members in
  let member_items =
    Array.map members ~f:(fun member ->
      let name = Module_group.name member in
      List.find_map items ~f:(function
        | Module (name', items) when String.equal name name' -> Some items
        | Module _ | Toplevel _ -> None)
      |> Option.value ~default:[])
  in
  let member_of = Typed.Ident.Table.create () in
  Array.iteri member_items ~f:(fun i items ->
    List.iter (toplevels items) ~f:(fun ident ->
      Hashtbl.set member_of ~key:ident ~data:i));
  let deps =
    Array.mapi member_items ~f:(fun i items ->
      List.concat_map (toplevels items) ~f:(fun ident ->
        Hashtbl.find references ident |> Option.value ~default:[])
      |> List.filter_map ~f:(Hashtbl.find member_of)
      |> List.filter ~f:(fun j -> j <> i)
      |> Int.Set.of_list
      |> Set.to_list)
  in
  Scc.topological_components ~successors:deps
  |> List.concat
  |> List.concat_map ~f:(fun i ->
    match members.(i) with
    | File _ -> toplevels member_items.(i)
    | Group (_, members) -> group_order ~references member_items.(i) members)
;;

let check_modules
  ?interpreter
  ?compile_time_c_calls
  ?(limits = Limits.unlimited)
  ?output
  ?prelude
  groups
  =
  let typecheck, interpreter =
    create_typecheck
      ?interpreter
      ?compile_time_c_calls
      ~limits
      ~output
      ?prelude
      ()
  in
  let%map.Result checked =
    Typecheck.typecheck
      typecheck
      ~program:(List.map groups ~f:Module_group.to_toplevel)
  in
  let eval_order =
    group_order
      ~references:(references checked.program)
      checked.program.items
      groups
  in
  { checked; interpreter; eval_order }
;;

let eval ?(limits = Limits.unlimited) t =
  Interpreter.set_limits t.interpreter limits;
  let%bind.Or_error () =
    List.fold_result t.eval_order ~init:() ~f:(fun () ident ->
      Interpreter.eval_toplevel t.interpreter ~ident)
  in
  Interpreter.eval t.interpreter
;;

let check_files
  ?interpreter
  ?compile_time_c_calls
  ?limits
  ?output
  ?prelude
  files
  =
  let parsed = List.map files ~f:Source_file.parse_recovering in
  let source_errors =
    List.concat_map parsed ~f:snd
    |> List.map ~f:(fun error -> Error.Source error)
  in
  match Option.all (List.map parsed ~f:fst) with
  | None -> Result.Error (Loc.sort_by source_errors ~f:Error.loc)
  | Some modules ->
    (match
       check_modules
         ?interpreter
         ?compile_time_c_calls
         ?limits
         ?output
         ?prelude
         (List.map modules ~f:(fun file -> Module_group.File file))
     with
     | Ok t when List.is_empty source_errors -> Ok t
     | Ok _ -> Error (Loc.sort_by source_errors ~f:Error.loc)
     | Error { errors; partial = _ } ->
       Error
         (Loc.sort_by
            (source_errors
             @ List.map errors ~f:(fun error -> Error.Typecheck error))
            ~f:Error.loc))
;;

let check_prelude ?interpreter ?(limits = Limits.unlimited) ?output prelude =
  let typecheck, interpreter =
    create_typecheck ?interpreter ~limits ~output ~prelude ()
  in
  let%map.Result checked = Typecheck.check_prelude typecheck in
  { checked; interpreter; eval_order = toplevels checked.program.items }
;;
