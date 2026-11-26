open! Import

(* Calls [f] on each use of a variable that evaluating [expr] (at [depth] quotes
   in) may need: not those in a function it only builds, nor in the code a quote
   builds (but those in the splices that run while it does). [built] is whether
   [expr]'s value is only built by what encloses it, rather than maybe called
   (or otherwise looked into): [f ~called:false] uses only need the variable's
   value. A splice's implicit providers are not uses: whether its code uses one
   at all, let alone while the provider's value is computed, is only known of
   the expansion, which is checked in turn. Explicit substitutions are, though
   the code may not read them. Returns the locals it may call (or look into). *)
let rec eager_uses (expr : Typed.Expr.t) ~depth ~built ~f : Typed.Ident.Set.t =
  let go = eager_uses ~f in
  let all ~built exprs =
    Typed.Ident.Set.union_list (List.map exprs ~f:(go ~depth ~built))
  in
  let use ~built (binder : Typed.Binder.t) =
    match binder with
    | _ when depth <> 0 -> Typed.Ident.Set.empty
    | Toplevel _ ->
      f binder ~called:(not built) expr.loc;
      Typed.Ident.Set.empty
    | Local ident ->
      f binder ~called:(not built) expr.loc;
      if built then Typed.Ident.Set.empty else Typed.Ident.Set.singleton ident
  in
  match expr.desc with
  | Var (Bound binder | Persistent binder) -> use ~built binder
  | Assign ((Bound binder | Persistent binder), value) ->
    Set.union (use ~built:true binder) (go value ~depth ~built:false)
  | Quote body -> go body ~depth:(depth + 1) ~built:false
  | Splice { code; subs } ->
    Set.union
      (go code ~depth:(depth - 1) ~built:false)
      (match subs with
       | Explicit subs -> all ~built:false (List.map subs ~f:snd)
       | Implicit _ -> Typed.Ident.Set.empty)
  | Toplevel_splice { expansion; code = _ } -> go expansion ~depth ~built
  | Fun _ when depth = 0 && built -> Typed.Ident.Set.empty
  | Let { ident; args; body; in_; _ } when depth = 0 ->
    let called = go in_ ~depth ~built in
    (* [ident]'s value is only built if [in_] only builds what it uses it in. *)
    let only_built = not (Set.mem called ident) in
    let body =
      if List.is_empty args
      then go body ~depth ~built:only_built
      else if not only_built
      then go body ~depth ~built:false
      else Typed.Ident.Set.empty
    in
    Set.remove (Set.union called body) ident
  | Seq (first, second) ->
    Set.union (go first ~depth ~built:true) (go second ~depth ~built)
  | Let_mut { body; in_; _ } ->
    Set.union (go body ~depth ~built:false) (go in_ ~depth ~built)
  | Match { scrutinee; cases; exception_cases } ->
    Set.union
      (go scrutinee ~depth ~built:false)
      (all ~built (List.map (cases @ exception_cases) ~f:snd))
  (* What failed to check is not known to call its parts: rather than blame them
     on its failure, as [(fun x -> f) : 'a] referring to [f] in [f]'s cyclic
     type. *)
  | Error _
  | Annotated _
  | Marked _
  | Tuple _
  | Construct _
  | Record _
  | Poly_record _
  | Poly_variant _ -> all ~built (Typed.Expr.sub_exprs expr)
  | _ -> all ~built:false (Typed.Expr.sub_exprs expr)
;;

let self_uses ~body ident =
  let uses = Queue.create () in
  eager_uses body ~depth:0 ~built:true ~f:(fun binder ~called:_ loc ->
    match binder with
    | Local used when Typed.Ident.equal used ident -> Queue.enqueue uses loc
    | Local _ | Toplevel _ -> ())
  |> (ignore : Typed.Ident.Set.t -> unit);
  Queue.to_list uses
;;

let rec run_toplevels (expr : Typed.Expr.t) ~depth ~f =
  (match expr.desc with
   | Var (Bound (Toplevel ident) | Persistent (Toplevel ident))
   | Assign ((Bound (Toplevel ident) | Persistent (Toplevel ident)), _) ->
     if depth = 0 then f ident
   | _ -> ());
  Typed.Expr.iter_children expr ~f:(fun child expr ->
    run_toplevels expr ~depth:(depth + child.depth) ~f)
;;

module Init_need = struct
  (* A toplevel's value, or everything calling (or looking into) it may need. *)
  type t =
    | Value of Typed.Ident.t
    | Call of Typed.Ident.t
  [@@deriving compare, sexp_of]

  include functor Comparable.Make_plain

  let ident (Value ident | Call ident) = ident
end

let check_initialization_cycles
  (program : Program_env.t)
  (members : Typed.Toplevel.t list)
  =
  let member ident =
    List.find members ~f:(fun (member : Typed.Toplevel.t) ->
      Typed.Ident.equal member.ident ident)
  in
  let is_function (member : Typed.Toplevel.t) =
    match member.kind with
    | Function _ -> true
    | Value | Mut -> false
  in
  (* Each need's direct needs, with where they are needed. *)
  let needs (need : Init_need.t) =
    let found = Queue.create () in
    let add need loc = Queue.enqueue found (need, loc) in
    (match need with
     | Value ident ->
       let toplevel = Option.value_exn (member ident) in
       if not (is_function toplevel)
       then
         eager_uses
           toplevel.body
           ~depth:0
           ~built:true
           ~f:(fun binder ~called loc ->
             match binder with
             | Local _ -> ()
             | Toplevel ident ->
               (match member ident with
                | None -> ()
                | Some used ->
                  if called
                  then add (Init_need.Call ident) loc
                  else if not (is_function used)
                  then add (Init_need.Value ident) loc))
         |> (ignore : Typed.Ident.Set.t -> unit)
     | Call ident ->
       let toplevel = Option.value_exn (member ident) in
       add (Value ident) toplevel.body.loc;
       run_toplevels toplevel.body ~depth:0 ~f:(fun used ->
         if Option.is_some (member used) then add (Call used) toplevel.body.loc));
    Queue.to_list found
  in
  List.fold members ~init:false ~f:(fun failed (toplevel : Typed.Toplevel.t) ->
    if failed || is_function toplevel
    then failed
    else (
      let start = Init_need.Value toplevel.ident in
      (* Breadth first, for the shortest cycle. *)
      let parent = ref Init_need.Map.empty in
      let queue = Queue.create () in
      let found = ref None in
      Queue.enqueue queue start;
      while Option.is_none !found && not (Queue.is_empty queue) do
        let current = Queue.dequeue_exn queue in
        List.iter (needs current) ~f:(fun (need, loc) ->
          if Option.is_none !found
          then
            if [%compare.equal: Init_need.t] need start
            then found := Some (current, loc)
            else if not (Map.mem !parent need)
            then (
              parent := Map.set !parent ~key:need ~data:(current, loc);
              Queue.enqueue queue need))
      done;
      match !found with
      | None -> failed
      | Some (last, last_loc) ->
        let rec path need acc =
          if [%compare.equal: Init_need.t] need start
          then need :: acc
          else path (fst (Map.find_exn !parent need)) (need :: acc)
        in
        let chain = path last [] in
        (* Where the first step is needed, in [toplevel]'s own body. *)
        let loc =
          match chain with
          | _ :: second :: _ -> snd (Map.find_exn !parent second)
          | [ _ ] | [] -> last_loc
        in
        let names =
          List.remove_consecutive_duplicates
            (List.map chain ~f:(fun need -> (Init_need.ident need).name)
             @ [ toplevel.ident.name ])
            ~equal:String.equal
        in
        let names =
          match names with
          | [ name ] -> [ name; name ]
          | names -> names
        in
        Program_env.report
          program
          (Typecheck_error.create ~loc (Toplevel_initialization_cycle names));
        true))
;;
