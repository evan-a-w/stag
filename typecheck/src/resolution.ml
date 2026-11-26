open! Import

type t =
  | Local of Env.Entry.t
  | Outside_quote of
      { quote : Scope.quote
      ; usable : Env.Entry.t option
      ; unusable : Env.Entry.t option
      }
  | By_symbol of Env.Entry.t
  | Unusable of Typecheck_error.Kind.t

let of_entry
  ?(spliced = false)
  (scope : Scope.t)
  ~name
  (entry : Env.Entry.t option)
  =
  (* Referred to by symbol, as is the variable code assigns. *)
  let is_mutable_toplevel (entry : Env.Entry.t) =
    match entry.binder with
    | Toplevel ident -> Program_env.is_mutable scope.program ident
    | Local _ -> false
  in
  let is_mutable_local (entry : Env.Entry.t) =
    match entry.binder with
    | Local ident -> Program_env.is_mutable scope.program ident
    | Toplevel _ -> false
  in
  (* A local of plain data may be copied into code whatever its stage, but not a
     mutable one, whose value changes, nor a toplevel, which may not have a
     value yet. One that may be, once what isn't known yet is, is until then
     ([Scope.embeddable]). *)
  let embeddable (entry : Env.Entry.t) =
    match entry.binder with
    | Local _ when not (is_mutable_local entry) ->
      (match
         Scope.embeddable
           scope
           entry.scheme
           (entry.mode :> Mode.Var.t)
           ~known_stage:(Scope.known_stage scope ~bounds:Live)
           ~finality:Provisional
       with
       | `Yes | `Wait -> true
       | `No -> false)
    | Local _ | Toplevel _ ->
      Mode.Stage.le
        (Scope.known_stage scope ~bounds:Live (entry.mode :> Mode.Var.t))
        Portable
  in
  (* Crossing a quote lock needs [portable]: so far, as only its lower bound is
     final, the constraint being added where it is used. A local of this level
     bound in another quote has no value here to embed. *)
  let usable (entry : Env.Entry.t) =
    (entry.level = scope.level
     && [%equal: int option] entry.quote_id (Scope.quote_id scope))
    || ((entry.level < scope.level
         ||
         match entry.binder with
         | Toplevel _ -> entry.level <= scope.level
         | Local _ -> false)
        && ((embeddable entry && not entry.defining)
            || is_mutable_toplevel entry))
  in
  match scope.quote, entry with
  | None, Some entry when usable entry -> Local entry
  | Some quote, Some entry
    when [%equal: int option] entry.quote_id (Some quote.id) -> Local entry
  | Some quote, Some ({ binder = Local _; _ } as entry)
    when spliced
         && entry.level < scope.level
         && (is_mutable_local entry || not (usable entry)) ->
    (* Spliced code's free variable, which this local can't provide: it stays a
       free variable of the quote. *)
    Outside_quote { quote; usable = None; unusable = Some entry }
  | _, Some entry when is_mutable_local entry && entry.level <> scope.level ->
    Unusable
      (Mutable_used_outside_its_code
         { name; var_level = entry.level; used_at = scope.level })
  | Some _, Some ({ binder = Local _; _ } as entry)
    when entry.level < scope.level && not (usable entry) ->
    (* A local of an earlier level: the quote can only embed its value. *)
    let why : Typecheck_error.Not_embeddable_why.t =
      if entry.defining
      then Defining
      else if Mode.Stage.le
                (Mode.Var.lower_assuming
                   (entry.mode :> Mode.Var.t)
                   Stage
                   ~raised:(fun _ -> None))
                Portable
      then Unknown
      else Nonportable
    in
    Unusable (Not_embeddable { name; why })
  | Some _, Some ({ binder = Toplevel _; _ } as entry)
    when (not spliced) && entry.level <= scope.level && not (usable entry) ->
    By_symbol entry
  | Some quote, Some entry when entry.level <= scope.level ->
    let usable = usable entry in
    Outside_quote
      { quote
      ; usable = Option.some_if usable entry
      ; unusable = Option.some_if (not usable) entry
      }
  | Some quote, None -> Outside_quote { quote; usable = None; unusable = None }
  | _, Some entry ->
    Unusable
      (Variable_used_at_wrong_level
         { name; var_level = entry.level; used_at = scope.level })
  | None, None -> Unusable (Not_in_scope (Path (Syntax.Path.of_string name)))
;;
