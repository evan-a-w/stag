open! Import
open Or_error.Let_syntax
module Env = Env
module Syntax_value = Syntax_value

module Value = struct
  include Value

  let to_syntax : type a. a Generated_syntax.t -> t -> loc:Loc.t -> a Or_error.t
    =
    fun syntax value ~loc ->
    match syntax with
    | Expr -> Syntax_value.Expr.of_value value ~loc
    | Type_expr -> Syntax_value.Type_expr.of_value value ~loc
    | Type_decl_kind -> Syntax_value.Type_decl.Kind.of_value value ~loc
    | Program -> Syntax_value.Program.of_value value ~loc
  ;;
end

(* The values embedded by a quote being built. [renames] are its own embeddings,
   of binders it refers to. *)
module Embedded = struct
  type t =
    { renames : Typed.Ident.t Typed.Ident.Table.t
    ; mutable values : Value.t Typed.Ident.Map.t
    }

  let create () =
    { renames = Typed.Ident.Table.create (); values = Typed.Ident.Map.empty }
  ;;

  let add t ~ident ~value = t.values <- Map.set t.values ~key:ident ~data:value
end

(* A place that a [Store] writes to: a pointer's cell, and the fields of the
   value in it to reach the part stored. Fields are copied on the way, so a
   record is never mutated in place: [p.*.x <- v] leaves other holders of the
   record's value unchanged. *)
module Store_place = struct
  type step =
    | Field of int
    | Poly_field of string

  type t =
    { cell : Value.t ref
    ; path : step list
    }

  let rec update (value : Value.t) (path : step list) ~value_to_store
    : Value.t Or_error.t
    =
    match path with
    | [] -> Ok value_to_store
    | Field index :: path ->
      (match value with
       | Record (record_type, values) when index < Array.length values ->
         let values = Array.copy values in
         (match update values.(index) path ~value_to_store with
          | Error _ as error -> error
          | Ok inner ->
            values.(index) <- inner;
            Ok (Value.Record (record_type, values)))
       | _ ->
         Or_error.error_s
           [%message "Field of a non-record" (index : int) (value : Value.t)])
    | Poly_field label :: path ->
      (match value with
       | Poly_record fields ->
         (match Map.find fields label with
          | None ->
            Or_error.error_s
              [%message "No such polymorphic record field" (label : string)]
          | Some field ->
            (match update field path ~value_to_store with
             | Error _ as error -> error
             | Ok field ->
               Ok (Poly_record (Map.set fields ~key:label ~data:field))))
       | _ ->
         Or_error.error_s
           [%message "Field of a non-polymorphic-record" (label : string)])
  ;;

  let store ({ cell; path } : t) ~value =
    match update !cell path ~value_to_store:value with
    | Error _ as error -> error
    | Ok content ->
      cell := content;
      Ok ()
  ;;
end

(* What a step evaluates to: a value, or an expression in tail position, to
   evaluate in place of the step so that tail calls take no stack. *)
module Next = struct
  (* Usually stack allocated, by [step]. *)
  type t =
    | Value of Value.t @@ global
    | Eval of
        { env : Value.t Env.t @@ global
        ; level : int
        ; expr : Typed.Expr.t @@ global
        }
    | Failed of Error.t @@ global

  let of_local_result : Value.t Eval_result.t @ local -> t @ local = function
    | Ok { global = value } -> exclave_ Value value
    | Error { global = error } -> exclave_ Failed error
  ;;
end

module Toplevel_state = struct
  type t =
    | Component of Typed.Scc.t (** None of its members' closures are bound. *)
    | Unevaluated of Typed.Expr.t
    | Evaluating
    | Evaluated
    | Failed of
        { error : Error.t
        ; uncatchable : Error.t
        (** As [error], but no [match] can handle it, as the toplevel's users
            can't handle its failure. *)
        }
end

module Phase = struct
  type t =
    | Run_time
    | Compile_time of { reflect : string -> Value.t Or_error.t }
    | Expect_test of { on_expect : string Located.t -> unit }

  let is_compile_time = function
    | Compile_time _ -> true
    | Run_time | Expect_test _ -> false
  ;;
end

type t =
  { toplevels : Toplevel_state.t Typed.Ident.Table.t
  (** Each has a slot in [env] once queued, filled once [Evaluated]. Once
      queued, only [force] and [bind_functions] change its state. *)
  ; ident_info : Typed.Ident_info.t Typed.Ident.Table.t
  ; closure_captures : Typed.Ident.Hash_set.t Typed.Ident.Table.t
  ; mutable env : Value.t Env.t
  ; stamps : Stamp_counter.t
  ; mutable budget : Budget.t
  ; mutable output : string -> unit
  ; mutable phase : Phase.t
  ; c_functions : C_call.t
  }

let captures t ~ident ~args ~body =
  let level (ident : Typed.Ident.t) =
    (Hashtbl.find_exn t.ident_info ident).level
  in
  let closure_level = level ident in
  let defined = Typed.Ident.Hash_set.of_list args in
  let captures = Typed.Ident.Hash_set.create () in
  let use_at ~depth ident ~level =
    if depth >= 0
       && (not (Hash_set.mem defined ident))
       && level <= closure_level
    then Hash_set.add captures ident
  in
  let use ~depth ident = use_at ~depth ident ~level:(level ident) in
  let rec go ~depth (expr : Typed.Expr.t) =
    Typed.Expr.iter_own_refs expr ~f:(function
      | Free _ -> ()
      (* In a quote, a toplevel is referred to by symbol: only its value is
         captured, and it may still be being checked. *)
      | Binder (Toplevel ident) ->
        if depth = 0 then use_at ~depth ident ~level:0
      | Binder (Local ident) -> use ~depth ident);
    Typed.Expr.iter_children expr ~f:(fun child expr ->
      List.iter child.binds ~f:(Hash_set.add defined);
      go ~depth:(depth + child.depth) expr)
  in
  go ~depth:0 body;
  captures
;;

let type_id = Type_equal.Id.create ~name:"Treewalk" sexp_of_opaque

let create ~stamps () =
  { toplevels = Typed.Ident.Table.create ()
  ; ident_info = Typed.Ident.Table.create ()
  ; closure_captures = Typed.Ident.Table.create ()
  ; env = Env.create ()
  ; stamps
  ; budget = Budget.create Limits.unlimited
  ; output = Out_channel.output_string stdout
  ; phase = Run_time
  ; c_functions = C_call.create ()
  }
;;

let set_output t output = t.output <- output

let with_output t output ~f =
  let previous = t.output in
  t.output <- output;
  Exn.protect ~f ~finally:(fun () -> t.output <- previous)
;;

let set_limits t limits = t.budget <- Budget.create limits

let add_ident_info t ~ident ~info =
  Hashtbl.set t.ident_info ~key:ident ~data:info
;;

let add_scc t (scc : Typed.Scc.t) =
  List.iter scc.members ~f:(fun { ident; _ } ->
    match Hashtbl.find t.toplevels ident with
    | Some (Evaluating | Evaluated) ->
      raise_s
        [%message
          "Treewalk.add_scc: toplevel already evaluated" (ident : Typed.Ident.t)]
    | Some (Component _ | Unevaluated _ | Failed _) | None -> ());
  List.iter scc.members ~f:(fun { ident; _ } ->
    if not (Env.mem t.env ident) then Env.declare t.env ~ident;
    Hashtbl.set t.toplevels ~key:ident ~data:(Component scc))
;;

let global_info t ~ident =
  if Env.mem t.env ident then Hashtbl.find t.ident_info ident else None
;;

let map_result list ~(f @ local) =
  List.fold_result list ~init:[] ~f:(fun acc x ->
    let%map y = f x in
    y :: acc)
  >>| List.rev
;;

let map_fields fields ~(f @ local) =
  map_result fields ~f:(fun (key, expr) ->
    let%map expr = f expr in
    key, expr)
  [@nontail]
;;

module Traverse_or_error = Typed.Expr.Traverse (struct
    type 'a t = 'a Or_error.t

    let return = Or_error.return

    let bind x ~(f @ local) =
      match x with
      | Ok x -> f x
      | Error _ as error -> error
    ;;
  end)

(* [expr] with [f ~depth] applied to its children, where [depth] is that of
   [expr], counting quotes up and splices down. Children are visited in order. *)
let map_children ~depth (expr : Typed.Expr.t) ~(f @ local) =
  Traverse_or_error.map_children expr ~f:(fun child expr ->
    f ~depth:(depth + child.depth) expr)
  [@nontail]
;;

let fresh_like t (ident : Typed.Ident.t) ~info =
  let fresh = Typed.Ident.fresh t.stamps ident.name in
  Hashtbl.set t.ident_info ~key:fresh ~data:info;
  fresh
;;

(* A copy of [code] with fresh binders, so copies spliced into one program are
   distinct, and the entries of its context resolved by [entry]. *)
let instantiate t (code : Typed.Expr.t) ~entry =
  let renames = Typed.Ident.Table.create () in
  let fresh ident =
    let fresh =
      fresh_like t ident ~info:(Hashtbl.find_exn t.ident_info ident)
    in
    Hashtbl.set renames ~key:ident ~data:fresh;
    fresh
  in
  (* A persistent local the code does not bind is one embedded in it, bound
     outside it to its value; any other is bound in the code. *)
  let rename_binder ~persistent : Typed.Binder.t -> Typed.Binder.t = function
    | Local ident ->
      (match Hashtbl.find renames ident with
       | Some ident -> Local ident
       | None when persistent -> Local ident
       | None ->
         raise_s
           [%message
             "Treewalk.instantiate: a local the code does not bind"
               (ident : Typed.Ident.t)])
    | Toplevel _ as binder -> binder
  in
  let rename_annots =
    List.map ~f:(fun (ident, annot) -> Hashtbl.find_exn renames ident, annot)
  in
  let rec pattern ({ desc; _ } as node : Typed.Pattern.t) =
    let desc : Typed.Pattern.t Typed.Pattern.Desc.t =
      match desc with
      | (Any | Lit _) as desc -> desc
      | Var ident -> Var (fresh ident)
      | Tuple patterns -> Tuple (List.map patterns ~f:pattern)
      | Deref inner -> Deref (pattern inner)
      | Error patterns -> Error (List.map patterns ~f:pattern)
      | Construct (constructor, patterns) ->
        Construct (constructor, List.map patterns ~f:pattern)
      | Record fields ->
        Record (List.map fields ~f:(fun (field, p) -> field, pattern p))
      | Poly_variant (tag, arg) -> Poly_variant (tag, Option.map arg ~f:pattern)
      | Poly_record { fields; rest } ->
        let fields = List.map fields ~f:(fun (label, p) -> label, pattern p) in
        let rest : Typed.Pattern.Rest.t =
          match rest with
          | (Closed | Open) as rest -> rest
          | Bind ident -> Bind (fresh ident)
        in
        Poly_record { fields; rest }
    in
    { node with desc }
  in
  let rec go ~depth (expr : Typed.Expr.t) =
    Budget.step t.budget ~loc:expr.loc ~f:(fun () -> go_step ~depth expr)
    [@nontail]
  and go_step ~depth (expr : Typed.Expr.t) =
    let desc : Typed.Expr.t Typed.Expr.Desc.t =
      match expr.desc with
      | Var (Free name as var_ref) when depth = 0 ->
        Var (Option.value (entry name) ~default:var_ref)
      | Assign ((Free name as var_ref), value) when depth = 0 ->
        Assign (Option.value (entry name) ~default:var_ref, value)
      | Var (Bound binder) ->
        Var (Bound (rename_binder ~persistent:false binder))
      | Var (Persistent binder) ->
        Var (Persistent (rename_binder ~persistent:true binder))
      | Assign (Bound binder, value) ->
        Assign (Bound (rename_binder ~persistent:false binder), value)
      | Assign (Persistent binder, value) ->
        Assign (Persistent (rename_binder ~persistent:true binder), value)
      | Let_mut let_ -> Let_mut { let_ with ident = fresh let_.ident }
      | Let let_ ->
        let ident = fresh let_.ident in
        let args = List.map let_.args ~f:fresh in
        Let { let_ with ident; args; annots = rename_annots let_.annots }
      | Fun fun_ ->
        let args = Nonempty_list.map fun_.args ~f:fresh in
        Fun { fun_ with args; annots = rename_annots fun_.annots }
      | Match { scrutinee; cases; exception_cases } ->
        let rename = List.map ~f:(fun (p, body) -> pattern p, body) in
        Match
          { scrutinee
          ; cases = rename cases
          ; exception_cases = rename exception_cases
          }
      | Splice { code; subs = Implicit providers } ->
        Splice
          { code
          ; subs =
              Implicit (Map.map providers ~f:(rename_binder ~persistent:false))
          }
      | desc -> desc
    in
    map_children ~depth { expr with desc } ~f:go
  in
  go ~depth:0 code
;;

(* [code], spliced at [level] with [subs]. *)
let splice_code
  t
  (code : Typed.Expr.t)
  ~(subs : Typed.Expr.t Typed.Expr.Subs.t)
  ~level
  =
  match subs with
  | Implicit providers ->
    instantiate t code ~entry:(fun name ->
      Map.find providers name
      |> Option.map ~f:(fun binder -> Typed.Var_ref.Bound binder))
  | Explicit subs ->
    (* A variable is substituted as itself, so that the code assigns it if it is
       mutable, and reads its value when it does. *)
    let subs =
      List.map subs ~f:(fun (name, (sub : Typed.Expr.t)) ->
        match sub.desc with
        | Var ((Bound _ | Persistent _) as var_ref) -> name, First var_ref
        | _ ->
          ( name
          , Second
              ( fresh_like
                  t
                  { name; stamp = 0 }
                  ~info:{ Typed.Ident_info.ty = sub.ty; level }
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
    let%map code = instantiate t code ~entry:(Map.find entries) in
    let subs =
      List.filter_map subs ~f:(fun (name, sub) ->
        Either.Second.to_option sub
        |> Option.map ~f:(fun (ident, sub) -> name, ident, sub))
    in
    List.fold_right subs ~init:code ~f:(fun (_, ident, sub) in_ ->
      { Typed.Expr.desc =
          Let
            { ident
            ; args = []
            ; annots = []
            ; result_annot = None
            ; mode = None
            ; generalized = false
            ; body = sub
            ; in_
            }
      ; ty = in_.ty
      ; loc = in_.loc
      ; made = Typed.Expr.Made.none
      })
;;

let runtime_error ~loc message = exclave_
  Eval_result.error (Runtime_error.to_error (Runtime_error.create ~loc message))
;;

let raise_exn ~loc exn = exclave_
  Eval_result.error (Runtime_error.to_error (Runtime_error.raise_exn ~loc exn))
;;

let prelude_exception name : Typed.Constructor_ref.t =
  Typed.Constructor_ref.exception_ (Prelude_path.canonical name)
;;

let prim_exception : Prim.Exception.t -> Value.t = function
  | Division_by_zero -> Construct (prelude_exception "Division_by_zero", [])
  | Invalid_argument message ->
    Construct (prelude_exception "Invalid_argument", [ String message ])
  | Failure message ->
    Construct (prelude_exception "Failure", [ String message ])
;;

let pattern_mismatch pattern value = exclave_
  Eval_result.error_s
    [%message
      "Pattern does not fit value" (pattern : Typed.Pattern.t) (value : Value.t)]
;;

let reached_error ~loc = exclave_
  Eval_result.error
    (Runtime_error.to_error { Runtime_error.loc; kind = Reached_error })
;;

(* Binds the variables of [pattern] in [env] if it matches [value]. *)
let rec match_pattern env (pattern : Typed.Pattern.t) (value : Value.t)
  : bool Eval_result.t @ local
  = exclave_
  match pattern.desc, value with
  | Any, _ -> Eval_result.return true
  | Error _, _ -> reached_error ~loc:pattern.loc
  | Var ident, _ ->
    Env.set env ~ident ~value;
    Eval_result.return true
  | Lit (Int a), Int b -> Eval_result.return (Int.equal a b)
  | Lit (Bool a), Bool b -> Eval_result.return (Bool.equal a b)
  | Lit (String a), String b -> Eval_result.return (String.equal a b)
  | Lit (Char a), Char b -> Eval_result.return (Char.equal a b)
  | Lit Unit, Unit -> Eval_result.return true
  | Tuple patterns, Tuple values -> match_all env pattern patterns values
  | Deref inner, Ptr cell -> match_pattern env inner !cell
  | Deref inner, Box value -> match_pattern env inner value
  | Construct (constructor, patterns), Construct (constructor', values) ->
    if constructor.index = constructor'.index
       && ((not (Typed.Constructor_ref.is_exception constructor))
           || String.equal constructor.name constructor'.name)
    then match_all env pattern patterns values
    else Eval_result.return false
  | Record fields, Record (_, values) ->
    if List.exists fields ~f:(fun ((field : Typed.Field_ref.t), _) ->
         field.index >= Array.length values)
    then pattern_mismatch pattern value
    else
      match_all
        env
        pattern
        (List.map fields ~f:snd)
        (List.map fields ~f:(fun (field, _) -> values.(field.index)))
  | Poly_variant (tag, arg), Poly_variant (tag', value) ->
    if not (String.equal tag tag')
    then Eval_result.return false
    else (
      match arg with
      | None -> Eval_result.return true
      | Some arg -> match_pattern env arg value)
  | Poly_record { fields; rest }, Poly_record values ->
    (match
       List.map fields ~f:(fun (label, p) ->
         Map.find values label |> Option.map ~f:(fun v -> p, v))
       |> Option.all
     with
     | None -> pattern_mismatch pattern value
     | Some pairs ->
       let%mapl_fun.Local_result { global = matched } =
         match_all env pattern (List.map pairs ~f:fst) (List.map pairs ~f:snd)
       in
       (match rest with
        | Bind ident when matched ->
          let rest =
            List.fold fields ~init:values ~f:(fun values (label, _) ->
              Map.remove values label)
          in
          Env.set env ~ident ~value:(Poly_record rest)
        | Bind _ | Closed | Open -> ());
       { global = matched })
  | ( ( Lit _
      | Tuple _
      | Deref _
      | Construct _
      | Record _
      | Poly_variant _
      | Poly_record _ )
    , _ ) -> pattern_mismatch pattern value

and match_all env pattern patterns values : bool Eval_result.t @ local =
  (* [go] returns on the heap, so each pattern's result is freed by its tail
     call. *)
  let rec go ps vs : bool Or_error.t =
    match ps, vs with
    | p :: ps, v :: vs ->
      (match match_pattern env p v with
       | Ok { global = true } -> go ps vs
       | Ok { global = false } -> Ok false
       | Error { global = error } -> Error error)
    | [], [] -> Ok true
    | [], _ :: _ | _ :: _, [] -> assert false
  in
  if List.length patterns <> List.length values
  then exclave_ pattern_mismatch pattern (Tuple values)
  else exclave_ Eval_result.of_or_error (go patterns values)
;;

(* [applied], latest first, as prim values in order, or [Null] if one isn't. *)
let prim_args applied =
  List.fold_until
    applied
    ~init:[]
    ~f:(fun args value ->
      match Value.to_prim value with
      | This arg -> Continue (arg :: args)
      | Null -> Stop Null)
    ~finish:(fun args -> This args)
;;

let poly_record (value : Value.t) : Value.t String.Map.t Eval_result.t @ local
  = exclave_
  match value with
  | Poly_record fields -> Eval_result.return fields
  | _ -> Eval_result.error_s [%message "Expected a record" (value : Value.t)]
;;

let poly_field fields label : Value.t Eval_result.t @ local = exclave_
  match Map.find fields label with
  | Some value -> Eval_result.return value
  | None ->
    Eval_result.error_s
      [%message "Missing field" label (fields : Value.t String.Map.t)]
;;

let code_of_value (value : Value.t)
  : (Typed.Expr.t * Value.t Typed.Ident.Map.t) Eval_result.t @ local
  = exclave_
  match value with
  | Code { expr; persistent } -> Eval_result.return (expr, persistent)
  | _ ->
    Eval_result.error_s [%message "Spliced a non-code value" (value : Value.t)]
;;

(* Toplevels are evaluated on first use, so compile time evaluation only runs
   what it needs. *)
let rec force t ident : unit Eval_result.t @ local = exclave_
  match Hashtbl.find t.toplevels ident with
  | None | Some (Evaluating | Evaluated) -> Eval_result.return ()
  | Some (Component scc) ->
    let%bindl.Local_result { global = () } = bind_functions t scc in
    force t ident
  | Some (Failed { error; uncatchable = _ }) -> Eval_result.error error
  | Some (Unevaluated body) ->
    Hashtbl.set t.toplevels ~key:ident ~data:Evaluating;
    (* An exception, eg. from [reflect], abandons the evaluation, to be done
       again when next forced. *)
    (match eval t ~env:(Env.create ()) ~level:0 body with
     | exception exn ->
       Hashtbl.set t.toplevels ~key:ident ~data:(Unevaluated body);
       raise exn
     | Ok { global = value } ->
       Env.set t.env ~ident ~value;
       Hashtbl.set t.toplevels ~key:ident ~data:Evaluated;
       Eval_result.return ()
     | Error { global = error } ->
       let uncatchable =
         match Runtime_error.of_error error with
         | Some { kind = Raised exn; loc } ->
           Runtime_error.to_error
             (Runtime_error.create
                ~loc
                (sprintf
                   "Initialization of %s raised %s"
                   ident.name
                   (Runtime_error.Raised.to_string_for_message exn)))
         | Some { kind = Message _ | Reached_error; loc = _ } | None -> error
       in
       Hashtbl.set t.toplevels ~key:ident ~data:(Failed { error; uncatchable });
       Eval_result.error error)

(* A filled slot's toplevel is [Evaluated], so needs no [force]. *)
and lookup t env ident ~loc : Value.t Eval_result.t @ local = exclave_
  match Env.find_slot env ~fallback:t.env ident with
  | This { value = This value } -> Eval_result.return value
  | This { value = Null } | Null -> lookup_forcing t env ident ~loc

and lookup_forcing t env ident ~loc : Value.t Eval_result.t @ local = exclave_
  match force t ident with
  | Error error ->
    (match Hashtbl.find t.toplevels ident with
     | Some (Failed { uncatchable; error = _ }) -> Eval_result.error uncatchable
     | Some (Component _ | Unevaluated _ | Evaluating | Evaluated) | None ->
       Error error)
  | Ok { global = () } ->
    (match Env.find_slot env ~fallback:t.env ident with
     | This { value = This value } -> Eval_result.return value
     | This { value = Null } ->
       runtime_error
         ~loc
         (sprintf
            "%s is used before its value is defined (a value may not depend on \
             itself)"
            ident.name)
     | Null -> Eval_result.of_or_error (Env.lookup env ~fallback:t.env ~ident))

(* Making closures reads nothing, so a component's functions are all bound at
   once, and its values evaluated each when first used. Its members leave
   [Component] together, here. *)
and bind_functions t ({ members } : Typed.Scc.t) : unit Eval_result.t @ local
  = exclave_
  let members =
    List.filter members ~f:(fun { ident; _ } ->
      match Hashtbl.find t.toplevels ident with
      | Some (Component _) -> true
      | Some (Unevaluated _ | Evaluating | Evaluated | Failed _) | None -> false)
  in
  let functions, values =
    List.partition_map members ~f:(fun member ->
      match member.kind with
      | Function args -> First (member, Nonempty_list.to_list args)
      | Value | Mut -> Second member)
  in
  match
    Eval_result.List.map
      functions
      ~f:(fun ({ ident; body; _ }, args) -> exclave_
        closure t ~env:t.env ~level:0 ~ident ~args ~body)
  with
  | Error { global = error } ->
    List.iter members ~f:(fun { ident; _ } ->
      Hashtbl.set
        t.toplevels
        ~key:ident
        ~data:(Failed { error; uncatchable = error }));
    Eval_result.error error
  | Ok { global = closures } ->
    List.iter2_exn functions closures ~f:(fun ({ ident; _ }, _) value ->
      Env.set t.env ~ident ~value;
      Hashtbl.set t.toplevels ~key:ident ~data:Evaluated);
    List.iter values ~f:(fun { ident; body; _ } ->
      Hashtbl.set t.toplevels ~key:ident ~data:(Unevaluated body));
    Eval_result.return ()

and store_place t ~env ~level (expr : Typed.Expr.t)
  : Store_place.t Eval_result.t @ local
  = exclave_
  match expr.desc with
  | Deref pointer ->
    let%bindl.Local_result { global = pointer } = eval t ~env ~level pointer in
    (match pointer with
     | Ptr cell -> Eval_result.return { Store_place.cell; path = [] }
     | _ ->
       Eval_result.error_s
         [%message "Dereferenced a non-pointer" (pointer : Value.t)])
  | Field (inner, field) ->
    let%bindl.Local_result { global = place } =
      store_place t ~env ~level inner
    in
    Eval_result.return
      { place with path = Store_place.Field field.index :: place.path }
  | Poly_field (inner, label) ->
    let%bindl.Local_result { global = place } =
      store_place t ~env ~level inner
    in
    Eval_result.return { place with path = Poly_field label :: place.path }
  | _ ->
    Eval_result.error_s
      [%message "Not a place to store to" (expr : Typed.Expr.t)]

(* [env] has the bindings of the current activation, at [level]. *)
and eval t ~env ~level expr : Value.t Eval_result.t @ local = exclave_
  match Budget.enter t.budget ~loc:expr.loc with
  | Error error -> Eval_result.error error
  | Ok () ->
    let result = run t ~env ~level expr in
    Budget.exit t.budget;
    result

(* Steps until a value, each tail step counted but no deeper. The [Next.t] of a
   step is on [run]'s stack, freed by the tail call, its fields being global. *)
and run t ~env ~level expr : Value.t Eval_result.t @ local =
  match step t ~env ~level expr with
  | Value value -> exclave_ Eval_result.return value
  | Failed error -> exclave_ Eval_result.error error
  | Eval { env; level; expr } ->
    (match Budget.tail_step t.budget ~loc:expr.loc with
     | Error error -> exclave_ Eval_result.error error
     | Ok () -> exclave_ run t ~env ~level expr)

and step t ~env ~level (expr : Typed.Expr.t) : Next.t =
  match expr.desc with
  | App (f, arg) ->
    (match eval t ~env ~level f with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = f } ->
       (match eval t ~env ~level arg with
        | Error { global = error } -> exclave_ Failed error
        | Ok { global = arg } -> exclave_ apply t f arg ~loc:expr.loc))
  | Annotated (expr, _) | Marked (_, expr) -> exclave_ Eval { env; level; expr }
  | Let { ident; args; body; in_; _ } ->
    (match bind_let t ~env ~level ~ident ~args ~body with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = () } -> exclave_ Eval { env; level; expr = in_ })
  | Seq (first, second) ->
    (match eval t ~env ~level first with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = (_ : Value.t) } ->
       exclave_ Eval { env; level; expr = second })
  | Let_mut { ident; body; in_; annot = _ } ->
    (match eval t ~env ~level body with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = value } ->
       Env.declare env ~ident;
       Env.set env ~ident ~value;
       exclave_ Eval { env; level; expr = in_ })
  | Toplevel_splice { code = _; expansion } ->
    exclave_ Eval { env; level; expr = expansion }
  | Match { scrutinee; cases; exception_cases } ->
    (match eval t ~env ~level scrutinee with
     | Ok { global = value } ->
       exclave_ eval_match t ~env ~level value cases ~loc:expr.loc
     | Error { global = error } ->
       (match exception_cases, Runtime_error.raised error with
        | _ :: _, Some exn ->
          exclave_ handle t ~env ~level exn exception_cases ~unhandled:error
        | _ -> exclave_ Failed error))
  | _ -> exclave_ Next.of_local_result (step_value t ~env ~level expr)

and step_value t ~env ~level (expr : Typed.Expr.t)
  : Value.t Eval_result.t @ local
  = exclave_
  match expr.desc with
  | Lit lit -> Eval_result.return (Value.of_lit lit)
  | Expect expected ->
    (match t.phase with
     | Expect_test { on_expect } ->
       on_expect expected;
       Eval_result.return Value.Unit
     | Run_time | Compile_time _ ->
       Eval_result.error_s [%message "[%expect] outside a running expect test"])
  | Var (Bound (Local ident | Toplevel ident))
  | Var (Persistent (Local ident | Toplevel ident)) ->
    lookup t env ident ~loc:expr.loc
  | Var (Free name) ->
    Eval_result.error_s [%message "Free variable of code evaluated" name]
  | Var (Prim prim) -> Eval_result.return (Value.Prim { prim; applied = [] })
  | External { symbol; signature; at_compile_time } ->
    Eval_result.return
      (Value.External { symbol; signature; at_compile_time; applied = [] })
  | Var (Intrinsic intrinsic) -> Eval_result.return (Value.Intrinsic intrinsic)
  | Type ty -> Eval_result.return (Value.Type ty)
  | Assign (var_ref, value) ->
    let%bindl.Local_result { global = value } = eval t ~env ~level value in
    (match var_ref with
     | Bound (Local ident | Toplevel ident)
     | Persistent (Local ident | Toplevel ident) ->
       let%bindl.Local_result { global = () } = force t ident in
       (match Env.find_slot env ~fallback:t.env ident with
        | This slot ->
          slot.value <- This value;
          Eval_result.return Value.Unit
        | Null ->
          Eval_result.error_s
            [%message "Assigned an unbound ident" (ident : Typed.Ident.t)])
     | Free _ | Prim _ | Intrinsic _ ->
       Eval_result.error_s
         [%message "Assigned a non-variable" (var_ref : Typed.Var_ref.t)])
  | App _
  | Let _
  | Seq _
  | Let_mut _
  | Annotated _
  | Marked _
  | Toplevel_splice _
  | Match _ -> run t ~env ~level expr
  | Fun { args = first :: _ as args; annots = _; body } ->
    (* Its first parameter identifies it, as a let's name does. *)
    closure t ~env ~level ~ident:first ~args:(Nonempty_list.to_list args) ~body
  | Splice _ ->
    Eval_result.error_s [%message "Splice outside a quote evaluated"]
  | Error _ -> reached_error ~loc:expr.loc
  | Quote body ->
    let embedded = Embedded.create () in
    (match build t ~env ~level ~embedded ~depth:1 body with
     | Ok expr ->
       Eval_result.return (Value.Code { expr; persistent = embedded.values })
     | Error error -> Eval_result.error error)
  | Tuple exprs ->
    let%mapl_fun.Local_result { global = values } =
      eval_all t ~env ~level exprs
    in
    { global = Value.Tuple values }
  | Construct (constructor, exprs) ->
    let%mapl_fun.Local_result { global = values } =
      eval_all t ~env ~level exprs
    in
    { global = Value.Construct (constructor, values) }
  | Record { record = record_type; fields } ->
    let%mapl_fun.Local_result { global = values } =
      eval_fields t ~env ~level fields
    in
    let record =
      Array.create ~len:(Array.length record_type.fields) Value.Unit
    in
    List.iter values ~f:(fun ((field : Typed.Field_ref.t), value) ->
      record.(field.index) <- value);
    { global = Value.Record (record_type, record) }
  | Deref pointer ->
    let%bindl.Local_result { global = pointer } = eval t ~env ~level pointer in
    (match pointer with
     | Ptr cell -> Eval_result.return !cell
     | Box value -> Eval_result.return value
     | _ ->
       Eval_result.error_s
         [%message "Dereferenced a non-pointer" (pointer : Value.t)])
  | Field (expr, field) ->
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    (match value with
     | Record (_, values) when field.index < Array.length values ->
       Eval_result.return values.(field.index)
     | _ ->
       Eval_result.error_s
         [%message
           "Field of a non-record" (field : Typed.Field_ref.t) (value : Value.t)])
  | Store (target, value) ->
    let%bindl.Local_result { global = place } =
      store_place t ~env ~level target
    in
    let%bindl.Local_result { global = value } = eval t ~env ~level value in
    let%bindl.Local_result (_ : unit Global.t) =
      Store_place.store place ~value |> Eval_result.of_or_error
    in
    Eval_result.return Value.Unit
  | Record_update { expr; fields; record = _ } ->
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    let%bindl.Local_result { global = values } =
      eval_fields t ~env ~level fields
    in
    (match value with
     | Record (record_type, record)
       when List.for_all values ~f:(fun ((field : Typed.Field_ref.t), _) ->
              field.index < Array.length record) ->
       let record = Array.copy record in
       List.iter values ~f:(fun ((field : Typed.Field_ref.t), value) ->
         record.(field.index) <- value);
       Eval_result.return (Value.Record (record_type, record))
     | _ ->
       Eval_result.error_s [%message "Update of a non-record" (value : Value.t)])
  | Poly_record fields ->
    let%bindl.Local_result { global = fields } =
      eval_fields t ~env ~level fields
    in
    (match String.Map.of_alist_or_error fields with
     | Ok fields -> Eval_result.return (Value.Poly_record fields)
     | Error error -> Eval_result.error error)
  | Poly_field (expr, label) ->
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    let%bindl.Local_result { global = fields } = poly_record value in
    poly_field fields label
  | Poly_extend (fields, expr) ->
    let%bindl.Local_result { global = added } =
      eval_fields t ~env ~level fields
    in
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    let%mapl_fun.Local_result { global = fields } = poly_record value in
    { global =
        Value.Poly_record
          (List.fold added ~init:fields ~f:(fun fields (key, data) ->
             Map.set fields ~key ~data))
    }
  | Poly_restrict (expr, label) ->
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    let%mapl_fun.Local_result { global = fields } = poly_record value in
    { global = Value.Poly_record (Map.remove fields label) }
  | Poly_update (expr, updates) ->
    let%bindl.Local_result { global = value } = eval t ~env ~level expr in
    let%bindl.Local_result { global = fields } = poly_record value in
    let%mapl_fun.Local_result { global = updates } =
      eval_fields t ~env ~level updates
    in
    { global =
        Value.Poly_record
          (List.fold updates ~init:fields ~f:(fun fields (key, data) ->
             Map.set fields ~key ~data))
    }
  | Poly_variant (tag, None) ->
    Eval_result.return (Value.Poly_variant (tag, Unit))
  | Poly_variant (tag, Some expr) ->
    let%mapl_fun.Local_result { global = value } = eval t ~env ~level expr in
    { global = Value.Poly_variant (tag, value) }

and eval_all t ~env ~level exprs : Value.t list Eval_result.t @ local = exclave_
  Eval_result.List.map exprs ~f:(fun expr -> exclave_ eval t ~env ~level expr)

and eval_fields
  : 'key.
  _
  -> env:_
  -> level:_
  -> ('key * Typed.Expr.t) list
  -> ('key * Value.t) list Eval_result.t @ local
  =
  fun t ~env ~level fields -> exclave_
  Eval_result.List.map fields ~f:(fun (key, expr) -> exclave_
    let%mapl_fun.Local_result { global = value } = eval t ~env ~level expr in
    { global = key, value })

and bind_let t ~env ~level ~ident ~args ~body : unit Eval_result.t @ local
  = exclave_
  Env.declare env ~ident;
  let%mapl_fun.Local_result { global = value } =
    match args with
    | [] -> eval t ~env ~level body
    | _ :: _ -> closure t ~env ~level ~ident ~args ~body
  in
  { global = Env.set env ~ident ~value }

and closure t ~env ~level ~ident ~args ~body : Value.t Eval_result.t @ local
  = exclave_
  let captures =
    Hashtbl.find_or_add t.closure_captures ident ~default:(fun () ->
      captures t ~ident ~args ~body)
  in
  match Env.of_captures env ~fallback:t.env ~captures with
  | Ok env -> Eval_result.return (Value.Function { args; body; env; level })
  | Error error -> Eval_result.error error

and apply t (f : Value.t) arg ~loc : Next.t =
  match f with
  | Function { args = param :: args; body; env; level } ->
    let env = Env.extend env in
    Env.set env ~ident:param ~value:arg;
    (match args with
     | [] -> exclave_ Eval { env; level; expr = body }
     | _ :: _ -> exclave_ Value (Function { args; body; env; level }))
  | _ -> exclave_ Next.of_local_result (apply_non_function t f arg ~loc)

and apply_non_function t (f : Value.t) arg ~loc : Value.t Eval_result.t @ local
  = exclave_
  match f with
  | Prim { prim; applied } ->
    let applied = arg :: applied in
    if List.length applied < Prim.arity prim
    then Eval_result.return (Value.Prim { prim; applied })
    else (
      match prim_args applied with
      | Null ->
        Eval_result.error_s
          [%message
            "Prim applied to a non-base value"
              (prim : Prim.t)
              (applied : Value.t list)]
      | This args ->
        (match Prim.apply prim ~output:t.output args with
         | Ok result -> Eval_result.return (Value.of_prim result)
         | Error exn -> raise_exn ~loc (prim_exception exn)))
  | External { symbol; signature; at_compile_time; applied } ->
    let applied = arg :: applied in
    if List.length applied < C_signature.arity signature
    then
      Eval_result.return
        (Value.External { symbol; signature; at_compile_time; applied })
    else (
      match at_compile_time with
      | Denied { external_; library } when Phase.is_compile_time t.phase ->
        runtime_error
          ~loc
          (Compile_time_c_calls.Policy.denial ~external_ ~library)
      | Allowed | Denied _ ->
        (match
           C_call.call t.c_functions ~symbol signature (List.rev applied)
         with
         | Ok result -> Eval_result.return result
         | Error message ->
           runtime_error
             ~loc
             (sprintf "The external %s could not be called: %s" symbol message)))
  | Intrinsic intrinsic ->
    (match intrinsic, arg with
     | Type_expr, Type ty ->
       Eval_result.return (Syntax_value.Type_expr.to_value ty)
     | ( (Type_ref_name | Constructor_ref_name | Field_ref_name | Value_ref_name)
       , Ref ref_ ) ->
       Eval_result.return (Value.String (Decl_ref.short_name ref_))
     | Type_ref_path, Ref ref_ ->
       Eval_result.return (Syntax_value.type_ref_path (Decl_ref.root_path ref_))
     | Raise, exn -> raise_exn ~loc exn
     | Ptr_make, value -> Eval_result.return (Value.Ptr (ref value))
     | Box_make, value -> Eval_result.return (Value.Box value)
     | Assume_global, value -> Eval_result.return value
     | Erase, Code { expr; persistent = _ } ->
       (match Erase.expr expr with
        | Ok expr -> Eval_result.return (Syntax_value.Expr.to_value expr)
        | Error Reached_error -> reached_error ~loc
        | Error (Inexpressible error) ->
          runtime_error
            ~loc
            (sprintf
               "%s: %s"
               (Intrinsic.public_name intrinsic)
               (Error.to_string_hum error)))
     | Type_decl, Ref { kind = Type; name } ->
       (match t.phase with
        | Compile_time { reflect } -> Eval_result.of_or_error (reflect name)
        | Run_time | Expect_test _ ->
          runtime_error
            ~loc
            (sprintf
               "%s is only available at compile time"
               (Intrinsic.public_name intrinsic)))
     | _ ->
       raise_s
         [%message
           "Intrinsic applied to an ill-typed argument"
             (intrinsic : Intrinsic.t)
             (arg : Value.t)])
  | _ -> Eval_result.error_s [%message "Applied a non-function" (f : Value.t)]

and eval_match t ~env ~level value cases ~loc : Next.t =
  match cases with
  | [] ->
    exclave_
    Next.of_local_result
      (runtime_error
         ~loc
         ("Match failure: no case matches the value "
          ^ Value.to_string_for_message value))
  | (pattern, body) :: cases ->
    (match match_pattern env pattern value with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = true } -> exclave_ Eval { env; level; expr = body }
     | Ok { global = false } ->
       exclave_ eval_match t ~env ~level value cases ~loc)

and handle t ~env ~level exn cases ~unhandled : Next.t =
  match cases with
  | [] -> exclave_ Failed unhandled
  | (pattern, body) :: cases ->
    (match match_pattern env pattern exn with
     | Error { global = error } -> exclave_ Failed error
     | Ok { global = true } -> exclave_ Eval { env; level; expr = body }
     | Ok { global = false } ->
       exclave_ handle t ~env ~level exn cases ~unhandled)

(* The code [expr] is in a quote built at [level], [depth] quotes in. Splices at
   [depth] 1 are evaluated now, and the values of persistent binders of [level]
   or below are embedded under fresh idents. Building allocates the code anyway,
   so it uses [Or_error.t]s, as [map_children] does. *)
and build t ~env ~level ~embedded ~depth expr : Typed.Expr.t Or_error.t =
  Budget.step t.budget ~loc:expr.loc ~f:(fun () ->
    build_step t ~env ~level ~embedded ~depth expr)
  [@nontail]

and build_step
  t
  ~env
  ~level
  ~(embedded : Embedded.t)
  ~depth
  (expr : Typed.Expr.t)
  : Typed.Expr.t Or_error.t
  =
  match expr.desc with
  | Var (Persistent (Local ident))
    when (Hashtbl.find_exn t.ident_info ident).level <= level ->
    let%map renamed =
      match Hashtbl.find embedded.renames ident with
      | Some renamed -> Ok renamed
      | None ->
        (match lookup t env ident ~loc:expr.loc with
         | Error { global = error } -> Error error
         | Ok { global = value } ->
           let renamed =
             fresh_like t ident ~info:(Hashtbl.find_exn t.ident_info ident)
           in
           Hashtbl.set embedded.renames ~key:ident ~data:renamed;
           Embedded.add embedded ~ident:renamed ~value;
           Ok renamed)
    in
    { expr with desc = Var (Persistent (Local renamed)) }
  | Splice { code; subs } when depth = 1 ->
    let%bind code, values = eval_code t ~env ~level code in
    let%bind (subs : Typed.Expr.t Typed.Expr.Subs.t) =
      match subs with
      | Explicit subs ->
        let%map subs =
          map_fields subs ~f:(build t ~env ~level ~embedded ~depth)
        in
        Typed.Expr.Subs.Explicit subs
      | Implicit _ -> Ok subs
    in
    Map.iteri values ~f:(fun ~key ~data ->
      Embedded.add embedded ~ident:key ~value:data);
    splice_code t code ~subs ~level:(level + 1)
  | _ ->
    map_children ~depth expr ~f:(fun ~depth ->
      build t ~env ~level ~embedded ~depth)

and eval_code t ~env ~level code =
  Eval_result.to_or_error
    (let%bindl.Local_result { global = value } = eval t ~env ~level code in
     code_of_value value) [@nontail]
;;

let in_phase t phase ~f =
  let outer = t.phase in
  t.phase <- phase;
  Exn.protect
    ~f:(fun () -> Budget.protect_depth t.budget ~f)
    ~finally:(fun () -> t.phase <- outer)
;;

let at_compile_time t ~reflect ~f =
  in_phase
    t
    (Compile_time
       { reflect =
           (fun name ->
             let%map ({ decl; decl_type } : Reflected_decl.Reflection.t) =
               reflect name
             in
             Syntax_value.reflected_decl decl ~decl_type)
       })
    ~f
;;

let splice t ~code ~level ~entry ~reflect =
  at_compile_time t ~reflect ~f:(fun () ->
    let%bind code, embedded = eval_code t ~env:(Env.create ()) ~level code in
    Map.iteri embedded ~f:(fun ~key ~data ->
      Env.set t.env ~ident:key ~value:data);
    instantiate t code ~entry)
;;

let eval_global t ~env ~level expr =
  Eval_result.to_or_error (eval t ~env ~level expr) [@nontail]
;;

let run t expr ~level ~reflect =
  at_compile_time t ~reflect ~f:(fun () ->
    eval_global t ~env:(Env.create ()) ~level expr)
;;

let run_expect_test t ({ body; _ } : Typed.Expect_test.t) ~on_expect =
  in_phase
    t
    (Expect_test { on_expect })
    ~f:(fun () ->
      eval_global t ~env:(Env.create ()) ~level:0 body |> Or_error.ignore_m)
;;

let eval_expr ?(bindings = []) t expr =
  let env = Env.create () in
  List.iter bindings ~f:(fun (ident, value) -> Env.set env ~ident ~value);
  Budget.protect_depth t.budget ~f:(fun () -> eval_global t ~env ~level:0 expr)
;;

let find t ~ident = Env.lookup t.env ~ident

let eval_toplevel t ~ident =
  Budget.protect_depth t.budget ~f:(fun () ->
    Eval_result.to_or_error (force t ident) [@nontail])
;;
