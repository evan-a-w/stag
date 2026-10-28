open! Import
open Or_error.Let_syntax
module Env = Env
module Value = Value

(* The values embedded by a quote being built. [renames] are its own
   embeddings, of binders it refers to. *)
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

(* What a step evaluates to: a value, or an expression in tail position, to
   evaluate in place of the step so that tail calls take no stack. *)
module Next = struct
  type t =
    | Value of Value.t
    | Eval of
        { env : Value.t Env.t
        ; level : int
        ; expr : Typed.Expr.t
        }
end

(* A toplevel not yet evaluated. *)
module Pending = struct
  type t =
    | Component of Typed.Scc.t (** None of its members' closures are bound. *)
    | Value of Typed.Expr.t
    | Failed of
        { error : Error.t
        ; uncatchable : Error.t
          (** As [error], but no [match] can handle it, as the toplevel's
            users can't handle its failure. *)
        }
end

type t =
  { sccs : Typed.Scc.t Queue.t
  ; pending : Pending.t Typed.Ident.Table.t
  ; ident_info : Typed.Ident_info.t Typed.Ident.Table.t
  ; closure_captures : Typed.Ident.Hash_set.t Typed.Ident.Table.t
  ; mutable env : Value.t Env.t
  ; stamps : Stamp_counter.t
  ; mutable budget : Budget.t
  ; mutable output : string -> unit
  ; mutable on_expect : (string Located.t -> unit) option
  ; mutable reflect : (string -> Value.t Or_error.t) option
    (** While evaluating at compile time. *)
  }

let captures t ~ident ~args ~body =
  let level (ident : Typed.Ident.t) =
    (Hashtbl.find_exn t.ident_info ident).level
  in
  let closure_level = level ident in
  let defined = Typed.Ident.Hash_set.create () in
  let captures = Typed.Ident.Hash_set.create () in
  let def ident = Hash_set.add defined ident in
  let use ~depth ident =
    if
      depth >= 0
      && (not (Hash_set.mem defined ident))
      && level ident <= closure_level
    then Hash_set.add captures ident
  in
  let rec go ~depth ({ desc; _ } : Typed.Expr.t) =
    match desc with
    | Lit _
    | Var (Free _ | Free_toplevel _ | Prim _ | Intrinsic _)
    | Expect _ | Type _ -> ()
    | Var
        ( Bound (Toplevel ident | Local ident)
        | Persistent (Toplevel ident | Local ident) ) -> use ~depth ident
    | Let { ident; args; body; in_; _ } ->
      def ident;
      List.iter args ~f:def;
      go ~depth body;
      go ~depth in_
    | Fun { args; annots = _; body } ->
      List.iter args ~f:def;
      go ~depth body
    | Annotated (expr, _) -> go ~depth expr
    | Splice { code; subs } ->
      go ~depth:(depth - 1) code;
      (match subs with
       | Explicit subs -> go_fields ~depth subs
       | Implicit providers ->
         Map.iter providers ~f:(fun (Toplevel ident | Local ident) ->
           use ~depth ident))
    | Toplevel_splice { code = _; expansion } -> go ~depth expansion
    | Match { scrutinee; cases; exception_cases } ->
      go ~depth scrutinee;
      List.iter (cases @ exception_cases) ~f:(fun (pattern, body) ->
        List.iter (Typed.Pattern.idents pattern) ~f:def;
        go ~depth body)
    | Quote expr -> go ~depth:(depth + 1) expr
    | Field (expr, _)
    | Poly_field (expr, _)
    | Poly_restrict (expr, _)
    | Poly_variant (_, Some expr) -> go ~depth expr
    | Poly_variant (_, None) -> ()
    | App (f, x) ->
      go ~depth f;
      go ~depth x
    | Tuple exprs | Construct (_, exprs) -> List.iter exprs ~f:(go ~depth)
    | Record fields -> go_fields ~depth fields
    | Record_update (expr, fields) ->
      go ~depth expr;
      go_fields ~depth fields
    | Poly_record fields -> go_fields ~depth fields
    | Poly_extend (fields, expr) | Poly_update (expr, fields) ->
      go ~depth expr;
      go_fields ~depth fields
  and go_fields : 'a. depth:int -> ('a * Typed.Expr.t) list -> unit =
    fun ~depth fields -> List.iter fields ~f:(fun (_, expr) -> go ~depth expr)
  in
  List.iter args ~f:def;
  go ~depth:0 body;
  captures
;;

let create ~stamps () =
  { sccs = Queue.create ()
  ; pending = Typed.Ident.Table.create ()
  ; ident_info = Typed.Ident.Table.create ()
  ; closure_captures = Typed.Ident.Table.create ()
  ; env = Env.create ()
  ; stamps
  ; budget = Budget.create Limits.unlimited
  ; output = Out_channel.output_string stdout
  ; on_expect = None
  ; reflect = None
  }
;;

let stamps t = t.stamps
let set_output t output = t.output <- output
let output t = t.output
let set_limits t limits = t.budget <- Budget.create limits

let add_ident_info t ~ident ~info =
  Hashtbl.set t.ident_info ~key:ident ~data:info
;;

let add_scc t (scc : Typed.Scc.t) =
  Queue.enqueue t.sccs scc;
  List.iter scc.members ~f:(fun { ident; _ } ->
    Hashtbl.set t.pending ~key:ident ~data:(Component scc))
;;

let global_info t ~ident =
  if Env.mem t.env ident then Hashtbl.find t.ident_info ident else None
;;

let map_result list ~f =
  List.fold_result list ~init:[] ~f:(fun acc x ->
    let%map y = f x in
    y :: acc)
  >>| List.rev
;;

let iter_result list ~f = List.fold_result list ~init:() ~f:(fun () x -> f x)

let map_fields fields ~f =
  map_result fields ~f:(fun (key, expr) ->
    let%map expr = f expr in
    key, expr)
;;

(* [expr] with [f ~depth] applied to its children, where [depth] is that of
   [expr], counting quotes up and splices down. Children are visited in order. *)
let map_children ~depth (expr : Typed.Expr.t) ~f =
  let same = f ~depth in
  let%map desc : Typed.Expr.t Typed.Expr.Desc.t =
    match expr.desc with
    | (Lit _ | Var _ | Poly_variant (_, None) | Expect _ | Type _) as desc ->
      Ok desc
    | App (a, b) ->
      let%bind a = same a in
      let%map b = same b in
      Typed.Expr.Desc.App (a, b)
    | Let let_ ->
      let%bind body = same let_.body in
      let%map in_ = same let_.in_ in
      Typed.Expr.Desc.Let { let_ with body; in_ }
    | Fun fun_ ->
      let%map body = same fun_.body in
      Typed.Expr.Desc.Fun { fun_ with body }
    | Annotated (expr, ty) ->
      let%map expr = same expr in
      Typed.Expr.Desc.Annotated (expr, ty)
    | Splice { code; subs } ->
      let%bind code = f ~depth:(depth - 1) code in
      let%map subs : Typed.Expr.t Typed.Expr.Subs.t =
        match subs with
        | Explicit subs ->
          let%map subs = map_fields subs ~f:same in
          Typed.Expr.Subs.Explicit subs
        | Implicit _ -> Ok subs
      in
      Typed.Expr.Desc.Splice { code; subs }
    | Toplevel_splice { code; expansion } ->
      let%bind code = f ~depth:(depth - 1) code in
      let%map expansion = same expansion in
      Typed.Expr.Desc.Toplevel_splice { code; expansion }
    | Quote body ->
      let%map body = f ~depth:(depth + 1) body in
      Typed.Expr.Desc.Quote body
    | Tuple exprs ->
      let%map exprs = map_result exprs ~f:same in
      Typed.Expr.Desc.Tuple exprs
    | Construct (constructor, exprs) ->
      let%map exprs = map_result exprs ~f:same in
      Typed.Expr.Desc.Construct (constructor, exprs)
    | Record fields ->
      let%map fields = map_fields fields ~f:same in
      Typed.Expr.Desc.Record fields
    | Field (expr, field) ->
      let%map expr = same expr in
      Typed.Expr.Desc.Field (expr, field)
    | Record_update (expr, fields) ->
      let%bind expr = same expr in
      let%map fields = map_fields fields ~f:same in
      Typed.Expr.Desc.Record_update (expr, fields)
    | Match { scrutinee; cases; exception_cases } ->
      let%bind scrutinee = same scrutinee in
      let%bind cases = map_fields cases ~f:same in
      let%map exception_cases = map_fields exception_cases ~f:same in
      Typed.Expr.Desc.Match { scrutinee; cases; exception_cases }
    | Poly_record fields ->
      let%map fields = map_fields fields ~f:same in
      Typed.Expr.Desc.Poly_record fields
    | Poly_field (expr, label) ->
      let%map expr = same expr in
      Typed.Expr.Desc.Poly_field (expr, label)
    | Poly_extend (fields, expr) ->
      let%bind fields = map_fields fields ~f:same in
      let%map expr = same expr in
      Typed.Expr.Desc.Poly_extend (fields, expr)
    | Poly_restrict (expr, label) ->
      let%map expr = same expr in
      Typed.Expr.Desc.Poly_restrict (expr, label)
    | Poly_update (expr, fields) ->
      let%bind expr = same expr in
      let%map fields = map_fields fields ~f:same in
      Typed.Expr.Desc.Poly_update (expr, fields)
    | Poly_variant (tag, Some expr) ->
      let%map expr = same expr in
      Typed.Expr.Desc.Poly_variant (tag, Some expr)
  in
  { expr with desc }
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
  let rename_binder : Typed.Binder.t -> Typed.Binder.t = function
    | Local ident ->
      Local (Hashtbl.find renames ident |> Option.value ~default:ident)
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
  and go_step ~depth (expr : Typed.Expr.t) =
    let desc : Typed.Expr.t Typed.Expr.Desc.t =
      match expr.desc with
      | Var ((Free name | Free_toplevel name) as var_ref) when depth = 0 ->
        Var (Option.value (entry name) ~default:var_ref)
      | Var (Bound binder) -> Var (Bound (rename_binder binder))
      | Var (Persistent binder) -> Var (Persistent (rename_binder binder))
      | Let let_ ->
        let ident = fresh let_.ident in
        let args = List.map let_.args ~f:fresh in
        Let { let_ with ident; args; annots = rename_annots let_.annots }
      | Fun fun_ ->
        let args = List.map fun_.args ~f:fresh in
        Fun { fun_ with args; annots = rename_annots fun_.annots }
      | Match { scrutinee; cases; exception_cases } ->
        let rename = List.map ~f:(fun (p, body) -> pattern p, body) in
        Match
          { scrutinee
          ; cases = rename cases
          ; exception_cases = rename exception_cases
          }
      | Splice { code; subs = Implicit providers } ->
        Splice { code; subs = Implicit (Map.map providers ~f:rename_binder) }
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
    let subs =
      List.map subs ~f:(fun (name, (sub : Typed.Expr.t)) ->
        let ident =
          fresh_like
            t
            { name; stamp = 0 }
            ~info:{ Typed.Ident_info.ty = sub.ty; level }
        in
        name, ident, sub)
    in
    let idents =
      List.map subs ~f:(fun (name, ident, _) -> name, ident)
      |> String.Map.of_alist_exn
    in
    let%map code =
      instantiate t code ~entry:(fun name ->
        Map.find idents name
        |> Option.map ~f:(fun ident -> Typed.Var_ref.Bound (Local ident)))
    in
    List.fold_right subs ~init:code ~f:(fun (_, ident, sub) in_ ->
      { Typed.Expr.desc =
          Let
            { ident
            ; args = []
            ; annots = []
            ; result_annot = None
            ; generalized = false
            ; body = sub
            ; in_
            }
      ; ty = in_.ty
      ; loc = in_.loc
      })
;;

let runtime_error ~loc message =
  Error (Runtime_error.to_error (Runtime_error.create ~loc message))
;;

let raise_exn ~loc exn =
  Error (Runtime_error.to_error (Runtime_error.raise_exn ~loc exn))
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

let pattern_mismatch pattern value =
  Or_error.error_s
    [%message
      "Pattern does not fit value" (pattern : Typed.Pattern.t) (value : Value.t)]
;;

(* Binds the variables of [pattern] in [env] if it matches [value]. *)
let rec match_pattern env (pattern : Typed.Pattern.t) (value : Value.t) =
  match pattern.desc, value with
  | Any, _ -> Ok true
  | Var ident, _ ->
    Env.set env ~ident ~value;
    Ok true
  | Lit (Int a), Int b -> Ok (Int.equal a b)
  | Lit (Bool a), Bool b -> Ok (Bool.equal a b)
  | Lit (String a), String b -> Ok (String.equal a b)
  | Lit (Char a), Char b -> Ok (Char.equal a b)
  | Lit Unit, Unit -> Ok true
  | Tuple patterns, Tuple values -> match_all env pattern patterns values
  | Construct (constructor, patterns), Construct (constructor', values) ->
    if
      constructor.index = constructor'.index
      && ((not (Typed.Constructor_ref.is_exception constructor))
          || String.equal constructor.name constructor'.name)
    then match_all env pattern patterns values
    else Ok false
  | Record fields, Record (_, values) ->
    if
      List.exists fields ~f:(fun ((field : Typed.Field_ref.t), _) ->
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
    then Ok false
    else (
      match arg with
      | None -> Ok true
      | Some arg -> match_pattern env arg value)
  | Poly_record { fields; rest }, Poly_record values ->
    (match
       List.map fields ~f:(fun (label, p) ->
         Map.find values label |> Option.map ~f:(fun v -> p, v))
       |> Option.all
     with
     | None -> pattern_mismatch pattern value
     | Some pairs ->
       let%map matched =
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
       matched)
  | ( (Lit _ | Tuple _ | Construct _ | Record _ | Poly_variant _ | Poly_record _)
    , _ ) -> pattern_mismatch pattern value

and match_all env pattern patterns values =
  match List.zip patterns values with
  | Unequal_lengths -> pattern_mismatch pattern (Tuple values)
  | Ok pairs ->
    List.fold_until
      pairs
      ~init:()
      ~f:(fun () (p, v) ->
        match match_pattern env p v with
        | Ok true -> Continue ()
        | Ok false -> Stop (Ok false)
        | Error _ as error -> Stop error)
      ~finish:(fun () -> Ok true)
;;

let poly_record (value : Value.t) =
  match value with
  | Poly_record fields -> Ok fields
  | _ -> Or_error.error_s [%message "Expected a record" (value : Value.t)]
;;

let poly_field fields label =
  match Map.find fields label with
  | Some value -> Ok value
  | None ->
    Or_error.error_s
      [%message "Missing field" label (fields : Value.t String.Map.t)]
;;

let code_of_value (value : Value.t) =
  match value with
  | Code { expr; persistent } -> Ok (expr, persistent)
  | _ ->
    Or_error.error_s [%message "Spliced a non-code value" (value : Value.t)]
;;

(* Closures capturing a toplevel not yet evaluated share its slot, so it is
   kept. *)
let declare_toplevel t ident =
  if not (Env.mem t.env ident) then Env.declare t.env ~ident
;;

(* Toplevels are evaluated on first use, so compile time evaluation only runs
   what it needs. *)
let rec force t ident =
  match Hashtbl.find t.pending ident with
  | None -> Ok ()
  | Some (Component scc) ->
    let%bind () = bind_functions t scc in
    force t ident
  | Some (Failed { error; uncatchable = _ }) -> Error error
  | Some (Value body) ->
    Hashtbl.remove t.pending ident;
    declare_toplevel t ident;
    (match eval t ~env:(Env.create ()) ~level:0 body with
     | Ok value ->
       Env.set t.env ~ident ~value;
       Ok ()
     | Error error ->
       let uncatchable =
         match Runtime_error.of_error error with
         | Some ({ kind = Raised exn; loc } : Runtime_error.t) ->
           Runtime_error.to_error
             (Runtime_error.create
                ~loc
                (sprintf
                   "Initialization of %s raised %s"
                   ident.name
                   (Value.to_string_for_message exn)))
         | Some { kind = Message _; loc = _ } | None -> error
       in
       Hashtbl.set t.pending ~key:ident ~data:(Failed { error; uncatchable });
       Error error)

and lookup t env ident ~loc =
  let%bind () =
    match force t ident with
    | Ok () -> Ok ()
    | Error error ->
      (match Hashtbl.find t.pending ident with
       | Some (Failed { uncatchable; error = _ }) -> Error uncatchable
       | Some (Component _ | Value _) | None -> Error error)
  in
  match Env.find_slot ~fallback:t.env env ident with
  | Some { contents = None } ->
    runtime_error
      ~loc
      (sprintf
         "%s is used before its value is defined (a value may not depend on \
          itself)"
         ident.name)
  | Some _ | None -> Env.lookup env ~fallback:t.env ~ident

(* Making closures reads nothing, so a component's functions are all bound at
   once, and its values evaluated each when first used. *)
and bind_functions t ({ members; recursive } : Typed.Scc.t) =
  let functions, values =
    List.partition_tf members ~f:(fun { args; _ } -> not (List.is_empty args))
  in
  List.iter values ~f:(fun { ident; body; _ } ->
    Hashtbl.set t.pending ~key:ident ~data:(Value body));
  List.iter functions ~f:(fun { ident; _ } ->
    Hashtbl.remove t.pending ident;
    if recursive then declare_toplevel t ident);
  iter_result functions ~f:(fun { ident; args; body; _ } ->
    let%map value = closure t ~env:t.env ~level:0 ~ident ~args ~body in
    Env.set t.env ~ident ~value)

(* [env] has the bindings of the current activation, at [level]. *)
and eval t ~env ~level expr =
  Budget.step t.budget ~loc:expr.loc ~f:(fun () ->
    run_next t (step t ~env ~level expr))

and run_next t (next : Next.t Or_error.t) =
  match next with
  | Error _ as error -> error
  | Ok (Value value) -> Ok value
  | Ok (Eval { env; level; expr }) ->
    (match Budget.tail_step t.budget ~loc:expr.loc with
     | Error _ as error -> error
     | Ok () -> run_next t (step t ~env ~level expr))

and step t ~env ~level (expr : Typed.Expr.t) : Next.t Or_error.t =
  let eval' = eval t ~env ~level in
  let tail expr = Ok (Next.Eval { env; level; expr }) in
  match expr.desc with
  | App (f, arg) ->
    let%bind f = eval' f in
    let%bind arg = eval' arg in
    apply t f arg ~loc:expr.loc
  | Annotated (expr, _) -> tail expr
  | Let { ident; args; body; in_; _ } ->
    let%bind () = bind_let t ~env ~level ~ident ~args ~body in
    tail in_
  | Toplevel_splice { code = _; expansion } -> tail expansion
  | Match { scrutinee; cases; exception_cases } ->
    (match eval' scrutinee with
     | Ok value -> eval_match t ~env ~level value cases ~loc:expr.loc
     | Error error ->
       (match exception_cases, Runtime_error.raised error with
        | _ :: _, Some exn ->
          handle t ~env ~level exn exception_cases ~unhandled:(Error error)
        | _ -> Error error))
  | _ ->
    let%map value = step_value t ~env ~level expr in
    Next.Value value

and step_value t ~env ~level (expr : Typed.Expr.t) : Value.t Or_error.t =
  let eval' = eval t ~env ~level in
  match expr.desc with
  | Lit lit -> Ok (Value.of_lit lit)
  | Expect expected ->
    (match t.on_expect with
     | Some on_expect ->
       on_expect expected;
       Ok Value.Unit
     | None ->
       Or_error.error_s [%message "[%expect] outside a running expect test"])
  | Var (Bound (Local ident | Toplevel ident))
  | Var (Persistent (Local ident | Toplevel ident)) ->
    lookup t env ident ~loc:expr.loc
  | Var (Free name | Free_toplevel name) ->
    Or_error.error_s [%message "Free variable of code evaluated" name]
  | Var (Prim prim) -> Ok (Value.Prim { prim; applied = [] })
  | Var (Intrinsic intrinsic) -> Ok (Value.Intrinsic intrinsic)
  | Type ty -> Ok (Value.Type ty)
  | App _ | Let _ | Annotated _ | Toplevel_splice _ | Match _ ->
    run_next t (step t ~env ~level expr)
  | Fun { args; annots = _; body } ->
    (* Its first parameter identifies it, as a let's name does. *)
    closure t ~env ~level ~ident:(List.hd_exn args) ~args ~body
  | Splice _ -> Or_error.error_s [%message "Splice outside a quote evaluated"]
  | Quote body ->
    let embedded = Embedded.create () in
    let%map expr = build t ~env ~level ~embedded ~depth:1 body in
    Value.Code { expr; persistent = embedded.values }
  | Tuple exprs ->
    let%map values = map_result exprs ~f:eval' in
    Value.Tuple values
  | Construct (constructor, exprs) ->
    let%map values = map_result exprs ~f:eval' in
    Value.Construct (constructor, values)
  | Record fields ->
    let%map values = map_fields fields ~f:eval' in
    let record_type = (fst (List.hd_exn values) : Typed.Field_ref.t).record in
    let record =
      Array.create ~len:(Array.length record_type.fields) Value.Unit
    in
    List.iter values ~f:(fun ((field : Typed.Field_ref.t), value) ->
      record.(field.index) <- value);
    Value.Record (record_type, record)
  | Field (expr, field) ->
    let%bind value = eval' expr in
    (match value with
     | Record (_, values) when field.index < Array.length values ->
       Ok values.(field.index)
     | _ ->
       Or_error.error_s
         [%message
           "Field of a non-record" (field : Typed.Field_ref.t) (value : Value.t)])
  | Record_update (expr, fields) ->
    let%bind value = eval' expr in
    let%bind values = map_fields fields ~f:eval' in
    (match value with
     | Record (record_type, record)
       when List.for_all values ~f:(fun ((field : Typed.Field_ref.t), _) ->
              field.index < Array.length record) ->
       let record = Array.copy record in
       List.iter values ~f:(fun ((field : Typed.Field_ref.t), value) ->
         record.(field.index) <- value);
       Ok (Value.Record (record_type, record))
     | _ ->
       Or_error.error_s [%message "Update of a non-record" (value : Value.t)])
  | Poly_record fields ->
    let%bind fields = map_fields fields ~f:eval' in
    let%map fields = String.Map.of_alist_or_error fields in
    Value.Poly_record fields
  | Poly_field (expr, label) ->
    let%bind fields = eval' expr >>= poly_record in
    poly_field fields label
  | Poly_extend (fields, expr) ->
    let%bind added = map_fields fields ~f:eval' in
    let%map fields = eval' expr >>= poly_record in
    Value.Poly_record
      (List.fold added ~init:fields ~f:(fun fields (key, data) ->
         Map.set fields ~key ~data))
  | Poly_restrict (expr, label) ->
    let%map fields = eval' expr >>= poly_record in
    Value.Poly_record (Map.remove fields label)
  | Poly_update (expr, updates) ->
    let%bind fields = eval' expr >>= poly_record in
    let%map updates = map_fields updates ~f:eval' in
    Value.Poly_record
      (List.fold updates ~init:fields ~f:(fun fields (key, data) ->
         Map.set fields ~key ~data))
  | Poly_variant (tag, None) -> Ok (Value.Poly_variant (tag, Unit))
  | Poly_variant (tag, Some expr) ->
    let%map value = eval' expr in
    Value.Poly_variant (tag, value)

and bind_let t ~env ~level ~ident ~args ~body =
  Env.declare env ~ident;
  let%map value =
    match args with
    | [] -> eval t ~env ~level body
    | _ :: _ -> closure t ~env ~level ~ident ~args ~body
  in
  Env.set env ~ident ~value

and closure t ~env ~level ~ident ~args ~body =
  let captures =
    Hashtbl.find_or_add t.closure_captures ident ~default:(fun () ->
      captures t ~ident ~args ~body)
  in
  Hash_set.iter captures ~f:(fun ident ->
    if Hashtbl.mem t.pending ident then declare_toplevel t ident);
  let%map env = Env.of_captures env ~fallback:t.env ~captures in
  Value.Function { args; body; env; level }

and apply t (f : Value.t) arg ~loc : Next.t Or_error.t =
  let value value = Ok (Next.Value value) in
  match f with
  | Prim { prim; applied } ->
    let applied = arg :: applied in
    if List.length applied < Prim.arity prim
    then value (Value.Prim { prim; applied })
    else (
      match List.rev applied |> List.map ~f:Value.to_prim |> Option.all with
      | None ->
        Or_error.error_s
          [%message
            "Prim applied to a non-base value"
              (prim : Prim.t)
              (applied : Value.t list)]
      | Some args ->
        (match Prim.apply prim ~output:t.output args with
         | Ok result -> value (Value.of_prim result)
         | Error exn -> raise_exn ~loc (prim_exception exn)))
  | Intrinsic intrinsic ->
    (match intrinsic, arg with
     | Type_expr, Type ty -> value (Syntax_value.Type_expr.to_value ty)
     | ( (Type_ref_name | Constructor_ref_name | Field_ref_name | Value_ref_name)
       , Ref ref_ ) -> value (String (Decl_ref.short_name ref_))
     | Type_ref_path, Ref ref_ ->
       value (Syntax_value.type_ref_path (Decl_ref.root_path ref_))
     | Raise, exn -> raise_exn ~loc exn
     | Erase, Code { expr; persistent = _ } ->
       (match Erase.expr expr with
        | Ok expr -> value (Syntax_value.Expr.to_value expr)
        | Error error ->
          runtime_error
            ~loc
            (sprintf
               "%s: %s"
               (Intrinsic.public_name intrinsic)
               (Error.to_string_hum error)))
     | Type_decl, Ref { kind = Type; name } ->
       (match t.reflect with
        | Some reflect ->
          let%map decl = reflect name in
          Next.Value decl
        | None ->
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
  | Function { args = param :: args; body; env; level } ->
    let env = Env.copy env in
    Env.set env ~ident:param ~value:arg;
    (match args with
     | [] -> Ok (Next.Eval { env; level; expr = body })
     | _ :: _ -> value (Value.Function { args; body; env; level }))
  | _ -> Or_error.error_s [%message "Applied a non-function" (f : Value.t)]

and eval_match t ~env ~level value cases ~loc =
  match cases with
  | [] ->
    runtime_error
      ~loc
      ("Match failure: no case matches the value "
       ^ Value.to_string_for_message value)
  | (pattern, body) :: cases ->
    (match%bind match_pattern env pattern value with
     | true -> Ok (Next.Eval { env; level; expr = body })
     | false -> eval_match t ~env ~level value cases ~loc)

and handle t ~env ~level exn cases ~unhandled =
  match cases with
  | [] -> unhandled
  | (pattern, body) :: cases ->
    (match%bind match_pattern env pattern exn with
     | true -> Ok (Next.Eval { env; level; expr = body })
     | false -> handle t ~env ~level exn cases ~unhandled)

(* The code [expr] is in a quote built at [level], [depth] quotes in. Splices at
   [depth] 1 are evaluated now, and the values of persistent binders of [level]
   or below are embedded under fresh idents. *)
and build t ~env ~level ~embedded ~depth expr =
  Budget.step t.budget ~loc:expr.loc ~f:(fun () ->
    build_step t ~env ~level ~embedded ~depth expr)

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
        let%map value = lookup t env ident ~loc:expr.loc in
        let renamed =
          fresh_like t ident ~info:(Hashtbl.find_exn t.ident_info ident)
        in
        Hashtbl.set embedded.renames ~key:ident ~data:renamed;
        Embedded.add embedded ~ident:renamed ~value;
        renamed
    in
    { expr with desc = Var (Persistent (Local renamed)) }
  | Splice { code; subs } when depth = 1 ->
    let%bind code, values = eval t ~env ~level code >>= code_of_value in
    let%bind subs : Typed.Expr.t Typed.Expr.Subs.t =
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
;;

let at_compile_time t ~reflect ~f =
  let outer = t.reflect in
  t.reflect <- Some reflect;
  Exn.protect ~f ~finally:(fun () -> t.reflect <- outer)
;;

let splice t ~code ~level ~entry ~reflect =
  at_compile_time t ~reflect ~f:(fun () ->
    let%bind code, embedded =
      eval t ~env:(Env.create ()) ~level code >>= code_of_value
    in
    Map.iteri embedded ~f:(fun ~key ~data ->
      Env.set t.env ~ident:key ~value:data);
    instantiate t code ~entry)
;;

let run t expr ~level ~reflect =
  at_compile_time t ~reflect ~f:(fun () ->
    eval t ~env:(Env.create ()) ~level expr)
;;

let run_expect_test t ({ body; _ } : Typed.Expect_test.t) ~on_expect =
  t.on_expect <- Some on_expect;
  Exn.protect
    ~f:(fun () ->
      eval t ~env:(Env.create ()) ~level:0 body |> Or_error.ignore_m)
    ~finally:(fun () -> t.on_expect <- None)
;;

let eval_expr ?(bindings = []) t expr =
  let env = Env.create () in
  List.iter bindings ~f:(fun (ident, value) -> Env.set env ~ident ~value);
  eval t ~env ~level:0 expr
;;

let find t ~ident = Env.lookup t.env ~ident

let eval t =
  let rec loop () =
    match Queue.dequeue t.sccs with
    | None -> Ok ()
    | Some scc ->
      let%bind () =
        iter_result scc.members ~f:(fun { ident; _ } -> force t ident)
      in
      loop ()
  in
  loop ()
;;

let eval_toplevel t ~ident = force t ident
