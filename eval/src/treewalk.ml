open! Import
open Or_error.Let_syntax

module Env = struct
  type 'value t = { values : 'value option ref Typed.Ident.Table.t }

  let create () = { values = Typed.Ident.Table.create () }
  let copy t = { values = Hashtbl.copy t.values }

  let find_slot ?fallback t ident =
    match Hashtbl.find t.values ident with
    | Some _ as slot -> slot
    | None ->
      Option.bind fallback ~f:(fun fallback ->
        Hashtbl.find fallback.values ident)
  ;;

  let lookup ?fallback t ~ident =
    match find_slot ?fallback t ident with
    | None ->
      Or_error.error_s [%message "Unbound ident" (ident : Typed.Ident.t)]
    | Some { contents = None } ->
      Or_error.error_s
        [%message
          "Used value before it was defined (eg. because of recursion on \
           values)"
            (ident : Typed.Ident.t)]
    | Some { contents = Some value } -> Ok value
  ;;

  let declare t ~ident = Hashtbl.set t.values ~key:ident ~data:(ref None)

  let set t ~ident ~value =
    let slot =
      Hashtbl.find_or_add t.values ident ~default:(fun () -> ref None)
    in
    slot := Some value
  ;;

  let of_captures ?fallback t ~captures =
    let env = create () in
    let%map () =
      Hash_set.to_list captures
      |> List.fold_result ~init:() ~f:(fun () ident ->
        match find_slot ?fallback t ident with
        | Some slot -> Ok (Hashtbl.set env.values ~key:ident ~data:slot)
        | None ->
          Or_error.error_s
            [%message "Captured ident not in scope" (ident : Typed.Ident.t)])
    in
    env
  ;;
end

module Value = struct
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | Tuple of t list
    | Construct of Typed.Constructor_ref.t * t list
    | Record of t array
    | Poly_record of t String.Map.t
    | Poly_variant of string * t
    | Function of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; env : t Env.t
        ; level : int
        }
    | Code of
        { expr : Typed.Expr.t
        ; persistent : t Typed.Ident.Map.t
        }
    | Prim of
        { prim : Prim.t
        ; applied : t list
        }

  let of_prim : Prim.Value.t -> t = function
    | Int n -> Int n
    | Bool b -> Bool b
    | String s -> String s
    | Char c -> Char c
    | Unit -> Unit
  ;;

  let to_prim : t -> Prim.Value.t option = function
    | Int n -> Some (Int n)
    | Bool b -> Some (Bool b)
    | String s -> Some (String s)
    | Char c -> Some (Char c)
    | Unit -> Some Unit
    | Tuple _
    | Construct _
    | Record _
    | Poly_record _
    | Poly_variant _
    | Function _
    | Code _
    | Prim _ -> None
  ;;

  let of_lit : Syntax.Lit.t -> t = function
    | Int n -> Int n
    | Bool b -> Bool b
    | String s -> String s
    | Char c -> Char c
    | Unit -> Unit
  ;;

  let rec sexp_of_t : t -> Sexp.t = function
    | Int n -> Atom (Int.to_string n)
    | Bool b -> sexp_of_bool b
    | String s -> List [ Atom "String"; Atom s ]
    | Char c -> List [ Atom "Char"; sexp_of_char c ]
    | Unit -> Atom "()"
    | Tuple values -> List (Atom "Tuple" :: List.map values ~f:sexp_of_t)
    | Construct ({ name; _ }, values) ->
      List (Atom name :: List.map values ~f:sexp_of_t)
    | Record values ->
      List (Atom "Record" :: Array.to_list (Array.map values ~f:sexp_of_t))
    | Poly_record fields -> [%sexp (fields : t String.Map.t)]
    | Poly_variant (tag, Unit) -> Atom ("`" ^ tag)
    | Poly_variant (tag, arg) -> List [ Atom ("`" ^ tag); sexp_of_t arg ]
    | Function _ | Prim _ -> Atom "<fun>"
    | Code { expr; persistent } ->
      List
        (Atom "Code"
         :: Typed.Expr.sexp_of_t expr
         ::
         (if Map.is_empty persistent
          then []
          else [ [%sexp (persistent : t Typed.Ident.Map.t)] ]))
  ;;

  let rec to_string_hum : t -> string = function
    | Int n -> Int.to_string n
    | Bool b -> Bool.to_string b
    | String s -> sprintf "%S" s
    | Char c -> sprintf "%C" c
    | Unit -> "()"
    | Tuple values -> tuple values
    | Construct ({ name; _ }, []) -> name
    | Construct ({ name; _ }, [ value ]) -> name ^ " " ^ argument value
    | Construct ({ name; _ }, values) -> name ^ " " ^ tuple values
    | Record _ -> "<record>"
    | Poly_record fields ->
      Map.to_alist fields
      |> List.map ~f:(fun (label, value) ->
        sprintf "`%s = %s" label (to_string_hum value))
      |> String.concat ~sep:"; "
      |> sprintf "{%s}"
    | Poly_variant (tag, Unit) -> "`" ^ tag
    | Poly_variant (tag, value) -> sprintf "`%s %s" tag (argument value)
    | Function _ | Prim _ -> "<fun>"
    | Code _ -> "<code>"

  and tuple values =
    sprintf "(%s)" (String.concat ~sep:", " (List.map values ~f:to_string_hum))

  and argument (value : t) =
    match value with
    | Construct (_, []) | Poly_variant (_, Unit) -> to_string_hum value
    | Construct _ | Poly_variant _ -> "(" ^ to_string_hum value ^ ")"
    | _ -> to_string_hum value
  ;;
end

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

(* A toplevel not yet evaluated. *)
module Pending = struct
  type t =
    | Component of Typed.Scc.t (** None of its members' closures are bound. *)
    | Value of Typed.Expr.t
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
  let rec def_pattern ({ desc; _ } : Typed.Pattern.t) =
    match desc with
    | Any | Lit _ | Poly_variant (_, None) -> ()
    | Var ident -> def ident
    | Tuple patterns | Construct (_, patterns) ->
      List.iter patterns ~f:def_pattern
    | Record fields -> List.iter fields ~f:(fun (_, p) -> def_pattern p)
    | Poly_variant (_, Some pattern) -> def_pattern pattern
    | Poly_record { fields; rest } ->
      List.iter fields ~f:(fun (_, p) -> def_pattern p);
      (match rest with
       | Closed | Open -> ()
       | Bind ident -> def ident)
  in
  let rec go ~depth ({ desc; _ } : Typed.Expr.t) =
    match desc with
    | Lit _ | Var (Free _ | Prim _) | Expect _ -> ()
    | Var
        ( Bound (Toplevel ident | Local ident)
        | Persistent (Toplevel ident | Local ident) ) -> use ~depth ident
    | Let { ident; args; generalized = _; body; in_ } ->
      def ident;
      List.iter args ~f:def;
      go ~depth body;
      go ~depth in_
    | Splice { code; subs } ->
      go ~depth:(depth - 1) code;
      (match subs with
       | Explicit subs -> go_fields ~depth subs
       | Implicit providers ->
         Map.iter providers ~f:(fun (Toplevel ident | Local ident) ->
           use ~depth ident))
    | Toplevel_splice { code = _; expansion } -> go ~depth expansion
    | Match (expr, cases) ->
      go ~depth expr;
      List.iter cases ~f:(fun (pattern, body) ->
        def_pattern pattern;
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
  if Hashtbl.mem t.env.values ident
  then Hashtbl.find t.ident_info ident
  else None
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
    | (Lit _ | Var _ | Poly_variant (_, None) | Expect _) as desc -> Ok desc
    | App (a, b) ->
      let%bind a = same a in
      let%map b = same b in
      Typed.Expr.Desc.App (a, b)
    | Let let_ ->
      let%bind body = same let_.body in
      let%map in_ = same let_.in_ in
      Typed.Expr.Desc.Let { let_ with body; in_ }
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
    | Match (expr, cases) ->
      let%bind expr = same expr in
      let%map cases = map_fields cases ~f:same in
      Typed.Expr.Desc.Match (expr, cases)
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
      | Var (Free name) when depth = 0 ->
        Var (Option.value (entry name) ~default:(Typed.Var_ref.Free name))
      | Var (Bound binder) -> Var (Bound (rename_binder binder))
      | Var (Persistent binder) -> Var (Persistent (rename_binder binder))
      | Let let_ ->
        let ident = fresh let_.ident in
        Let { let_ with ident; args = List.map let_.args ~f:fresh }
      | Match (expr, cases) ->
        Match (expr, List.map cases ~f:(fun (p, body) -> pattern p, body))
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
          Let { ident; args = []; generalized = false; body = sub; in_ }
      ; ty = in_.ty
      ; loc = in_.loc
      })
;;

let runtime_error ~loc message = Error (Runtime_error.to_error { loc; message })

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
    if constructor.index = constructor'.index
    then match_all env pattern patterns values
    else Ok false
  | Record fields, Record values ->
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
  if not (Hashtbl.mem t.env.values ident) then Env.declare t.env ~ident
;;

(* Toplevels are evaluated on first use, so compile time evaluation only runs
   what it needs. *)
let rec force t ident =
  match Hashtbl.find t.pending ident with
  | None -> Ok ()
  | Some (Component scc) ->
    let%bind () = bind_functions t scc in
    force t ident
  | Some (Value body) ->
    Hashtbl.remove t.pending ident;
    declare_toplevel t ident;
    let%map value = eval t ~env:(Env.create ()) ~level:0 body in
    Env.set t.env ~ident ~value

and lookup t env ident ~loc =
  let%bind () = force t ident in
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
  Budget.step t.budget ~loc:expr.loc ~f:(fun () -> step t ~env ~level expr)

and step t ~env ~level (expr : Typed.Expr.t) : Value.t Or_error.t =
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
  | Var (Free name) ->
    Or_error.error_s [%message "Free variable of code evaluated" name]
  | Var (Prim prim) -> Ok (Value.Prim { prim; applied = [] })
  | App (f, arg) ->
    let%bind f = eval' f in
    let%bind arg = eval' arg in
    apply t f arg ~loc:expr.loc
  | Let { ident; args; generalized = _; body; in_ } ->
    let%bind () = bind_let t ~env ~level ~ident ~args ~body in
    eval' in_
  | Splice _ -> Or_error.error_s [%message "Splice outside a quote evaluated"]
  | Toplevel_splice { code = _; expansion } -> eval' expansion
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
    let record = Array.create ~len:(List.length values) Value.Unit in
    List.iter values ~f:(fun ((field : Typed.Field_ref.t), value) ->
      record.(field.index) <- value);
    Value.Record record
  | Field (expr, field) ->
    let%bind value = eval' expr in
    (match value with
     | Record values when field.index < Array.length values ->
       Ok values.(field.index)
     | _ ->
       Or_error.error_s
         [%message
           "Field of a non-record" (field : Typed.Field_ref.t) (value : Value.t)])
  | Match (scrutinee, cases) ->
    let%bind value = eval' scrutinee in
    eval_match t ~env ~level value cases ~loc:expr.loc
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

and apply t (f : Value.t) arg ~loc =
  match f with
  | Prim { prim; applied } ->
    let applied = arg :: applied in
    if List.length applied < Prim.arity prim
    then Ok (Value.Prim { prim; applied })
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
         | Ok value -> Ok (Value.of_prim value)
         | Error message -> runtime_error ~loc message))
  | Function { args = param :: args; body; env; level } ->
    let env = Env.copy env in
    Env.set env ~ident:param ~value:arg;
    (match args with
     | [] -> eval t ~env ~level body
     | _ :: _ -> Ok (Value.Function { args; body; env; level }))
  | _ -> Or_error.error_s [%message "Applied a non-function" (f : Value.t)]

and eval_match t ~env ~level value cases ~loc =
  match cases with
  | [] ->
    runtime_error
      ~loc
      ("Match failure: no case matches the value " ^ Value.to_string_hum value)
  | (pattern, body) :: cases ->
    (match%bind match_pattern env pattern value with
     | true -> eval t ~env ~level body
     | false -> eval_match t ~env ~level value cases ~loc)

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

let splice t ~code ~level ~entry =
  let%bind code, embedded =
    eval t ~env:(Env.create ()) ~level code >>= code_of_value
  in
  Map.iteri embedded ~f:(fun ~key ~data -> Env.set t.env ~ident:key ~value:data);
  instantiate t code ~entry
;;

let run_expect_test t ({ body; _ } : Typed.Expect_test.t) ~on_expect =
  t.on_expect <- Some on_expect;
  Exn.protect
    ~f:(fun () ->
      eval t ~env:(Env.create ()) ~level:0 body |> Or_error.ignore_m)
    ~finally:(fun () -> t.on_expect <- None)
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
