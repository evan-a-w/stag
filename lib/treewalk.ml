open! Core

module Env = struct
  type 'value t =
    { prev : 'value t option
    ; values : 'value option ref Typed.Ident.Table.t
    }
  [@@deriving sexp_of, fields]

  let rec lookup t ~ident =
    match Hashtbl.find t.values ident with
    | None ->
      (match t.prev with
       | None -> Ok None
       | Some prev -> lookup ~ident prev)
    | Some { contents = None } ->
      Or_error.error_s
        [%message
          "Used value before it was defined (eg. because of recursion on \
           values)"
            (ident : Typed.Ident.t)]
    | Some { contents } -> Ok contents
  ;;

  let create () = { prev = None; values = Typed.Ident.Table.create () }

  let set t ~ident ~value = 
    let ref = Hashtbl.find_or_add t.values ident ~default:(fun () -> ref None) in
    ref := Some value


  let push t =
    let res = create () in
    { res with prev = Some t }
  ;;

  let pop t = Option.value_or_thunk t.prev ~default:create
end

module Value = struct
  type t =
    | Int of string
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | Record of t String.Map.t
    | Variant of string * t
    | Function of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; env : t Env.t
        }
  [@@deriving sexp_of, variants]
end

type t =
  { eval_stack : Typed.Scc.t Stack.t
  ; ident_info : Typed.Ident_info.t Typed.Ident.Table.t
  ; values : Value.t Typed.Ident.Table.t
  ; closure_captures : Typed.Ident.Hash_set.t Typed.Ident.Table.t
  ; mutable env : Value.t Env.t
  }

let captures t ~ident ~args ~body =
  Hashtbl.find_or_add t.closure_captures ident ~default:(fun () ->
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
      | Lit _ | Var (Free _) -> ()
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
    captures)
;;

let create () =
  { eval_stack = Stack.create ()
  ; ident_info = Typed.Ident.Table.create ()
  ; values = Typed.Ident.Table.create ()
  ; closure_captures = Typed.Ident.Table.create ()
  ; env = { prev = None; values = Typed.Ident.Table.create () }
  }
;;

let add_program t ~program:({ sccs; ident_info } : Typed.Program.t) =
  List.rev sccs |> List.iter ~f:(Stack.push t.eval_stack);
  Map.iteri ident_info ~f:(fun ~key ~data ->
    Hashtbl.set t.ident_info ~key ~data)
;;

let create' ~program =
  let t = create () in
  add_program t ~program;
  t
;;

(* module Toplevel : sig
  type t =
    { ident : Ident.t
    ; args : Ident.t list
    ; scheme : Type.ty
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end *)

let make_env t ~captures =
  let env = Env.create () in
  Hash_set.to_list captures |> List.iter ~f:(fun ident -> 
    Env.set env ~ident ~value:(Env.lookup t.env ~ident)
    );
    env

(* ident already in scope before this, but not filled in *)
let rec eval t ~ident ~args ~body = 

    | Function of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; env : t Env.t
        }


let eval_one_scc t ~(scc : Typed.Scc.t) =
  List.iter scc.members ~f:(fun { ident; _ } ->
    Hashtbl.set t.env.values ~key:ident ~data:(ref None));
    List.map scc.members ~f:(fun { ident; args; body; scheme = _ } -> 
      
      eval t ~ident ~args ~body
      ) |> Result.all_unit

;;

let eval t = ()
