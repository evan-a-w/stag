open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

module Var = struct
  type 'a state =
    | Unbound of rank
    | Generic
    | Link of 'a

  type ('a, 'bound) t =
    { id : id
    ; mutable state : 'a state
    ; mutable bounds : 'bound list
    }

  let create id state = { id; state; bounds = [] }

  let rank_exn t =
    match t.state with
    | Unbound rank -> rank
    | Generic | Link _ ->
      raise_s [%message "Type.Var.rank_exn: variable not unbound" (t.id : id)]
  ;;

  let sexp_of_t sexp_of_a _ t =
    match t.state with
    | Unbound rank -> [%sexp "Unbound", (t.id : id), (rank : rank)]
    | Generic -> [%sexp "Generic", (t.id : id)]
    | Link a -> [%sexp "Link", (a : a)]
  ;;

  let rec canonical x ~var =
    match var x with
    | Some ({ state = Link x'; _ } as t) ->
      let x'' = canonical x' ~var in
      t.state <- Link x'';
      x''
    | Some { state = Unbound _ | Generic; _ } | None -> x
  ;;
end

type ty =
  | TCon of string * arg list
  | TArrow of ty * ty
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : context
      }
  | TVar of (ty, Nothing.t) Var.t

and context =
  | CEmpty
  | CExtend of string * presence * ty * context
  | CVar of (context, bound) Var.t

and presence =
  | Present
  | Absent
  | PVar of (presence, Nothing.t) Var.t

and arg =
  | ATy of ty
  | AContext of context

and bound =
  { context : context
  ; shadowed : String.Set.t
  }
[@@deriving sexp_of]

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let char = TCon ("char", [])
let unit = TCon ("unit", [])

let canonical_ty =
  Var.canonical ~var:(function
    | TVar var -> Some var
    | TCon _ | TArrow _ | TTuple _ | TCode _ -> None)
;;

let canonical_context =
  Var.canonical ~var:(function
    | CVar var -> Some var
    | CEmpty | CExtend _ -> None)
;;

let canonical_presence =
  Var.canonical ~var:(function
    | PVar var -> Some var
    | Present | Absent -> None)
;;

let rec tail_var context =
  match context with
  | CEmpty -> None
  | CExtend (_, _, _, context') | CVar { state = Link context'; _ } ->
    tail_var context'
  | CVar var -> Some var
;;

module Mapper = struct
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; context : (context, bound) Var.t -> context
    ; presence : (presence, Nothing.t) Var.t -> presence
    }
end

let rec map_ty (f : Mapper.t) ty =
  match canonical_ty ty with
  | TCon (name, args) ->
    TCon
      ( name
      , List.map args ~f:(function
          | ATy ty -> ATy (map_ty f ty)
          | AContext context -> AContext (map_context f context)) )
  | TArrow (ty1, ty2) -> TArrow (map_ty f ty1, map_ty f ty2)
  | TTuple tys -> TTuple (List.map tys ~f:(map_ty f))
  | TCode { inner; context } ->
    TCode { inner = map_ty f inner; context = map_context f context }
  | TVar var -> f.ty var

and map_context f context =
  match canonical_context context with
  | CEmpty -> CEmpty
  | CExtend (name, presence, ty, rest) ->
    CExtend (name, map_presence f presence, map_ty f ty, map_context f rest)
  | CVar var -> f.context var

and map_presence f presence =
  match canonical_presence presence with
  | (Present | Absent) as presence -> presence
  | PVar var -> f.presence var
;;

module Iter = struct
  type 'e t =
    { ty : (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; context : (context, bound) Var.t -> (unit, 'e) Result.t
    ; presence : (presence, Nothing.t) Var.t -> (unit, 'e) Result.t
    }
end

let rec iter_ty (f : _ Iter.t) ty =
  match canonical_ty ty with
  | TCon (_, args) ->
    List.fold_result args ~init:() ~f:(fun () -> function
      | ATy ty -> iter_ty f ty
      | AContext context -> iter_context f context)
  | TArrow (ty1, ty2) ->
    let%bind.Result () = iter_ty f ty1 in
    iter_ty f ty2
  | TTuple tys -> List.fold_result tys ~init:() ~f:(fun () -> iter_ty f)
  | TCode { inner; context } ->
    let%bind.Result () = iter_ty f inner in
    iter_context f context
  | TVar var -> f.ty var

and iter_context f context =
  match canonical_context context with
  | CEmpty -> Ok ()
  | CExtend (_, presence, ty, rest) ->
    let%bind.Result () = iter_presence f presence in
    let%bind.Result () = iter_ty f ty in
    iter_context f rest
  | CVar var -> f.context var

and iter_presence f presence =
  match canonical_presence presence with
  | Present | Absent -> Ok ()
  | PVar var -> f.presence var
;;

module Printer = struct
  type t =
    { type_name : string -> string
    ; names : int Int.Table.t
    ; bounded : (string * (context, bound) Var.t) Queue.t
      (* Bounded context variables, in order of appearance, to print their
         bounds after the types. *)
    }

  let create ?(type_name = Fn.id) () =
    { type_name; names = Int.Table.create (); bounded = Queue.create () }
  ;;

  let var_name t (var : _ Var.t) =
    let index =
      Hashtbl.find_or_add t.names var.id ~default:(fun () ->
        Hashtbl.length t.names)
    in
    let letter = Char.of_int_exn (Char.to_int 'a' + (index % 26)) in
    let suffix = if index >= 26 then Int.to_string (index / 26) else "" in
    let weak =
      match var.state with
      | Unbound _ -> true
      | Generic -> false
      | Link _ -> assert false
    in
    sprintf "'%s%c%s" (if weak then "_" else "") letter suffix
  ;;

  let context_var_name t (var : _ Var.t) =
    let first_appearance = not (Hashtbl.mem t.names var.id) in
    let name = ".." ^ var_name t var in
    if first_appearance && not (List.is_empty var.bounds)
    then Queue.enqueue t.bounded (name, var);
    name
  ;;

  let rec ty_to_string t ~prec ty =
    let parens_if cond s = if cond then "(" ^ s ^ ")" else s in
    match canonical_ty ty with
    | TCon (name, []) -> t.type_name name
    | TCon (name, args) ->
      sprintf
        "%s[%s]"
        (t.type_name name)
        (List.map args ~f:(function
           | ATy ty -> ty_to_string t ~prec:0 ty
           | AContext context -> context_to_string t context)
         |> String.concat ~sep:", ")
    | TArrow (ty1, ty2) ->
      let ty1 = ty_to_string t ~prec:1 ty1 in
      let ty2 = ty_to_string t ~prec:0 ty2 in
      parens_if (prec > 0) (sprintf "%s -> %s" ty1 ty2)
    | TTuple tys ->
      parens_if
        (prec > 1)
        (List.map tys ~f:(ty_to_string t ~prec:2) |> String.concat ~sep:" * ")
    | TCode { inner; context } ->
      let inner = ty_to_string t ~prec:0 inner in
      sprintf "<%s>%s" inner (context_to_string t context)
    | TVar var -> var_name t var

  and context_to_string t context =
    let rec entries context =
      match canonical_context context with
      | CEmpty -> [], None
      | CExtend (name, presence, ty, rest) ->
        let entries, tail = entries rest in
        (name, presence, ty) :: entries, tail
      | CVar var -> [], Some var
    in
    let entry (name, presence, ty) =
      let ty () = ty_to_string t ~prec:0 ty in
      match canonical_presence presence with
      | Present -> sprintf "%s : %s" name (ty ())
      | Absent -> "-" ^ name
      | PVar var ->
        let presence = var_name t var in
        sprintf "%s?%s : %s" name presence (ty ())
    in
    let fields, tail = entries context in
    let fields =
      List.stable_sort fields ~compare:(fun (a, _, _) (b, _, _) ->
        String.compare a b)
      |> List.map ~f:entry
    in
    let tail = Option.map tail ~f:(context_var_name t) |> Option.to_list in
    "{" ^ String.concat ~sep:"; " (fields @ tail) ^ "}"
  ;;

  let ty t ty = ty_to_string t ~prec:0 ty

  let rec bounds t =
    match Queue.dequeue t.bounded with
    | None -> []
    | Some (name, var) ->
      let these =
        List.map var.bounds ~f:(fun bound ->
          sprintf "%s <= %s" name (context_to_string t bound.context))
      in
      these @ bounds t
  ;;

  let where t =
    match bounds t with
    | [] -> ""
    | bounds -> " where " ^ String.concat ~sep:", " bounds
  ;;
end

let to_string_hum ty =
  let printer = Printer.create () in
  let ty = Printer.ty printer ty in
  ty ^ Printer.where printer
;;
