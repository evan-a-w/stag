open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

type ty =
  | TCon of string * arg list
  | TArrow of ty * ty
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : context
      }
  | TVar of tvar ref

and tvar =
  | Link of ty
  | Generic of id
  | Unbound of id * rank

and cvar =
  | CLink of context
  | CGeneric of id
  | CUnbound of id * rank

and context =
  | CEmpty
  | CExtend of string * presence * ty * context
  | CVar of cvar ref

and presence =
  | Present
  | Absent
  | PVar of pvar ref

and pvar =
  | PLink of presence
  | PGeneric of id
  | PUnbound of id * rank

and arg =
  | ATy of ty
  | AContext of context
[@@deriving sexp, compare, equal]

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let char = TCon ("char", [])
let unit = TCon ("unit", [])

module Var = struct
  type t =
    | TVar of tvar ref
    | CVar of cvar ref
  [@@deriving sexp, compare, equal]
end

let rec canonical_ty ty =
  match ty with
  | TCon _
  | TArrow (_, _)
  | TTuple _ | TCode _
  | TVar { contents = Generic _ | Unbound (_, _) } -> ty
  | TVar ({ contents = Link ty' } as tvar) ->
    let ty'' = canonical_ty ty' in
    tvar := Link ty'';
    ty''
;;

let rec canonical_context context =
  match context with
  | CEmpty
  | CExtend (_, _, _, _)
  | CVar { contents = CGeneric _ | CUnbound (_, _) } -> context
  | CVar ({ contents = CLink context' } as cvar) ->
    let context'' = canonical_context context' in
    cvar := CLink context'';
    context''
;;

let rec canonical_presence presence =
  match presence with
  | Present | Absent | PVar { contents = PGeneric _ | PUnbound (_, _) } ->
    presence
  | PVar ({ contents = PLink presence' } as pvar) ->
    let presence'' = canonical_presence presence' in
    pvar := PLink presence'';
    presence''
;;

let rec tail_var context =
  match context with
  | CEmpty -> None
  | CExtend (_, _, _, context') -> tail_var context'
  | CVar { contents = CLink context' } -> tail_var context'
  | CVar ({ contents = CGeneric _ | CUnbound (_, _) } as cvar) -> Some cvar
;;

let to_string_hum ?(context_bounds = fun (_ : id) -> []) ty =
  let names = Int.Table.create () in
  let var_name id ~weak =
    let index =
      Hashtbl.find_or_add names id ~default:(fun () -> Hashtbl.length names)
    in
    let letter = Char.of_int_exn (Char.to_int 'a' + (index % 26)) in
    let suffix = if index >= 26 then Int.to_string (index / 26) else "" in
    sprintf "'%s%c%s" (if weak then "_" else "") letter suffix
  in
  (* Bounded context variables, in order of appearance, to print their bounds
     after the type. *)
  let bounded = Queue.create () in
  let context_var_name id ~weak =
    let first_appearance = not (Hashtbl.mem names id) in
    let name = ".." ^ var_name id ~weak in
    if first_appearance && not (List.is_empty (context_bounds id))
    then Queue.enqueue bounded (name, id);
    name
  in
  let rec ty_to_string ~prec ty =
    let parens_if cond s = if cond then "(" ^ s ^ ")" else s in
    match canonical_ty ty with
    | TCon (name, []) -> name
    | TCon (name, args) ->
      sprintf
        "%s[%s]"
        name
        (List.map args ~f:(function
           | ATy ty -> ty_to_string ~prec:0 ty
           | AContext context -> context_to_string context)
         |> String.concat ~sep:", ")
    | TArrow (ty1, ty2) ->
      let ty1 = ty_to_string ~prec:1 ty1 in
      let ty2 = ty_to_string ~prec:0 ty2 in
      parens_if (prec > 0) (sprintf "%s -> %s" ty1 ty2)
    | TTuple tys ->
      parens_if
        (prec > 1)
        (List.map tys ~f:(ty_to_string ~prec:2) |> String.concat ~sep:" * ")
    | TCode { inner; context } ->
      let inner = ty_to_string ~prec:0 inner in
      sprintf "<%s>%s" inner (context_to_string context)
    | TVar { contents = Generic id } -> var_name id ~weak:false
    | TVar { contents = Unbound (id, _) } -> var_name id ~weak:true
    | TVar { contents = Link _ } -> assert false
  and context_to_string context =
    let rec entries context =
      match canonical_context context with
      | CEmpty -> [], None
      | CExtend (name, presence, ty, rest) ->
        let entries, tail = entries rest in
        (name, presence, ty) :: entries, tail
      | CVar { contents = CGeneric id } -> [], Some (id, false)
      | CVar { contents = CUnbound (id, _) } -> [], Some (id, true)
      | CVar { contents = CLink _ } -> assert false
    in
    let entry (name, presence, ty) =
      let ty () = ty_to_string ~prec:0 ty in
      match canonical_presence presence with
      | Present -> sprintf "%s : %s" name (ty ())
      | Absent -> "-" ^ name
      | PVar { contents = PGeneric id } ->
        let presence = var_name id ~weak:false in
        sprintf "%s?%s : %s" name presence (ty ())
      | PVar { contents = PUnbound (id, _) } ->
        let presence = var_name id ~weak:true in
        sprintf "%s?%s : %s" name presence (ty ())
      | PVar { contents = PLink _ } -> assert false
    in
    let fields, tail = entries context in
    let fields =
      List.stable_sort fields ~compare:(fun (a, _, _) (b, _, _) ->
        String.compare a b)
      |> List.map ~f:entry
    in
    let tail =
      Option.map tail ~f:(fun (id, weak) -> context_var_name id ~weak)
      |> Option.to_list
    in
    "{" ^ String.concat ~sep:"; " (fields @ tail) ^ "}"
  in
  let ty = ty_to_string ~prec:0 ty in
  let rec constraints () =
    match Queue.dequeue bounded with
    | None -> []
    | Some (name, id) ->
      let bounds =
        List.map (context_bounds id) ~f:(fun bound ->
          sprintf "%s <= %s" name (context_to_string bound))
      in
      bounds @ constraints ()
  in
  match constraints () with
  | [] -> ty
  | constraints -> ty ^ " where " ^ String.concat ~sep:", " constraints
;;
