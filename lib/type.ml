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
  | CExtend of string * ty * context
  | CVar of cvar ref

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
  | CExtend (_, _, _)
  | CVar { contents = CGeneric _ | CUnbound (_, _) } -> context
  | CVar ({ contents = CLink context' } as cvar) ->
    let context'' = canonical_context context' in
    cvar := CLink context'';
    context''
;;

let rec tail_var context =
  match context with
  | CEmpty -> None
  | CExtend (_, _, context') -> tail_var context'
  | CVar { contents = CLink context' } -> tail_var context'
  | CVar ({ contents = CGeneric _ | CUnbound (_, _) } as cvar) -> Some cvar
;;

let to_string_hum ty =
  let names = Int.Table.create () in
  let var_name id ~weak =
    let index =
      Hashtbl.find_or_add names id ~default:(fun () -> Hashtbl.length names)
    in
    let letter = Char.of_int_exn (Char.to_int 'a' + (index % 26)) in
    let suffix = if index >= 26 then Int.to_string (index / 26) else "" in
    sprintf "'%s%c%s" (if weak then "_" else "") letter suffix
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
      | CEmpty -> []
      | CExtend (name, ty, rest) ->
        let entry = sprintf "%s : %s" name (ty_to_string ~prec:0 ty) in
        entry :: entries rest
      | CVar { contents = CGeneric id } -> [ ".." ^ var_name id ~weak:false ]
      | CVar { contents = CUnbound (id, _) } ->
        [ ".." ^ var_name id ~weak:true ]
      | CVar { contents = CLink _ } -> assert false
    in
    "{" ^ String.concat ~sep:"; " (entries context) ^ "}"
  in
  ty_to_string ~prec:0 ty
;;
