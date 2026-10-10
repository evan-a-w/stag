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
