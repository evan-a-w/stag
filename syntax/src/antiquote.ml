open! Import
open Syntax
module Kind = Traverse.Splice.Kind

type t =
  { kind : Kind.t
  ; expr : Expr.t
  ; loc : Loc.t
  ; has_subs : bool
  }
[@@deriving sexp_of]

let map ?type_expr:replace_type_expr toplevels ~f =
  let antiquote (splice : Traverse.Splice.t) expr =
    { kind = splice.kind; expr; loc = splice.loc; has_subs = splice.has_subs }
  in
  let default =
    Traverse.Visitor.default
      ~enter:(fun depth (child : Traverse.Child.t) -> depth + child.depth)
      ~into_expansions:false
  in
  let visitor =
    { default with
      expr =
        (fun v depth (child : Traverse.Child.t) e ->
          match child.splice with
          | Some splice when depth < 0 -> f (antiquote splice e)
          | Some _ | None -> Traverse.expr v depth e)
    ; type_expr =
        (fun v depth (t : Type_expr.t) ->
          match t.txt, replace_type_expr with
          | Splice e, Some replace when depth = 0 ->
            let replaced : Type_expr.t =
              replace
                (antiquote
                   { kind = Type_expr; loc = t.loc; has_subs = false }
                   e)
            in
            { t with txt = replaced.txt }
          | _ -> Traverse.type_expr v depth t)
    }
  in
  List.map toplevels ~f:(visitor.toplevel visitor 0)
;;

let iter toplevels ~f =
  ignore
    (map toplevels ~f:(fun t ->
       f t;
       t.expr)
     : Toplevel.t list)
;;

(* No program can name it: canonical names never start with [%]. *)
let placeholder_prefix = "%antiquote."

let placeholder i =
  Located.create
    ~loc:Loc.none
    (Expr.Var (Resolved (placeholder_prefix ^ Int.to_string i), Annot.none))
;;

let index (e : Expr.t) =
  match e.txt with
  | Var (Resolved name, annot) when Annot.is_none annot ->
    String.chop_prefix name ~prefix:placeholder_prefix
    |> Option.map ~f:Int.of_string
  | _ -> None
;;
