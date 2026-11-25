open! Import
open Syntax
module Namespace = Traverse.Namespace

module Context = struct
  type t =
    { modules : string list
    ; locals : String.Set.t
    ; quoted : quoted option
    }

  (* Inside a struct quote, [depth] quotes in. Only its antiquotes, back at
     depth 0, hold resolved paths, and they are evaluated outside the quote. *)
  and quoted =
    { depth : int
    ; outside : t
    }

  let enter t (child : Traverse.Child.t) =
    match t.quoted, child.members with
    | Some { depth; outside }, _ ->
      let depth = depth + child.depth in
      if depth = 0 then outside else { t with quoted = Some { depth; outside } }
    | None, Some Struct_quote ->
      { t with quoted = Some { depth = child.depth; outside = t } }
    | None, Some (Module name) -> { t with modules = t.modules @ [ name ] }
    | None, Some Expansion -> t
    | None, None ->
      { t with locals = Set.union t.locals (String.Set.of_list child.binds) }
  ;;
end

let map (t : Toplevel.t) ~f =
  let default =
    Traverse.Visitor.default ~enter:Context.enter ~into_expansions:false
  in
  let visitor =
    { default with
      expr =
        (fun v ctx _ e ->
          match
            List_literal.map_expr_items e ~f:(fun item ->
              v.expr v ctx Traverse.Child.same item)
          with
          | Some e -> e
          | None -> Traverse.expr v ctx e)
    ; pattern =
        (fun v ctx p ->
          match List_literal.map_pattern_items p ~f:(v.pattern v ctx) with
          | Some p -> p
          | None -> Traverse.pattern v ctx p)
    ; decl_path =
        (fun ({ modules; locals; quoted } : Context.t) namespace path ->
          match quoted with
          | Some _ -> path
          | None ->
            let locals =
              match namespace with
              | Value -> locals
              | Type | Constructor | Field -> String.Set.empty
            in
            f namespace ~modules ~locals path)
    }
  in
  visitor.toplevel
    visitor
    { modules = []; locals = String.Set.empty; quoted = None }
    t
;;
