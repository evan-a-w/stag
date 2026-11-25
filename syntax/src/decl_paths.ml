open! Import
open Syntax
module Namespace = Traverse.Namespace

module Context = struct
  type t =
    { modules : string list
    ; locals : String.Set.t
    ; in_struct_quote : bool
    }

  (* A struct quote's declarations are in no module of the program, so are bound
     as locals, and its modules are not the enclosing toplevel's. *)
  let enter t (child : Traverse.Child.t) =
    let in_struct_quote =
      t.in_struct_quote
      || [%equal: Traverse.Members.t option] child.members (Some Struct_quote)
    in
    let locals () = Set.union t.locals (String.Set.of_list child.binds) in
    match child.members with
    | Some (Module name) when not in_struct_quote ->
      { t with modules = t.modules @ [ name ] }
    | Some (Module _ | Expansion) when not in_struct_quote -> t
    | Some (Module _ | Expansion | Struct_quote) | None ->
      { t with locals = locals (); in_struct_quote }
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
        (fun ({ modules; locals; in_struct_quote = _ } : Context.t)
          namespace
          path ->
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
    { modules = []; locals = String.Set.empty; in_struct_quote = false }
    t
;;
