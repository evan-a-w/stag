open! Import
open Syntax

exception Error of Loc.t * string

let expand_exn ?(types_only = false) toplevels ~loc =
  let antiquotes = Queue.create () in
  let toplevels =
    Antiquote.map toplevels ~f:(fun antiquote ->
      match antiquote.kind with
      | (Item | Expr) when types_only -> antiquote.expr
      | Item | Expr | Type_expr | Type_kind ->
      if antiquote.has_subs
      then
        raise
          (Error
             ( antiquote.loc
             , "An antiquote of a struct quote takes no substitutions: write \
                `.~e`, of a `Stag.Expr.t`" ));
      Queue.enqueue antiquotes antiquote.expr;
      Antiquote.placeholder (Queue.length antiquotes - 1))
  in
  let node txt = Located.create ~loc txt in
  let resolved name = Decl_path.Resolved (Prelude_path.canonical name) in
  let module Lift =
    Lift.Make (struct
      type t = Expr.t

      let con name args =
        node (Expr.Construct (Located.create ~loc (resolved name), args))
      ;;

      let int n = node (Expr.Lit (Int n))
      let bool b = node (Expr.Lit (Bool b))
      let string s = node (Expr.Lit (String s))
      let char c = node (Expr.Lit (Char c))
      let tuple es = node (Expr.Tuple es)

      let record fields =
        node
          (Expr.Poly_record
             (List.map fields ~f:(fun (name, e) -> Located.create ~loc name, e)))
      ;;

      let ref_ (_ : Lift.Ref_kind.t) name =
        raise
          (Error
             ( loc
             , sprintf
                 "A struct quote can't hold the resolved reference %s"
                 (Prelude_path.to_string_hum name) ))
      ;;

      let append a b =
        let append = node (Expr.Var (resolved "Stag.append", None)) in
        node (Expr.App (node (Expr.App (append, a)), b))
      ;;
    end)
  in
  Lift.program toplevels ~antiquote:(fun _ e ->
    Antiquote.index e |> Option.map ~f:(Queue.get antiquotes))
;;

let expand toplevels ~loc =
  match expand_exn toplevels ~loc with
  | expr -> Ok expr
  | exception Error (loc, message) -> Error (loc, message)
;;

let of_type_splices (toplevel : Toplevel.t) =
  match toplevel.txt with
  | Module _ | Module_splice _ | Include _ -> None
  | Let _ | Type _ | Exception _ | Expect_test _ | Bench _ ->
    let has_type_splice = ref false in
    Antiquote.iter [ toplevel ] ~f:(fun antiquote ->
      match antiquote.kind with
      | Type_expr | Type_kind -> has_type_splice := true
      | Item | Expr -> ());
    if !has_type_splice
    then Some (expand_exn ~types_only:true [ toplevel ] ~loc:toplevel.loc)
    else None
;;
