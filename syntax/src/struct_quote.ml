open! Import
open Syntax

exception Error of Loc.t * string

(* Its value, of the type of a splice of its kind. *)
module Bound = struct
  type t =
    { antiquote : Antiquote.t
    ; name : string
    }

  let type_ (kind : Antiquote.Kind.t) ~loc : Type_expr.t =
    let con ?(args = []) name : Type_expr.t =
      Located.create
        ~loc
        (Type_expr.Con
           ( Located.create
               ~loc
               (Decl_path.Resolved (Prelude_path.canonical name))
           , args ))
    in
    match kind with
    | Item -> con "list" ~args:[ Ty (con "Stag.Toplevel.t") ]
    | Type_expr -> con "Stag.Type_expr.t"
    | Type_kind -> con "Stag.Type_decl.Kind.t"
    | Expr -> con "Stag.Expr.t"
  ;;

  (* Valid, so that erased code holding the expansion is syntax, and in no
     antiquote, so that none captures the bindings before it. *)
  let prefix antiquotes =
    let sources =
      List.map antiquotes ~f:(fun (antiquote : Antiquote.t) ->
        Printer.expr antiquote.expr)
    in
    let rec go prefix =
      if
        List.exists sources ~f:(fun source ->
          String.is_substring source ~substring:prefix)
      then go (prefix ^ "_")
      else prefix
    in
    go "antiquote_"
  ;;

  let var t ~loc : Expr.t =
    Located.create ~loc (Expr.Var (Path (Path.name t.name), None))
  ;;

  let bind t ~(in_ : Expr.t) : Expr.t =
    let loc = t.antiquote.loc in
    Located.create
      ~loc:in_.loc
      (Expr.Let
         ( Located.create ~loc t.name
         , []
         , Some (type_ t.antiquote.kind ~loc)
         , t.antiquote.expr
         , in_ ))
  ;;
end

let expand_exn toplevels ~loc =
  let bound = Queue.create () in
  let toplevels =
    Antiquote.map toplevels ~f:(fun antiquote ->
      if antiquote.has_subs
      then
        raise
          (Error
             ( antiquote.loc
             , "An antiquote of a struct quote takes no substitutions: write \
                `.~e`, of a `Stag.Expr.t`" ));
      let index = Queue.length bound in
      Queue.enqueue bound antiquote;
      Antiquote.placeholder index)
  in
  let prefix = Bound.prefix (Queue.to_list bound) in
  let bound =
    Queue.mapi bound ~f:(fun i antiquote ->
      { Bound.antiquote; name = sprintf "%s%d" prefix i })
  in
  let node txt = Located.create ~loc txt in
  let resolved name = Decl_path.Resolved (Prelude_path.canonical name) in
  let module Lift = Lift.Make (struct
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

      let ref_ (_ : Decl_ref.Kind.t) name =
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
  let lifted =
    Lift.program toplevels ~antiquote:(fun _ e ->
      Antiquote.index e
      |> Option.map ~f:(fun i ->
        let bound = Queue.get bound i in
        Bound.var bound ~loc:bound.antiquote.loc))
  in
  List.fold_right (Queue.to_list bound) ~init:lifted ~f:(fun bound in_ ->
    Bound.bind bound ~in_)
;;

let rec check_no_expansions (toplevels : Toplevel.t list) =
  List.iter toplevels ~f:(fun toplevel ->
    match toplevel.txt with
    | Include (_, Some _) | Module_splice (_, _, Some _) ->
      raise
        (Error
           ( toplevel.loc
           , "A struct quote can't hold an inline expansion: generated \
              declarations have none" ))
    | Module (_, members) -> check_no_expansions members
    | Include _
    | Module_splice _
    | Let _
    | Type _
    | Exception _
    | Malformed_exception _
    | Expect_test _
    | Bench _ -> ())
;;

let expand toplevels ~loc =
  match
    check_no_expansions toplevels;
    expand_exn toplevels ~loc
  with
  | expr -> Ok expr
  | exception Error (loc, message) -> Error (loc, message)
;;
