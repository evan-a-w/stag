open! Import
open Syntax

let cons = Decl_path.Resolved (Prelude_path.canonical "Cons")
let nil = Decl_path.Resolved (Prelude_path.canonical "Nil")
let ptr_make = Decl_path.Resolved (Prelude_path.canonical "Ptr.make")

module Node = struct
  type 'a t =
    { construct : Decl_path.t Located.t -> 'a Located.t list -> 'a
    ; destruct : 'a -> (Decl_path.t Located.t * 'a Located.t list) option
    ; pair : Loc.t -> 'a Located.t -> 'a Located.t -> 'a Located.t
    ; unpair : 'a Located.t -> ('a Located.t * 'a Located.t) option
    }

  let expr : Expr.desc t =
    { construct = (fun c args -> Construct (c, args))
    ; destruct =
        (function
          | Construct (c, args) -> Some (c, args)
          | _ -> None)
    ; pair =
        (fun loc x xs ->
          let make = Located.create ~loc (Expr.Var (ptr_make, Annot.none)) in
          Located.create
            ~loc
            (Expr.App (make, Located.create ~loc (Expr.Tuple [ x; xs ]))))
    ; unpair =
        (function
          | { txt =
                App
                  ({ txt = Var (name, annot); _ }, { txt = Tuple [ x; xs ]; _ })
            ; _
            }
            when Decl_path.equal name ptr_make && Annot.is_none annot ->
            Some (x, xs)
          | _ -> None)
    }
  ;;

  let pattern : Pattern.desc t =
    { construct = (fun c args -> Construct (c, args))
    ; destruct =
        (function
          | Construct (c, args) -> Some (c, args)
          | _ -> None)
    ; pair =
        (fun loc x xs ->
          Located.create
            ~loc
            (Pattern.Deref (Located.create ~loc (Pattern.Tuple [ x; xs ]))))
    ; unpair =
        (function
          | { txt = Deref { txt = Tuple [ x; xs ]; _ }; _ } -> Some (x, xs)
          | _ -> None)
    }
  ;;
end

let build (node : _ Node.t) ~items ~close =
  let at loc txt = { Located.txt; loc } in
  let rec go = function
    | [] -> at close (node.construct (at close nil) [])
    | (before, item) :: rest ->
      let loc = Loc.merge before close in
      at loc (node.construct (at before cons) [ node.pair loc item (go rest) ])
  in
  go items
;;

(* The [Cons]es of a literal and the [Nil] ending it, if it is one. *)
let spine (node : _ Node.t) (t : _ Located.t) =
  let rec go (t : _ Located.t) conses =
    match node.destruct t.txt with
    | Some ({ txt; _ }, []) when Decl_path.equal txt nil ->
      Some (List.rev conses, t)
    | Some ({ txt; _ }, [ pair ]) when Decl_path.equal txt cons ->
      Option.bind (node.unpair pair) ~f:(fun (item, rest) ->
        go rest ((t, item) :: conses))
    | _ -> None
  in
  go t []
;;

let items node t =
  Option.map (spine node t) ~f:(fun (conses, _) -> List.map conses ~f:snd)
;;

let map_items (node : _ Node.t) t ~f =
  Option.map (spine node t) ~f:(fun (conses, nil) ->
    List.fold_right
      conses
      ~init:nil
      ~f:(fun ((cons : _ Located.t), item) tail ->
        let c, _ = Option.value_exn (node.destruct cons.txt) in
        { cons with
          txt = node.construct c [ node.pair cons.loc (f item) tail ]
        }))
;;

let expr ~items ~close = build Node.expr ~items ~close
let pattern ~items ~close = build Node.pattern ~items ~close
let expr_items = items Node.expr
let pattern_items = items Node.pattern
let map_expr_items = map_items Node.expr
let map_pattern_items = map_items Node.pattern
