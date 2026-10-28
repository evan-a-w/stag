open! Import
open Syntax

let cons = Decl_path.Resolved (Prelude_path.canonical "Cons")
let nil = Decl_path.Resolved (Prelude_path.canonical "Nil")

module Node = struct
  type 'a t =
    { construct : Decl_path.t Located.t -> 'a Located.t list -> 'a
    ; destruct : 'a -> (Decl_path.t Located.t * 'a Located.t list) option
    }

  let expr : Expr.desc t =
    { construct = (fun c args -> Construct (c, args))
    ; destruct =
        (function
          | Construct (c, args) -> Some (c, args)
          | _ -> None)
    }
  ;;

  let pattern : Pattern.desc t =
    { construct = (fun c args -> Construct (c, args))
    ; destruct =
        (function
          | Construct (c, args) -> Some (c, args)
          | _ -> None)
    }
  ;;
end

let build (node : _ Node.t) ~items ~close =
  let at loc txt = { Located.txt; loc } in
  let rec go = function
    | [] -> at close (node.construct (at close nil) [])
    | (before, item) :: rest ->
      at
        (Loc.merge before close)
        (node.construct (at before cons) [ item; go rest ])
  in
  go items
;;

(* The [Cons]es of a literal and the [Nil] ending it, if it is one. *)
let spine (node : _ Node.t) (t : _ Located.t) =
  let rec go (t : _ Located.t) conses =
    match node.destruct t.txt with
    | Some ({ txt; _ }, []) when Decl_path.equal txt nil ->
      Some (List.rev conses, t)
    | Some ({ txt; _ }, [ item; rest ]) when Decl_path.equal txt cons ->
      go rest ((t, item) :: conses)
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
        { cons with txt = node.construct c [ f item; tail ] }))
;;

let expr ~items ~close = build Node.expr ~items ~close
let pattern ~items ~close = build Node.pattern ~items ~close
let expr_items = items Node.expr
let pattern_items = items Node.pattern
let map_expr_items = map_items Node.expr
let map_pattern_items = map_items Node.pattern
