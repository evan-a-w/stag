open! Import

(* Kept on the entry's type variable, so that its instances and what it is bound
   to keep it too. *)
let key : Typecheck_error.Unusable_in_quote.t Type_equal.Id.t =
  Type_equal.Id.create ~name:"unusable_in_quote" sexp_of_opaque
;;

let rec watcher unusable =
  Watcher.create
    key
    unusable
    ~bind:(fun unusable target ->
      record ~entry:target unusable;
      Ok ())
    ~instantiate:(fun unusable ~at:_ -> Some unusable)
    ~generalize:(fun _ -> Ok ())

and record ~entry unusable =
  match (entry : Type.ty) with
  | TVar { state = Link target; _ } -> record ~entry:target unusable
  | TVar var ->
    if Option.is_none (Watcher.find var.watchers key)
    then Type.Var.set_watchers var (watcher unusable :: var.watchers)
  | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ -> ()
;;

let rec find ~entry =
  match (entry : Type.ty) with
  | TVar var ->
    (match Watcher.find var.watchers key, var.state with
     | Some unusable, _ -> Some unusable
     | None, Link target -> find ~entry:target
     | None, (Unbound _ | Generic) -> None)
  | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ -> None
;;
