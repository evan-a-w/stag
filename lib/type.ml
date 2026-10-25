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
  | CExtend of string * presence * ty * context
  | CVar of cvar ref

and presence =
  | Present
  | Absent
  | PVar of pvar ref

and pvar =
  | PLink of presence
  | PGeneric of id
  | PUnbound of id * rank

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
  | CExtend (_, _, _, _)
  | CVar { contents = CGeneric _ | CUnbound (_, _) } -> context
  | CVar ({ contents = CLink context' } as cvar) ->
    let context'' = canonical_context context' in
    cvar := CLink context'';
    context''
;;

let rec canonical_presence presence =
  match presence with
  | Present | Absent | PVar { contents = PGeneric _ | PUnbound (_, _) } ->
    presence
  | PVar ({ contents = PLink presence' } as pvar) ->
    let presence'' = canonical_presence presence' in
    pvar := PLink presence'';
    presence''
;;

let rec tail_var context =
  match context with
  | CEmpty -> None
  | CExtend (_, _, _, context') -> tail_var context'
  | CVar { contents = CLink context' } -> tail_var context'
  | CVar ({ contents = CGeneric _ | CUnbound (_, _) } as cvar) -> Some cvar
;;

module Printer = struct
  type t =
    { context_bounds : id -> context list
    ; type_name : string -> string
    ; names : int Int.Table.t
    ; bounded : (string * id) Queue.t
      (* Bounded context variables, in order of appearance, to print their
         bounds after the types. *)
    }

  let create ?(context_bounds = fun (_ : id) -> []) ?(type_name = Fn.id) () =
    { context_bounds
    ; type_name
    ; names = Int.Table.create ()
    ; bounded = Queue.create ()
    }
  ;;

  let var_name t id ~weak =
    let index =
      Hashtbl.find_or_add t.names id ~default:(fun () -> Hashtbl.length t.names)
    in
    let letter = Char.of_int_exn (Char.to_int 'a' + (index % 26)) in
    let suffix = if index >= 26 then Int.to_string (index / 26) else "" in
    sprintf "'%s%c%s" (if weak then "_" else "") letter suffix
  ;;

  let context_var_name t id ~weak =
    let first_appearance = not (Hashtbl.mem t.names id) in
    let name = ".." ^ var_name t id ~weak in
    if first_appearance && not (List.is_empty (t.context_bounds id))
    then Queue.enqueue t.bounded (name, id);
    name
  ;;

  let rec ty_to_string t ~prec ty =
    let parens_if cond s = if cond then "(" ^ s ^ ")" else s in
    match canonical_ty ty with
    | TCon (name, []) -> t.type_name name
    | TCon (name, args) ->
      sprintf
        "%s[%s]"
        (t.type_name name)
        (List.map args ~f:(function
           | ATy ty -> ty_to_string t ~prec:0 ty
           | AContext context -> context_to_string t context)
         |> String.concat ~sep:", ")
    | TArrow (ty1, ty2) ->
      let ty1 = ty_to_string t ~prec:1 ty1 in
      let ty2 = ty_to_string t ~prec:0 ty2 in
      parens_if (prec > 0) (sprintf "%s -> %s" ty1 ty2)
    | TTuple tys ->
      parens_if
        (prec > 1)
        (List.map tys ~f:(ty_to_string t ~prec:2) |> String.concat ~sep:" * ")
    | TCode { inner; context } ->
      let inner = ty_to_string t ~prec:0 inner in
      sprintf "<%s>%s" inner (context_to_string t context)
    | TVar { contents = Generic id } -> var_name t id ~weak:false
    | TVar { contents = Unbound (id, _) } -> var_name t id ~weak:true
    | TVar { contents = Link _ } -> assert false

  and context_to_string t context =
    let rec entries context =
      match canonical_context context with
      | CEmpty -> [], None
      | CExtend (name, presence, ty, rest) ->
        let entries, tail = entries rest in
        (name, presence, ty) :: entries, tail
      | CVar { contents = CGeneric id } -> [], Some (id, false)
      | CVar { contents = CUnbound (id, _) } -> [], Some (id, true)
      | CVar { contents = CLink _ } -> assert false
    in
    let entry (name, presence, ty) =
      let ty () = ty_to_string t ~prec:0 ty in
      match canonical_presence presence with
      | Present -> sprintf "%s : %s" name (ty ())
      | Absent -> "-" ^ name
      | PVar { contents = PGeneric id } ->
        let presence = var_name t id ~weak:false in
        sprintf "%s?%s : %s" name presence (ty ())
      | PVar { contents = PUnbound (id, _) } ->
        let presence = var_name t id ~weak:true in
        sprintf "%s?%s : %s" name presence (ty ())
      | PVar { contents = PLink _ } -> assert false
    in
    let fields, tail = entries context in
    let fields =
      List.stable_sort fields ~compare:(fun (a, _, _) (b, _, _) ->
        String.compare a b)
      |> List.map ~f:entry
    in
    let tail =
      Option.map tail ~f:(fun (id, weak) -> context_var_name t id ~weak)
      |> Option.to_list
    in
    "{" ^ String.concat ~sep:"; " (fields @ tail) ^ "}"
  ;;

  let ty t ty = ty_to_string t ~prec:0 ty

  let rec bounds t =
    match Queue.dequeue t.bounded with
    | None -> []
    | Some (name, id) ->
      let these =
        List.map (t.context_bounds id) ~f:(fun bound ->
          sprintf "%s <= %s" name (context_to_string t bound))
      in
      these @ bounds t
  ;;

  let where t =
    match bounds t with
    | [] -> ""
    | bounds -> " where " ^ String.concat ~sep:", " bounds
  ;;
end

let to_string_hum ?context_bounds ty =
  let printer = Printer.create ?context_bounds () in
  let ty = Printer.ty printer ty in
  ty ^ Printer.where printer
;;
