open! Import

(* While a value is converted, its types are left as placeholders, rendered
   in reading order once it is done, so variables are named as they are read. *)
module Hum = struct
  let pending : Type.ty Queue.t option ref = ref None

  (* Compared physically, so no other sexp is mistaken for a placeholder. *)
  let marker = String.make 1 '\000'

  let sexp_of_ty ty =
    match !pending with
    | Some pending ->
      let index = Queue.length pending in
      Queue.enqueue pending ty;
      Sexp.List [ Atom marker; Atom (Int.to_string index) ]
    | None -> Sexp.Atom (Type.to_string_hum ty)
  ;;

  let rec render printer types (sexp : Sexp.t) : Sexp.t =
    match sexp with
    | List [ Atom atom; Atom index ] when phys_equal atom marker ->
      Atom (Type.Printer.ty printer (Queue.get types (Int.of_string index)))
    | Atom _ -> sexp
    | List sexps -> List (List.map sexps ~f:(render printer types))
  ;;

  let scoped f =
    match !pending with
    | Some _ -> f ()
    | None ->
      let types = Queue.create () in
      pending := Some types;
      let sexp = Exn.protect ~f ~finally:(fun () -> pending := None) in
      let printer = Type.Printer.create () in
      let sexp = render printer types sexp in
      (match Type.Printer.where printer with
       | "" -> sexp
       | where ->
         let where =
           Sexp.List
             [ Atom "where"
             ; Atom (String.chop_prefix_exn where ~prefix:" where ")
             ]
         in
         (match sexp with
          | List items -> List (items @ [ where ])
          | Atom _ -> List [ sexp; where ]))
  ;;

  module Ty = struct
    type t = Type.ty

    let sexp_of_t = sexp_of_ty
  end

  let typed desc ty =
    let ty = [ Sexp.Atom ":"; sexp_of_ty ty ] in
    match (desc : Sexp.t) with
    | List (Atom _ :: _ as items) -> Sexp.List (items @ ty)
    | Atom _ | List _ -> List (desc :: ty)
  ;;
end

module Ident = struct
  module T = struct
    type t =
      { name : string
      ; stamp : int
      }
    [@@deriving compare, hash]

    let sexp_of_t t = Sexp.Atom [%string "%{t.name}/%{t.stamp#Int}"]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let fresh stamps name = { name; stamp = Stamp_counter.next stamps }
end

module Binder = struct
  type t =
    | Local of Ident.t
    | Toplevel of Ident.t
  [@@deriving sexp_of, equal]
end

module Var_ref = struct
  type t =
    | Bound of Binder.t
    | Persistent of Binder.t
    | Free of string
    | Prim of Prim.t
    | Intrinsic of Intrinsic.t
  [@@deriving sexp_of]
end

module Constructor_ref = struct
  type t =
    { type_name : string
    ; name : string
    ; index : int
    }
  [@@deriving sexp_of]
end

module Field_ref = struct
  type t =
    { type_name : string
    ; name : string
    ; index : int
    }
  [@@deriving sexp_of]
end

module Pattern = struct
  module Rest = struct
    type t =
      | Closed
      | Open
      | Bind of Ident.t
    [@@deriving sexp_of]
  end

  module Desc = struct
    type 'p t =
      | Any
      | Var of Ident.t
      | Lit of Syntax.Lit.t
      | Tuple of 'p list
      | Construct of Constructor_ref.t * 'p list
      | Record of (Field_ref.t * 'p) list
      | Poly_variant of string * 'p option
      | Poly_record of
          { fields : (string * 'p) list
          ; rest : Rest.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)

  let rec is_irrefutable t =
    match t.desc with
    | Any | Var _ -> true
    | Lit _ | Construct _ | Record _ | Poly_variant _ -> false
    | Tuple patterns -> List.for_all patterns ~f:is_irrefutable
    | Poly_record { fields; rest = _ } ->
      List.for_all fields ~f:(fun (_, pattern) -> is_irrefutable pattern)
  ;;
end

module Expr = struct
  module Subs = struct
    type 'e t =
      | Explicit of (string * 'e) list
      | Implicit of Binder.t String.Map.t
    [@@deriving sexp_of]
  end

  module Desc = struct
    type 'e t =
      | Lit of Syntax.Lit.t
      | Var of Var_ref.t
      | App of 'e * 'e
      | Let of
          { ident : Ident.t
          ; args : Ident.t list
          ; generalized : bool
          ; body : 'e
          ; in_ : 'e
          }
      | Splice of
          { code : 'e
          ; subs : 'e Subs.t
          }
      | Toplevel_splice of
          { code : 'e
          ; expansion : 'e
          }
      | Quote of 'e
      | Tuple of 'e list
      | Construct of Constructor_ref.t * 'e list
      | Record of (Field_ref.t * 'e) list
      | Field of 'e * Field_ref.t
      | Match of 'e * (Pattern.t * 'e) list
      | Poly_record of (string * 'e) list
      | Poly_field of 'e * string
      | Poly_extend of (string * 'e) list * 'e
      | Poly_restrict of 'e * string
      | Poly_update of 'e * (string * 'e) list
      | Poly_variant of string * 'e option
      | Expect of string Located.t
      | Type of Syntax.Type_expr.t
      (** [type t], its types named by canonical names ([Resolved]). *)
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)

  let sub_exprs t =
    let snds = List.map ~f:snd in
    match t.desc with
    | Lit _ | Var _ | Poly_variant (_, None) | Expect _ | Type _ -> []
    | App (a, b)
    | Let { body = a; in_ = b; _ }
    | Toplevel_splice { code = a; expansion = b } -> [ a; b ]
    | Splice { code; subs = Explicit subs } -> code :: snds subs
    | Splice { code; subs = Implicit _ } -> [ code ]
    | Quote e
    | Field (e, _)
    | Poly_field (e, _)
    | Poly_restrict (e, _)
    | Poly_variant (_, Some e) -> [ e ]
    | Tuple es | Construct (_, es) -> es
    | Record fields -> snds fields
    | Poly_record fields -> snds fields
    | Poly_extend (fields, e) | Poly_update (e, fields) -> e :: snds fields
    | Match (e, cases) -> e :: snds cases
  ;;

  let rec refers_to t ~f =
    match t.desc with
    | Var (Bound binder | Persistent binder) -> f binder
    | Splice { subs = Implicit providers; _ } when Map.exists providers ~f ->
      true
    | _ -> List.exists (sub_exprs t) ~f:(refers_to ~f)
  ;;

  let rec is_value t =
    match t.desc with
    | Lit _ | Var _ | Type _ -> true
    | Quote expr -> splices_are_values expr ~depth:1
    | Let { ident; args; body; in_; generalized = _ } ->
      binding_is_value ~ident ~args ~body && is_value in_
    | Toplevel_splice { expansion; code = _ } -> is_value expansion
    | App _ | Splice _ | Expect _ -> false
    | Tuple _
    | Construct _
    | Record _
    | Field _
    | Match _
    | Poly_record _
    | Poly_field _
    | Poly_extend _
    | Poly_restrict _
    | Poly_update _
    | Poly_variant _ -> List.for_all (sub_exprs t) ~f:is_value

  (* Whether the splices run when the enclosing quote [depth] levels out is
     built are values. *)
  and splices_are_values t ~depth =
    match t.desc with
    | Splice { code; subs } ->
      (if depth = 1
       then is_value code
       else splices_are_values code ~depth:(depth - 1))
      &&
        (match subs with
        | Explicit subs ->
          List.for_all subs ~f:(fun (_, expr) -> splices_are_values expr ~depth)
        | Implicit _ -> true)
    | Quote expr -> splices_are_values expr ~depth:(depth + 1)
    | Toplevel_splice { expansion; code = _ } ->
      splices_are_values expansion ~depth
    | Lit _
    | Var _
    | App _
    | Let _
    | Tuple _
    | Construct _
    | Record _
    | Field _
    | Match _
    | Poly_record _
    | Poly_field _
    | Poly_extend _
    | Poly_restrict _
    | Poly_update _
    | Poly_variant _
    | Expect _
    | Type _ -> List.for_all (sub_exprs t) ~f:(splices_are_values ~depth)

  and binding_is_value ~ident ~args ~body =
    (not (List.is_empty args))
    || (is_value body
        && not
             (refers_to body ~f:(function
                | Local ident' -> Ident.equal ident ident'
                | Toplevel _ -> false)))
  ;;
end

module Toplevel = struct
  type t =
    { ident : Ident.t
    ; args : Ident.t list
    ; scheme : Hum.Ty.t
    ; body : Expr.t
    }
  [@@deriving sexp_of]

  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t t)
end

module Scc = struct
  type t =
    { members : Toplevel.t list
    ; recursive : bool
    }
  [@@deriving sexp_of]
end

module Ident_info = struct
  type t =
    { ty : Hum.Ty.t
    ; level : int
    }
  [@@deriving sexp_of]
end

module Expect_test = struct
  type t =
    { name : string Located.t
    ; body : Expr.t
    ; loc : Loc.t
    ; generated : bool [@sexp.bool]
    }
  [@@deriving sexp_of]
end

module Program = struct
  type t =
    { sccs : Scc.t list
    ; expect_tests : Expect_test.t list
    ; ident_info : Ident_info.t Ident.Map.t
    }
  [@@deriving sexp_of]
end
