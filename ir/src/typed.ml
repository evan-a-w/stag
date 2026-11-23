open! Import

(* While a value is converted, its types are left as placeholders, rendered in
   reading order once it is done, so variables are named as they are read. *)
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
  type t =
    { name : string
    ; stamp : int
    }
  [@@deriving compare]

  (* Stamps are rarely shared, and hashing names is costly. *)
  let hash_fold_t state t = Int.hash_fold_t state t.stamp
  let hash t = Int.hash t.stamp
  let sexp_of_t t = Sexp.Atom [%string "%{t.name}/%{t.stamp#Int}"]

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain

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
    | Free_toplevel of string
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

  let exception_index = -1

  let exception_ name =
    { type_name = Type.exn_name; name; index = exception_index }
  ;;

  let is_exception t = t.index = exception_index
end

module Record_type = struct
  type t =
    { name : string
    ; fields : string array
    }

  let sexp_of_t t = Sexp.Atom t.name
end

module Field_ref = struct
  type t =
    { record : Record_type.t
    ; index : int
    }

  let name t = t.record.fields.(t.index)

  let sexp_of_t t =
    [%sexp
      { type_name : string = t.record.name
      ; name : string = name t
      ; index : int = t.index
      }]
  ;;
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
      | Deref of 'p
      | Construct of Constructor_ref.t * 'p list
      | Record of (Field_ref.t * 'p) list
      | Poly_variant of string * 'p option
      | Poly_record of
          { fields : (string * 'p) list
          ; rest : Rest.t
          }
      | Error of 'p list
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)

  let rec idents t =
    match t.desc with
    | Any | Lit _ | Poly_variant (_, None) -> []
    | Var ident -> [ ident ]
    | Tuple ps | Construct (_, ps) | Error ps -> List.concat_map ps ~f:idents
    | Record fields -> List.concat_map fields ~f:(fun (_, p) -> idents p)
    | Poly_record { fields; rest } ->
      (match rest with
       | Closed | Open -> []
       | Bind ident -> [ ident ])
      @ List.concat_map fields ~f:(fun (_, p) -> idents p)
    | Deref p | Poly_variant (_, Some p) -> idents p
  ;;

  let error patterns ~loc = { desc = Error patterns; ty = Type.error; loc }

  let rec has_error t =
    match t.desc with
    | Error _ -> true
    | Any | Var _ | Lit _ | Poly_variant (_, None) -> false
    | Tuple ps | Construct (_, ps) -> List.exists ps ~f:has_error
    | Deref p | Poly_variant (_, Some p) -> has_error p
    | Record fields -> List.exists fields ~f:(fun (_, p) -> has_error p)
    | Poly_record { fields; rest = _ } ->
      List.exists fields ~f:(fun (_, p) -> has_error p)
  ;;

  let rec is_irrefutable t =
    match t.desc with
    | Any | Var _ -> true
    | Lit _ | Construct _ | Record _ | Poly_variant _ | Error _ -> false
    | Tuple patterns -> List.for_all patterns ~f:is_irrefutable
    | Deref pattern -> is_irrefutable pattern
    | Poly_record { fields; rest = _ } ->
      List.for_all fields ~f:(fun (_, pattern) -> is_irrefutable pattern)
  ;;
end

module Marker = struct
  type t =
    | Stack
    | Exclave
    | Nontail
  [@@deriving sexp_of, equal]
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
          ; annots : (Ident.t * Syntax.Annot.t) list [@sexp.list]
          ; result_annot : Syntax.Type_expr.t option [@sexp.option]
          ; mode : Syntax.Mode_expr.t option [@sexp.option]
          ; generalized : bool
          ; body : 'e
          ; in_ : 'e
          }
      | Let_mut of
          { ident : Ident.t
          ; annot : Syntax.Type_expr.t option [@sexp.option]
          ; mode : Syntax.Mode_expr.t option [@sexp.option]
          ; body : 'e
          ; in_ : 'e
          }
      | Assign of Var_ref.t * 'e
      | Fun of
          { args : Ident.t list
          ; annots : (Ident.t * Syntax.Annot.t) list [@sexp.list]
          ; body : 'e
          }
      | Annotated of 'e * Syntax.Annot.t
      | Marked of Marker.t * 'e
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
      | Deref of 'e
      | Construct of Constructor_ref.t * 'e list
      | Record of (Field_ref.t * 'e) list
      | Field of 'e * Field_ref.t
      | Record_update of 'e * (Field_ref.t * 'e) list
      | Store of 'e * 'e
      | Match of
          { scrutinee : 'e
          ; cases : (Pattern.t * 'e) list
          ; exception_cases : (Pattern.t * 'e) list [@sexp.list]
          }
      | Poly_record of (string * 'e) list
      | Poly_field of 'e * string
      | Poly_extend of (string * 'e) list * 'e
      | Poly_restrict of 'e * string
      | Poly_update of 'e * (string * 'e) list
      | Poly_variant of string * 'e option
      | Expect of string Located.t
      | Type of Syntax.Type_expr.t
      (** [type t], its types named by canonical names ([Resolved]). *)
      | Error of 'e list
      (** In place of what did not parse or check, with the parts of it that
          did. *)
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
    | Let_mut { body = a; in_ = b; _ }
    | Toplevel_splice { code = a; expansion = b } -> [ a; b ]
    | Splice { code; subs = Explicit subs } -> code :: snds subs
    | Splice { code; subs = Implicit _ } -> [ code ]
    | Store (target, value) -> [ target; value ]
    | Quote e
    | Annotated (e, _)
    | Marked (_, e)
    | Assign (_, e)
    | Fun { body = e; _ }
    | Deref e
    | Field (e, _)
    | Poly_field (e, _)
    | Poly_restrict (e, _)
    | Poly_variant (_, Some e) -> [ e ]
    | Tuple es | Construct (_, es) | Error es -> es
    | Record fields -> snds fields
    | Poly_record fields -> snds fields
    | Poly_extend (fields, e) | Poly_update (e, fields) -> e :: snds fields
    | Record_update (e, fields) -> e :: snds fields
    | Match { scrutinee; cases; exception_cases } ->
      (scrutinee :: snds cases) @ snds exception_cases
  ;;

  let rec has_error t =
    match t.desc with
    | Error _ -> true
    | Match { cases; exception_cases; scrutinee = _ }
      when List.exists (cases @ exception_cases) ~f:(fun (pattern, _) ->
             Pattern.has_error pattern) -> true
    | _ -> List.exists (sub_exprs t) ~f:has_error
  ;;

  let rec refers_to t ~(local_ f) =
    match t.desc with
    | Var (Bound binder | Persistent binder) -> f binder
    | Assign ((Bound binder | Persistent binder), _) when f binder -> true
    | Splice { subs = Implicit providers; _ } when Map.exists providers ~f ->
      true
    | _ -> List.exists (sub_exprs t) ~f:(refers_to ~f) [@nontail]
  ;;
end

module Toplevel = struct
  type t =
    { ident : Ident.t
    ; mut : bool [@sexp.bool]
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

module Bench = struct
  type t =
    { name : string Located.t
    ; param : (Ident.t * Expr.t list) option
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
    ; benches : Bench.t list
    ; declaration_splices : Expr.t list
    ; ident_info : Ident_info.t Ident.Map.t
    }
  [@@deriving sexp_of]

  let exprs { sccs; expect_tests; benches; declaration_splices; ident_info = _ }
    =
    List.concat_map sccs ~f:(fun (scc : Scc.t) ->
      List.map scc.members ~f:(fun (member : Toplevel.t) -> member.body))
    @ List.map expect_tests ~f:(fun (test : Expect_test.t) -> test.body)
    @ List.concat_map benches ~f:(fun (bench : Bench.t) ->
      bench.body
      :: Option.value_map bench.param ~default:[] ~f:(fun (_, args) -> args))
    @ declaration_splices
  ;;

  let has_error t = List.exists (exprs t) ~f:Expr.has_error
end
