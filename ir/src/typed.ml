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
    | Region
    | Nontail
  [@@deriving sexp_of, equal]
end

module Allocation = struct
  module Id = struct
    include Int

    let of_int = Fn.id
  end

  module Kind = struct
    type t =
      | Closure
      | Partial_application
      | Code
      | Pointer
      | Box
      | Cell
    [@@deriving sexp_of, compare, equal]
  end

  module Placement = struct
    type t =
      | Heap
      | Region
      | Caller_region
    [@@deriving sexp_of, compare, equal]
  end

  module Side = struct
    module Step = struct
      type t =
        | Arg
        | Res
        | Component of int
        | Type_arg of int
        | Field of string
        | Code
      [@@deriving sexp_of, compare, equal]
    end

    type t =
      { steps : Step.t list
      ; side : [ `Arg | `Res ]
      }
    [@@deriving sexp_of, compare, equal]

    let to_string { steps; side } =
      let step : Step.t -> string = function
        | Arg -> "arg"
        | Res -> "res"
        | Component i -> sprintf "component %d" i
        | Type_arg i -> sprintf "type arg %d" i
        | Field name -> name
        | Code -> "code"
      in
      String.concat
        ~sep:"."
        (List.map steps ~f:step
         @ [ (match side with
              | `Arg -> "arg"
              | `Res -> "res")
           ])
    ;;
  end

  module Owner = struct
    module Let = struct
      type t =
        { id : Id.t
        ; name : string
        ; loc : Loc.t
        }
      [@@deriving sexp_of]
    end

    type t =
      { toplevel : Ident.t
      ; lets : Let.t list
      }
    [@@deriving sexp_of]

    let equal a b =
      Ident.equal a.toplevel b.toplevel
      && List.equal
           (fun (a : Let.t) (b : Let.t) -> Id.equal a.id b.id)
           a.lets
           b.lets
    ;;

    let to_string { toplevel; lets } =
      String.concat
        ~sep:"."
        (toplevel.name :: List.map lets ~f:(fun (l : Let.t) -> l.name))
    ;;
  end

  module Follows = struct
    type t =
      { owner : Owner.t
      ; side : Side.t
      ; if_local : Placement.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { id : Id.t
    ; owner : Owner.t
    ; loc : Loc.t
    ; kind : Kind.t
    ; placement : Placement.t
    ; follows : Follows.t option
    }
  [@@deriving sexp_of]
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
      | Seq of 'e * 'e
      | Let_mut of
          { ident : Ident.t
          ; annot : Syntax.Type_expr.t option [@sexp.option]
          ; mode : Syntax.Mode_expr.t option [@sexp.option]
          ; body : 'e
          ; in_ : 'e
          }
      | Assign of Var_ref.t * 'e
      | Fun of
          { args : Ident.t Nonempty_list.t
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
      | Record of
          { record : Record_type.t
          ; fields : (Field_ref.t * 'e) list
          }
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
      | External of
          { symbol : string
          ; signature : C_signature.t
          ; at_compile_time : Compile_time_c_calls.Policy.t
          }
      | Type of Syntax.Type_expr.t
      (** [type t], its types named by canonical names ([Resolved]). *)
      | Error of 'e list
      (** In place of what did not parse or check, with the parts of it that
          did. *)
    [@@deriving sexp_of]
  end

  module Made = struct
    type t =
      { made : Allocation.Id.t option
      ; partials : Allocation.Id.t list
      ; owns : Allocation.Id.t option
      }
    [@@deriving sexp_of]

    let none = { made = None; partials = []; owns = None }
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t
    ; made : Made.t
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)

  module Child = struct
    type t =
      { depth : int
      ; binds : Ident.t list
      }
    [@@deriving sexp_of]

    let same = { depth = 0; binds = [] }
    let binding binds = { same with binds }
  end

  module Traverse (M : Local_monad.S) = struct
    module Let_syntax = struct
      let bind = M.bind
    end

    let rec all list ~(f @ local) =
      match list with
      | [] -> M.return []
      | x :: rest ->
        let%bindl_fun y = f x in
        let%bindl_fun rest = all rest ~f in
        M.return (y :: rest)
    ;;

    let map_children t ~(f @ local) =
      let same e = f Child.same e in
      let snds child fields =
        all fields ~f:(fun (key, e) ->
          let%bindl_fun e = f child e in
          M.return (key, e))
        [@nontail]
      in
      let cases cases =
        all cases ~f:(fun (pattern, body) ->
          let%bindl_fun body =
            f (Child.binding (Pattern.idents pattern)) body
          in
          M.return (pattern, body))
        [@nontail]
      in
      let%bindl_fun (desc : t Desc.t) =
        match t.desc with
        | ( Lit _ | Var _
          | Poly_variant (_, None)
          | Expect _ | External _ | Type _ ) as desc -> M.return desc
        | App (a, b) ->
          let%bindl_fun a = same a in
          let%bindl_fun b = same b in
          M.return (Desc.App (a, b))
        | Let let_ ->
          let%bindl_fun body =
            f (Child.binding (let_.ident :: let_.args)) let_.body
          in
          let%bindl_fun in_ = f (Child.binding [ let_.ident ]) let_.in_ in
          M.return (Desc.Let { let_ with body; in_ })
        | Let_mut let_ ->
          let%bindl_fun body = same let_.body in
          let%bindl_fun in_ = f (Child.binding [ let_.ident ]) let_.in_ in
          M.return (Desc.Let_mut { let_ with body; in_ })
        | Seq (first, second) ->
          let%bindl_fun first = same first in
          let%bindl_fun second = same second in
          M.return (Desc.Seq (first, second))
        | Assign (var_ref, value) ->
          let%bindl_fun value = same value in
          M.return (Desc.Assign (var_ref, value))
        | Store (target, value) ->
          let%bindl_fun target = same target in
          let%bindl_fun value = same value in
          M.return (Desc.Store (target, value))
        | Fun fun_ ->
          let%bindl_fun body =
            f (Child.binding (Nonempty_list.to_list fun_.args)) fun_.body
          in
          M.return (Desc.Fun { fun_ with body })
        | Annotated (e, annot) ->
          let%bindl_fun e = same e in
          M.return (Desc.Annotated (e, annot))
        | Marked (marker, e) ->
          let%bindl_fun e = same e in
          M.return (Desc.Marked (marker, e))
        | Splice { code; subs } ->
          let%bindl_fun code = f { Child.same with depth = -1 } code in
          let%bindl_fun (subs : t Subs.t) =
            match subs with
            | Explicit subs ->
              let%bindl_fun subs = snds Child.same subs in
              M.return (Subs.Explicit subs)
            | Implicit _ -> M.return subs
          in
          M.return (Desc.Splice { code; subs })
        | Toplevel_splice { code; expansion } ->
          let%bindl_fun expansion = same expansion in
          M.return (Desc.Toplevel_splice { code; expansion })
        | Quote body ->
          let%bindl_fun body = f { Child.same with depth = 1 } body in
          M.return (Desc.Quote body)
        | Tuple es ->
          let%bindl_fun es = all es ~f:same in
          M.return (Desc.Tuple es)
        | Deref e ->
          let%bindl_fun e = same e in
          M.return (Desc.Deref e)
        | Construct (c, es) ->
          let%bindl_fun es = all es ~f:same in
          M.return (Desc.Construct (c, es))
        | Error es ->
          let%bindl_fun es = all es ~f:same in
          M.return (Desc.Error es)
        | Record { record; fields } ->
          let%bindl_fun fields = snds Child.same fields in
          M.return (Desc.Record { record; fields })
        | Field (e, field) ->
          let%bindl_fun e = same e in
          M.return (Desc.Field (e, field))
        | Record_update (e, fields) ->
          let%bindl_fun e = same e in
          let%bindl_fun fields = snds Child.same fields in
          M.return (Desc.Record_update (e, fields))
        | Match { scrutinee; cases = cases'; exception_cases } ->
          let%bindl_fun scrutinee = same scrutinee in
          let%bindl_fun cases' = cases cases' in
          let%bindl_fun exception_cases = cases exception_cases in
          M.return (Desc.Match { scrutinee; cases = cases'; exception_cases })
        | Poly_record fields ->
          let%bindl_fun fields = snds Child.same fields in
          M.return (Desc.Poly_record fields)
        | Poly_field (e, label) ->
          let%bindl_fun e = same e in
          M.return (Desc.Poly_field (e, label))
        | Poly_extend (fields, e) ->
          let%bindl_fun fields = snds Child.same fields in
          let%bindl_fun e = same e in
          M.return (Desc.Poly_extend (fields, e))
        | Poly_restrict (e, label) ->
          let%bindl_fun e = same e in
          M.return (Desc.Poly_restrict (e, label))
        | Poly_update (e, fields) ->
          let%bindl_fun e = same e in
          let%bindl_fun fields = snds Child.same fields in
          M.return (Desc.Poly_update (e, fields))
        | Poly_variant (tag, Some e) ->
          let%bindl_fun e = same e in
          M.return (Desc.Poly_variant (tag, Some e))
      in
      M.return { t with desc }
    ;;
  end

  module Traverse_identity = Traverse (struct
      type 'a t = 'a

      let return x = x
      let bind x ~(f @ local) = f x [@nontail]
    end)

  let map_children = Traverse_identity.map_children

  let iter_children t ~(f @ local) =
    ignore
      (map_children t ~f:(fun child e ->
         f child e;
         e)
       : t)
  ;;

  let fold_children t ~init ~(f @ local) =
    let acc = ref init in
    iter_children t ~f:(fun child e -> acc := f !acc child e);
    !acc
  ;;

  let sub_exprs t =
    List.rev (fold_children t ~init:[] ~f:(fun acc _ e -> e :: acc))
  ;;

  let rec has_error t =
    match t.desc with
    | Error _ -> true
    | Match { cases; exception_cases; scrutinee = _ }
      when List.exists (cases @ exception_cases) ~f:(fun (pattern, _) ->
             Pattern.has_error pattern) -> true
    | _ -> List.exists (sub_exprs t) ~f:has_error
  ;;

  module Ref = struct
    type t =
      | Binder of Binder.t
      | Free of string
  end

  let iter_own_refs t ~(f @ local) =
    let var : Var_ref.t -> unit = function
      | Bound binder | Persistent binder -> f (Ref.Binder binder)
      | Free name -> f (Free name)
      | Prim _ | Intrinsic _ -> ()
    in
    match t.desc with
    | Var var_ref | Assign (var_ref, _) -> var var_ref [@nontail]
    | Splice { subs = Implicit providers; _ } ->
      Map.iter providers ~f:(fun binder -> f (Binder binder)) [@nontail]
    | _ -> ()
  ;;

  let rec refers_to t ~(local_ f) =
    let found = ref false in
    iter_own_refs t ~f:(function
      | Binder binder -> if f binder then found := true
      | Free _ -> ());
    !found || (List.exists (sub_exprs t) ~f:(refers_to ~f) [@nontail])
  ;;
end

module Toplevel = struct
  module Kind = struct
    type t =
      | Value
      | Mut
      | Function of Ident.t Nonempty_list.t
    [@@deriving sexp_of]
  end

  type t =
    { ident : Ident.t
    ; kind : Kind.t
    ; scheme : Hum.Ty.t
    ; body : Expr.t
    ; partials : Allocation.Id.t list [@sexp.list]
    }
  [@@deriving sexp_of]

  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t t)
end

module Scc = struct
  type t = { members : Toplevel.t list } [@@deriving sexp_of]
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
  module Item = struct
    type t =
      | Toplevel of Ident.t
      | Module of string * t list
    [@@deriving sexp_of]

    let rec toplevels = function
      | Toplevel ident -> [ ident ]
      | Module (_, items) -> List.concat_map items ~f:toplevels
    ;;
  end

  type t =
    { sccs : Scc.t list
    ; items : Item.t list
    ; expect_tests : Expect_test.t list
    ; benches : Bench.t list
    ; declaration_splices : Expr.t list
    ; ident_info : Ident_info.t Ident.Map.t
    ; allocations : Allocation.t list
    }
  [@@deriving sexp_of]

  let exprs
    { sccs
    ; items = _
    ; expect_tests
    ; benches
    ; declaration_splices
    ; ident_info = _
    ; allocations = _
    }
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
