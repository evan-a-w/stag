open! Core

module Env = struct
  type 'value t =
    { prev : 'value t option
    ; values : 'value Typed.Ident.Table.t
    }
  [@@deriving sexp_of, fields]

  let rec lookup t ~ident =
    match Hashtbl.find t.values ident with
    | None -> Option.bind t.prev ~f:(lookup ~ident)
    | Some _ as res -> res
  ;;
end

module Value = struct
  type t =
    | Int of string
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | Record of t String.Map.t
    | Variant of string * t
    | Function of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; env : t Env.t
        }
  [@@deriving sexp_of, variants]
end

type t =
  { eval_stack : Typed.Scc.t Stack.t
  ; idents : Type.ty Typed.Ident.Map.t
  ; values : Value.t Typed.Ident.Table.t
  ; closure_captures : Typed.Ident.Hash_set.t Typed.Ident.Table.t
  ; mutable env : Value.t Env.t
  }

let captures t ~ident ~args ~body =
  Hashtbl.find_or_add t.closure_captures ~default:(fun () ->
    let defined = Typed.Ident.Hash_set.create () in
    let captures = Typed.Ident.Hash_set.create () in
    let def ident = Hash_set.add defined ident in
    let use ident =
      if Hash_set.mem defined ident then () else Hash_set.add captures ident
    in
    List.iter args ~f:(Hash_set.add defined);
    let rec go ({ desc; _ } : Typed.Expr.t) =
      match desc with
      | Lit _ | Var (Free _) -> ()
      | Var
          ( Bound (Toplevel ident | Local ident)
          | Persistent (Toplevel ident | Local ident) ) -> use ident
      | App (a, b) ->
        go a;
        go b
      | Let { ident; args = _; generalized = _; body; in_ } ->
        (* in scope immediately *)
        def ident;
        go body;
        go in_
     
      | Splice
          { code
          ; subs
          } ->  
            match subs with
            | Explicit l -> List.iter l ~f:(fun )

      | Explicit of (string * 'e) list
      (** Each entry, by name, is the code it is bound to, at the splice's
          level. A substitution without an expression is a [Var]. *)
      | Implicit of Binder.t String.Map.t
      (** The binders at the splice that may provide the code's entries of
          their name. The other entries of the code become entries of the
         
            (*
      | Quote of 'e
      | Tuple l -> List.iter l ~f:go
      | Construct of Constructor_ref.t * 'e list
      | Record of (Field_ref.t * 'e) list
      | Field of 'e * Field_ref.t
      | Match of 'e * (Pattern.t * 'e) list
      | Poly_record of (string * 'e) list
      | Poly_field of 'e * string
      | Poly_extend of (string * 'e) list * 'e
      | Poly_restrict of 'e * string
      | Poly_update of 'e * (string * 'e) list
      | Poly_variant of string * 'e option *)
    in
    go body;
    captures)
;;

let create ~program:({ sccs; idents } : Typed.Program.t) =
  { eval_stack = Stack.of_list sccs
  ; idents
  ; values = Typed.Ident.Table.create ()
  ; closure_captures = Typed.Ident.Table.create ()
  }
;;

let eval t = ()
