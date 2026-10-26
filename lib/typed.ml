open! Core

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
end

module Binder = struct
  type t =
    | Local of Ident.t
    | Toplevel of Ident.t
  [@@deriving sexp_of]
end

module Var_ref = struct
  type t =
    | Bound of Binder.t
    | Persistent of Binder.t
    | Free of string
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
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)
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
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    }

  let rec sexp_of_t' t = Hum.typed (Desc.sexp_of_t sexp_of_t' t.desc) t.ty
  let sexp_of_t t = Hum.scoped (fun () -> sexp_of_t' t)
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

module Program = struct
  type t =
    { sccs : Scc.t list
    ; ident_info : Ident_info.t Ident.Map.t
    }
  [@@deriving sexp_of]
end
