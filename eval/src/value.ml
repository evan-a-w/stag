open! Import

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Construct of Typed.Constructor_ref.t * t list
  | Record of t array
  | Poly_record of t String.Map.t
  | Poly_variant of string * t
  | Function of
      { args : Typed.Ident.t list
      ; body : Typed.Expr.t
      ; env : t Env.t
      ; level : int
      }
  | Code of
      { expr : Typed.Expr.t
      ; persistent : t Typed.Ident.Map.t
      }
  | Prim of
      { prim : Prim.t
      ; applied : t list
      }
  | Intrinsic of Intrinsic.t
  | Type of Syntax.Type_expr.t
  | Ref of Decl_ref.t

let of_prim : Prim.Value.t -> t = function
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s
  | Char c -> Char c
  | Unit -> Unit
;;

let to_prim : t -> Prim.Value.t option = function
  | Int n -> Some (Int n)
  | Bool b -> Some (Bool b)
  | String s -> Some (String s)
  | Char c -> Some (Char c)
  | Unit -> Some Unit
  | Tuple _
  | Construct _
  | Record _
  | Poly_record _
  | Poly_variant _
  | Function _
  | Code _
  | Prim _
  | Intrinsic _
  | Type _
  | Ref _ -> None
;;

let of_lit : Syntax.Lit.t -> t = function
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s
  | Char c -> Char c
  | Unit -> Unit
;;

let rec sexp_of_t : t -> Sexp.t = function
  | Int n -> Atom (Int.to_string n)
  | Bool b -> sexp_of_bool b
  | String s -> List [ Atom "String"; Atom s ]
  | Char c -> List [ Atom "Char"; sexp_of_char c ]
  | Unit -> Atom "()"
  | Tuple values -> List (Atom "Tuple" :: List.map values ~f:sexp_of_t)
  | Construct ({ name; _ }, values) ->
    List (Atom (Prelude_path.to_string_hum name) :: List.map values ~f:sexp_of_t)
  | Record values ->
    List (Atom "Record" :: Array.to_list (Array.map values ~f:sexp_of_t))
  | Poly_record fields -> [%sexp (fields : t String.Map.t)]
  | Poly_variant (tag, Unit) -> Atom ("`" ^ tag)
  | Poly_variant (tag, arg) -> List [ Atom ("`" ^ tag); sexp_of_t arg ]
  | Function _ | Prim _ | Intrinsic _ -> Atom "<fun>"
  | Type ty -> List [ Atom "Type"; Syntax.Type_expr.sexp_of_t ty ]
  | Ref ref_ -> List [ Atom "Ref"; Decl_ref.sexp_of_t ref_ ]
  | Code { expr; persistent } ->
    List
      (Atom "Code"
       :: Typed.Expr.sexp_of_t expr
       ::
       (if Map.is_empty persistent
        then []
        else [ [%sexp (persistent : t Typed.Ident.Map.t)] ]))
;;

let rec to_string_hum : t -> string = function
  | Int n -> Int.to_string n
  | Bool b -> Bool.to_string b
  | String s -> sprintf "%S" s
  | Char c -> sprintf "%C" c
  | Unit -> "()"
  | Tuple values -> tuple values
  | Construct ({ name; _ }, []) -> Prelude_path.to_string_hum name
  | Construct ({ name; _ }, [ (Tuple _ as value) ]) ->
    sprintf "%s (%s)" (Prelude_path.to_string_hum name) (to_string_hum value)
  | Construct ({ name; _ }, [ value ]) ->
    Prelude_path.to_string_hum name ^ " " ^ argument value
  | Construct ({ name; _ }, values) ->
    Prelude_path.to_string_hum name ^ " " ^ tuple values
  | Record _ -> "<record>"
  | Poly_record fields ->
    Map.to_alist fields
    |> List.map ~f:(fun (label, value) ->
      sprintf "`%s = %s" label (to_string_hum value))
    |> String.concat ~sep:"; "
    |> sprintf "{%s}"
  | Poly_variant (tag, Unit) -> "`" ^ tag
  | Poly_variant (tag, value) -> sprintf "`%s %s" tag (argument value)
  | Function _ | Prim _ | Intrinsic _ -> "<fun>"
  | Code _ -> "<code>"
  | Type _ -> "<type>"
  | Ref ref_ -> Decl_ref.to_string_hum ref_

and tuple values =
  sprintf "(%s)" (String.concat ~sep:", " (List.map values ~f:to_string_hum))

and argument (value : t) =
  match value with
  | Construct (_, []) | Poly_variant (_, Unit) -> to_string_hum value
  | Construct _ | Poly_variant _ -> "(" ^ to_string_hum value ^ ")"
  | Int n when n < 0 -> "(" ^ to_string_hum value ^ ")"
  | _ -> to_string_hum value
;;
