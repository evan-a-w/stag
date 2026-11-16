open! Import

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Construct of Typed.Constructor_ref.t * t list
  | Record of Typed.Record_type.t * t array
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
  | String_list _ -> raise_s [%message "Value.of_prim: prims return no lists"]
;;

(* A list is [Nil] or [Cons] of an element and a list: constructors without and
   with arguments. *)
let string_list value =
  let rec loop acc : t -> _ = function
    | Construct (_, []) -> This (Prim.Value.String_list (List.rev acc))
    | Construct (_, [ String s; rest ]) -> loop (s :: acc) rest
    | _ -> Null
  in
  loop [] value
;;

let to_prim : t -> Prim.Value.t or_null = function
  | Int n -> This (Int n)
  | Bool b -> This (Bool b)
  | String s -> This (String s)
  | Char c -> This (Char c)
  | Unit -> This Unit
  | Construct _ as value -> string_list value
  | Tuple _
  | Record _
  | Poly_record _
  | Poly_variant _
  | Function _
  | Code _
  | Prim _
  | Intrinsic _
  | Type _
  | Ref _ -> Null
;;

let of_lit : Syntax.Lit.t -> t = function
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s
  | Char c -> Char c
  | Unit -> Unit
;;

(* Unqualified, as fields are looked up in the record's type. *)
let field_name (record_type : Typed.Record_type.t) index =
  Prelude_path.short_name record_type.fields.(index)
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
  | Record (record_type, values) ->
    List
      (Atom (Prelude_path.to_string_hum record_type.name)
       :: Array.to_list
            (Array.mapi values ~f:(fun index value ->
               Sexp.List
                 [ Atom (field_name record_type index); sexp_of_t value ])))
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

exception Too_long

let to_string_hum ?max_length value =
  let buffer = Buffer.create 64 in
  let add string =
    Buffer.add_string buffer string;
    match max_length with
    | Some max_length when Buffer.length buffer > max_length -> raise Too_long
    | Some _ | None -> ()
  in
  let rec value_ : t -> unit = function
    | Int n -> add (Int.to_string n)
    | Bool b -> add (Bool.to_string b)
    | String s -> add (sprintf "%S" s)
    | Char c -> add (sprintf "%C" c)
    | Unit -> add "()"
    | Tuple values -> tuple values
    | Construct ({ name; _ }, []) -> add (Prelude_path.to_string_hum name)
    | Construct ({ name; _ }, [ (Tuple _ as value) ]) ->
      add (Prelude_path.to_string_hum name);
      add " (";
      value_ value;
      add ")"
    | Construct ({ name; _ }, [ value ]) ->
      add (Prelude_path.to_string_hum name);
      add " ";
      argument value
    | Construct ({ name; _ }, values) ->
      add (Prelude_path.to_string_hum name);
      add " ";
      tuple values
    | Record (record_type, values) ->
      add "{";
      Array.iteri values ~f:(fun index value ->
        if index > 0 then add "; ";
        add (sprintf "%s = " (field_name record_type index));
        value_ value);
      add "}"
    | Poly_record fields ->
      add "{";
      List.iteri (Map.to_alist fields) ~f:(fun i (label, value) ->
        if i > 0 then add "; ";
        add (sprintf "`%s = " label);
        value_ value);
      add "}"
    | Poly_variant (tag, Unit) -> add ("`" ^ tag)
    | Poly_variant (tag, value) ->
      add (sprintf "`%s " tag);
      argument value
    | Function _ | Prim _ | Intrinsic _ -> add "<fun>"
    | Code _ -> add "<code>"
    | Type _ -> add "<type>"
    | Ref ref_ -> add (Decl_ref.to_string_hum ref_)
  and tuple values =
    add "(";
    List.iteri values ~f:(fun i value ->
      if i > 0 then add ", ";
      value_ value);
    add ")"
  and argument (value : t) =
    match value with
    | Construct (_, []) | Poly_variant (_, Unit) -> value_ value
    | Construct _ | Poly_variant _ ->
      add "(";
      value_ value;
      add ")"
    | Int n when n < 0 ->
      add "(";
      value_ value;
      add ")"
    | _ -> value_ value
  in
  match value_ value with
  | () -> Buffer.contents buffer
  | exception Too_long ->
    Buffer.To_string.sub buffer ~pos:0 ~len:(Option.value_exn max_length)
    ^ "..."
;;

let to_string_for_message = to_string_hum ~max_length:1000
