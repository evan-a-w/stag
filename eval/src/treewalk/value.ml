open! Import

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Ptr of t ref
  | Box of t
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
  | External of
      { symbol : string
      ; signature : C_signature.t
      ; at_compile_time : Compile_time_c_calls.Policy.t
      ; applied : t list
      }
  | Intrinsic of Intrinsic.t
  | Type of Syntax.Type_expr.t
  | Ref of Decl_ref.t

let of_prim (value : Prim.Value.t) ~con : t =
  match value with
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s
  | Char c -> Char c
  | Unit -> Unit
  | String_list strings ->
    List.fold_right strings ~init:(con "Nil" []) ~f:(fun s rest ->
      con "Cons" [ Box (Tuple [ String s; rest ]) ])
  | String_option None -> con "None" []
  | String_option (Some s) -> con "Some" [ String s ]
;;

(* A list is [Nil] or [Cons] of an element and a list: constructors without and
   with arguments. *)
let string_list value =
  let rec loop acc : t -> _ = function
    | Construct (_, []) -> This (Prim.Value.String_list (List.rev acc))
    | Construct (_, [ Box (Tuple [ String s; rest ]) ]) -> loop (s :: acc) rest
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
  | Ptr _
  | Box _
  | Record _
  | Poly_record _
  | Poly_variant _
  | Function _
  | Code _
  | Prim _
  | External _
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

let print_budget = 100_000
let max_print_depth = 1024

let sexp_of_t value =
  let remaining = ref print_budget in
  let rec sexp depth pointers value : Sexp.t =
    if !remaining = 0 || depth >= max_print_depth
    then Atom "<truncated>"
    else (
      decr remaining;
      let sexp_of_t = sexp (depth + 1) pointers in
      match value with
      | Int n -> Atom (Int.to_string n)
      | Bool b -> sexp_of_bool b
      | String s -> List [ Atom "String"; Atom s ]
      | Char c -> List [ Atom "Char"; sexp_of_char c ]
      | Unit -> Atom "()"
      | Tuple values -> List (Atom "Tuple" :: List.map values ~f:sexp_of_t)
      | Ptr cell ->
        if List.exists pointers ~f:(phys_equal cell)
        then Atom "<cycle>"
        else List [ Atom "Ptr"; sexp (depth + 1) (cell :: pointers) !cell ]
      | Box value -> List [ Atom "Box"; sexp_of_t value ]
      | Construct ({ name; _ }, [ Box (Tuple [ _; _ ] as cell) ])
        when String.equal name (Prelude_path.canonical "Cons") ->
        (match sexp_of_t cell with
         | List (Atom "Tuple" :: values) -> List (Atom "Cons" :: values)
         | payload -> List [ Atom "Cons"; List [ Atom "Box"; payload ] ])
      | Construct ({ name; _ }, values) ->
        List
          (Atom (Prelude_path.to_string_hum name)
           :: List.map values ~f:sexp_of_t)
      | Record (record_type, values) ->
        List
          (Atom (Prelude_path.to_string_hum record_type.name)
           :: Array.to_list
                (Array.mapi values ~f:(fun index value ->
                   Sexp.List
                     [ Atom (field_name record_type index); sexp_of_t value ]))
          )
      | Poly_record fields ->
        [%sexp (Map.map fields ~f:sexp_of_t : Sexp.t String.Map.t)]
      | Poly_variant (tag, Unit) -> Atom ("`" ^ tag)
      | Poly_variant (tag, arg) -> List [ Atom ("`" ^ tag); sexp_of_t arg ]
      | Function _ | Prim _ | External _ | Intrinsic _ -> Atom "<fun>"
      | Type ty -> List [ Atom "Type"; Syntax.Type_expr.sexp_of_t ty ]
      | Ref ref_ -> List [ Atom "Ref"; Decl_ref.sexp_of_t ref_ ]
      | Code { expr; persistent } ->
        List
          (Atom "Code"
           :: Typed.Expr.sexp_of_t expr
           ::
           (if Map.is_empty persistent
            then []
            else
              [ [%sexp
                  (Map.map persistent ~f:sexp_of_t : Sexp.t Typed.Ident.Map.t)]
              ])))
  in
  sexp 0 [] value
;;

exception Too_long
exception Print_limit

let to_string_hum ?max_length value =
  let buffer = Buffer.create 64 in
  let depth = ref 0 in
  let remaining = ref print_budget in
  let pointers = ref [] in
  let add string =
    Buffer.add_string buffer string;
    match max_length with
    | Some max_length when Buffer.length buffer > max_length -> raise Too_long
    | Some _ | None -> ()
  in
  let rec value_ value =
    if !remaining = 0 then raise Print_limit;
    if !depth >= max_print_depth
    then add "<truncated>"
    else (
      incr depth;
      decr remaining;
      print_value value;
      decr depth)
  and print_value : t -> unit = function
    | Int n -> add (Int.to_string n)
    | Bool b -> add (Bool.to_string b)
    | String s -> add (sprintf "%S" s)
    | Char c -> add (sprintf "%C" c)
    | Unit -> add "()"
    | Tuple values -> tuple values
    | Ptr cell ->
      if List.exists !pointers ~f:(phys_equal cell)
      then add "<cycle>"
      else (
        let previous = !pointers in
        pointers := cell :: previous;
        add "Ptr.make ";
        argument !cell;
        pointers := previous)
    | Box value ->
      add "Box.make ";
      argument value
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
        (* The first qualified, as a constructor is: the rest are found from it. *)
        let name =
          if index = 0
          then Prelude_path.to_string_hum record_type.fields.(index)
          else field_name record_type index
        in
        add (sprintf "%s = " name);
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
    | Function _ | Prim _ | External _ | Intrinsic _ -> add "<fun>"
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
    | Ptr cell when List.exists !pointers ~f:(phys_equal cell) -> value_ value
    | Ptr _ | Box _ | Construct _ | Poly_variant _ ->
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
  | exception Print_limit ->
    (match add "<truncated>" with
     | () -> Buffer.contents buffer
     | exception Too_long ->
       Buffer.To_string.sub buffer ~pos:0 ~len:(Option.value_exn max_length)
       ^ "...")
  | exception Too_long ->
    Buffer.To_string.sub buffer ~pos:0 ~len:(Option.value_exn max_length)
    ^ "..."
;;

let to_string_for_message = to_string_hum ~max_length:1000
