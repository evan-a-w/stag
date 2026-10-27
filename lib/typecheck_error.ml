open! Core

module Kind = struct
  type t =
    | Unification_error of Unification_error.t
    | Not_in_scope of Syntax.Path.t
    | Duplicate_toplevel of string
    | Duplicate_substitution of string
    | Variable_used_at_wrong_level of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
    | Unbound_module of Syntax.Path.t
    | Unbound_type of Syntax.Path.t
    | Unbound_type_variable of string
    | Type_variable_kind_mismatch of string
    | Unbound_constructor of Syntax.Path.t
    | Unbound_field of Syntax.Path.t
    | Type_arity_mismatch of
        { type_name : Syntax.Path.t
        ; expected : int
        ; got : int
        }
    | Type_arg_kind_mismatch of
        { type_name : Syntax.Path.t
        ; param : Syntax.Type_decl.Param.t
        }
    | Cyclic_type_alias of string
    | Non_regular_recursive_alias of string
    | Duplicate_module of string
    | Duplicate_type of string
    | Duplicate_type_parameter of
        { type_name : string
        ; param : string
        }
    | Duplicate_constructor of string
    | Duplicate_field of string
    | Constructor_arity_mismatch of
        { constructor : string
        ; expected : int
        ; got : int
        }
    | Missing_field of string
    | Fields_of_different_types of
        { field : string
        ; expected_type : string
        ; actual_type : string
        }
    | Empty_record
    | Duplicate_pattern_variable of string
    | Duplicate_row_entry of string
    | Compile_time_use_of_toplevel_being_checked of string
    | Expect_outside_expect_test
    | Splice_evaluation_failed of
        { code : Typed.Expr.t
        ; error : Error.t
        }
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t
  ; kind : Kind.t
  }

let sexp_of_t { loc; kind } = [%sexp_of: Kind.t Located.t] { txt = kind; loc }
let create ~loc kind = { loc; kind }
let unification_error ~loc error = create ~loc (Unification_error error)

let level_name level =
  match level with
  | 0 -> "runtime (level 0)"
  | level when level < 0 -> sprintf "compile time (level %d)" level
  | level -> sprintf "inside %d quote(s) (level %d)" level level
;;

let unification_message (error : Unification_error.t) =
  let printer = Type.Printer.create () in
  let ty = Type.Printer.ty printer in
  let row row = ty (TRecord { id = -1; row }) in
  let arg : Type.arg -> string = function
    | ATy t -> ty t
    | ARow r -> "row " ^ row r
  in
  let message =
    match error with
    | Type_mismatch (expected, actual, reason) ->
      let expected = ty expected in
      let actual = ty actual in
      let reason =
        match reason with
        | Base_types_not_equal _ | Structural_mismatch -> ""
        | Args_length_mismatch (a, b) ->
          sprintf "\nThey have different numbers of arguments: %d and %d." a b
        | Mismatch_arg_kind (a, b) ->
          let a = arg a in
          sprintf "\nThe arguments %s and %s are of different kinds." a (arg b)
      in
      sprintf
        "This expression has type %s but an expression was expected of type \
         %s%s"
        actual
        expected
        reason
    | Row_mismatch (expected, actual, reason) ->
      let expected = row expected in
      let actual = row actual in
      let reason =
        match reason with
        | Missing_label label -> sprintf "\nThe label %s is missing." label
        | Presence_mismatch label ->
          sprintf
            "\nThe label %s is present in one and absent in the other."
            label
        | Cyclic_row -> "\nThe row would be cyclic."
      in
      sprintf
        "The row %s is incompatible with the row %s%s"
        actual
        expected
        reason
    | Occurs_check_failed var ->
      sprintf "The type variable %s occurs inside its own definition" (arg var)
  in
  message ^ Type.Printer.where printer
;;

let message { kind; loc = _ } =
  match kind with
  | Unification_error error -> unification_message error
  | Not_in_scope path -> sprintf "Unbound value %s" (Syntax.Path.to_string path)
  | Duplicate_toplevel name ->
    sprintf "The value %s is defined more than once in this module" name
  | Duplicate_substitution name ->
    sprintf "The variable %s is substituted more than once" name
  | Variable_used_at_wrong_level { name; var_level; used_at } ->
    sprintf
      "The variable %s is bound at %s but used at %s; only toplevel functions \
       may be used at other levels"
      name
      (level_name var_level)
      (level_name used_at)
  | Unbound_module path ->
    sprintf "Unbound module %s" (Syntax.Path.to_string path)
  | Unbound_type path -> sprintf "Unbound type %s" (Syntax.Path.to_string path)
  | Unbound_type_variable name -> sprintf "Unbound type variable '%s" name
  | Type_variable_kind_mismatch name ->
    sprintf "The variable %s is used both as a type and as a row" name
  | Unbound_constructor path ->
    sprintf "Unbound constructor %s" (Syntax.Path.to_string path)
  | Unbound_field path ->
    sprintf "Unbound record field %s" (Syntax.Path.to_string path)
  | Type_arity_mismatch { type_name; expected; got } ->
    sprintf
      "The type %s expects %d argument(s), but is given %d"
      (Syntax.Path.to_string type_name)
      expected
      got
  | Type_arg_kind_mismatch { type_name; param } ->
    let expected =
      match param with
      | Ty _ -> "a type"
      | Row _ -> "a row"
    in
    sprintf
      "The parameter %s of type %s expects %s"
      (Syntax.Type_decl.Param.name param)
      (Syntax.Path.to_string type_name)
      expected
  | Cyclic_type_alias name -> sprintf "The type abbreviation %s is cyclic" name
  | Non_regular_recursive_alias name ->
    sprintf
      "The recursive type abbreviation %s is used with different arguments in \
       its definition"
      name
  | Duplicate_module name ->
    sprintf "The module %s is defined more than once" name
  | Duplicate_type name ->
    sprintf "The type %s is defined more than once in this module" name
  | Duplicate_type_parameter { type_name; param } ->
    sprintf "The parameter %s occurs more than once in type %s" param type_name
  | Duplicate_constructor name ->
    sprintf "The constructor %s is defined more than once in this module" name
  | Duplicate_field name ->
    sprintf "The record field %s is defined more than once in this module" name
  | Constructor_arity_mismatch { constructor; expected; got } ->
    sprintf
      "The constructor %s expects %d argument(s), but is applied to %d"
      constructor
      expected
      got
  | Missing_field name -> sprintf "Some record fields are undefined: %s" name
  | Fields_of_different_types { field; expected_type; actual_type } ->
    sprintf
      "The record field %s belongs to the type %s but is mixed here with \
       fields of type %s"
      field
      actual_type
      expected_type
  | Empty_record -> "A record must have at least one field"
  | Duplicate_pattern_variable name ->
    sprintf "The variable %s is bound several times in this matching" name
  | Duplicate_row_entry name ->
    sprintf "The label %s is given more than once" name
  | Compile_time_use_of_toplevel_being_checked name ->
    sprintf
      "The toplevel %s is used at compile time before it is fully checked (it \
       is in the same recursive group as this splice)"
      name
  | Expect_outside_expect_test ->
    "[%expect] is only allowed in the body of a let%expect_test, outside \
     quotes and toplevel splices"
  | Splice_evaluation_failed { code = _; error } ->
    let error =
      match Runtime_error.of_error error with
      | Some { loc; message } when not (Loc.is_none loc) ->
        sprintf "%s, at %s" message (Loc.to_string loc)
      | Some { message; loc = _ } -> message
      | None -> Error.to_string_hum error
    in
    sprintf "This toplevel splice failed at compile time: %s" error
;;
