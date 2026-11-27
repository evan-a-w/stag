open! Import

module Unusable_in_quote = struct
  type t =
    { loc : Loc.t
    ; var_level : Env.Level.t
    ; used_at : Env.Level.t
    }
  [@@deriving sexp_of]
end

module Not_embeddable_why = struct
  type t =
    | Defining
    | Unknown
    | Nonportable
  [@@deriving sexp_of]
end

module Splice_kind = struct
  type t =
    | Include
    | Type_definition
  [@@deriving sexp_of]
end

module Kind = struct
  type t =
    | Unification_error of Unification_error.t
    | Not_in_scope of Syntax.Decl_path.t
    | Duplicate_toplevel of string
    | Duplicate_substitution of string
    | Unused_substitution of string
    | Variable_used_at_wrong_level of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
    | Free_in_spliced_code of
        { name : string
        ; unusable : Unusable_in_quote.t
        }
    | Not_embeddable of
        { name : string
        ; why : Not_embeddable_why.t
        }
    | Forgotten of string
    | Duplicate_forget of string
    | Unbound_module of Syntax.Decl_path.t
    | Unbound_type of Syntax.Decl_path.t
    | Unbound_type_variable of string
    | Type_variable_kind_mismatch of string
    | Unbound_constructor of Syntax.Decl_path.t
    | Unbound_field of Syntax.Decl_path.t
    | No_such_constructor of
        { type_name : string
        ; constructor : string
        }
    | No_such_field of
        { type_name : string
        ; field : string
        }
    | Type_arity_mismatch of
        { type_name : Syntax.Decl_path.t
        ; expected : int
        ; got : int
        }
    | Type_arg_kind_mismatch of
        { type_name : Syntax.Decl_path.t
        ; param : Syntax.Type_decl.Param.t
        }
    | Cyclic_type_alias of string
    | Infinite_type_layout of string
    | Non_regular_recursive_alias of string
    | Duplicate_module of string
    | Duplicate_type of string
    | Duplicate_type_parameter of
        { type_name : string
        ; param : string
        }
    | Duplicate_constructor of string
    | Duplicate_field of string
    | Repeated_field of string
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
    | Misplaced_exception_pattern
    | Match_without_value_cases
    | Non_exhaustive_match of string
    | Unused_arm
    | Unused_exception_arm
    | Duplicate_pattern_variable of string
    | Or_pattern_variable_missing of string
    | Or_pattern_variable_type of
        { name : string
        ; error : Unification_error.t
        }
    | Duplicate_parameter of string
    | Duplicate_row_entry of string
    | Assign_to_immutable of string
    | Store_not_through_pointer
    (** A [Store] whose target is not a dereference, or a field of one. *)
    | Store_into_box (** A [Store] through a [box], which is immutable. *)
    | Exclave_not_in_tail_position
    (** [exclave_ e] other than as the result of a function body. *)
    | Stack_not_on_allocation (** [stack_ e] where [e] allocates nothing. *)
    | Compile_time_use_of_mutable of
        { toplevel : string
        ; mutable_ : string
        }
    | Mode_error of Mode.Error.t
    | Mutable_used_outside_its_code of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
    | Toplevel_initialization_cycle of string list
    | Used_before_defined of
        { name : string
        ; shadows : bool
        }
    | Compile_time_use_of_toplevel_being_checked of string
    | Expect_outside_expect_test
    | Splice_evaluation_failed of
        { code : Typed.Expr.t
        ; error : Error.t
        }
    | Malformed_syntax_splice of Error.t
    | Syntax_splice_with_substitutions
    | Expect_in_syntax_splice
    | Toplevel_splice_of_neither of Type.ty
    | In_syntax_splice of
        { generated : Syntax.Expr.t [@sexp.opaque]
        ; error : t
        }
    | Declaration_splice_failed of Error.t
    | Malformed_declarations of Error.t
    | Malformed_struct_quote of string
    | Misplaced_type_splice
    | Misplaced_mut
    | Misplaced_modality
    | Locality_in_mode_argument
    | Relative_locality_modality
    | Mode_variable_in_declaration of string
    | Anonymous_mode_in_constraint
    | Invalid_mode of string
    | Invalid_external of
        { symbol : string
        ; problem : C_signature.Problem.t
        }
    | Invalid_external_symbol of string
    | Spliced_code_assigns_immutable of string
    | Type_splice_failed of Error.t
    | Malformed_type_splice of Error.t
    | In_type_splice of
        { generated : Syntax.Type_expr.t [@sexp.opaque]
        ; error : t
        }
    | Via_quote of
        { toplevel : string
        ; quote : Loc.t
        ; error : t
        }
    | Type_splice_cycle
    | Nested_type_splice
    | Type_definition_cycle of
        { name : string
        ; splice : Loc.t
        }
    | Declaration_splice_cycle of
        { name : string
        ; module_ : string option
        ; splice : Loc.t
        ; splice_kind : Splice_kind.t
        }
    | Generated_after_include of
        { error : t
        ; include_ : Loc.t
        ; splice_kind : Splice_kind.t
        }
    | Maybe_generated_by_running_include of
        { error : t
        ; include_ : Loc.t
        ; splice_kind : Splice_kind.t
        ; after : bool
        }
    | Shadowed_prelude_module of
        { error : t
        ; module_ : string
        ; shadowed_by : Loc.t
        }
    | In_generated_declaration of
        { declaration : string
        ; generated : Syntax.Toplevel.t [@sexp.opaque]
        ; error : t
        }
    | Already_reported
    | Internal_error of Error.t
  [@@deriving sexp_of]
end

module In_scope = struct
  type t =
    { types : String.Set.t
    ; modules : String.Set.t
    ; root_modules : String.Set.t
    }

  let shadows t relative =
    Prelude_path.shadowed relative ~has:(fun kind name ->
      Set.mem
        (match kind with
         | `Type -> t.types
         | `Module -> t.modules)
        name)
  ;;
end

type t =
  { loc : Loc.t
  ; kind : Kind.t
  ; in_scope : In_scope.t option
  }

let sexp_of_t { loc; kind; in_scope = _ } =
  [%sexp_of: Kind.t Located.t] { txt = kind; loc }
;;

(* An instance needing pointer-free values is where it fails: the binding that
   gave it pointers, at [loc], is said in the reason. *)
let at_instance ~loc (error : Mode.Error.t) =
  match error with
  | Has_pointers { because; bound = { instance = Some instance; _ } as bound }
    ->
    let because =
      List.map because ~f:(fun (reason : Mode.Reason.t) ->
        if Loc.is_none reason.loc && not (Loc.equal loc instance)
        then { reason with loc }
        else reason)
    in
    Some (instance, Mode.Error.Has_pointers { because; bound })
  | Has_pointers _ | Mismatch _ | Unequal _ | Exceeds _ -> None
;;

let create ~loc (kind : Kind.t) =
  let relocated =
    match kind with
    | Mode_error error ->
      Option.map (at_instance ~loc error) ~f:(fun (loc, error) ->
        loc, Kind.Mode_error error)
    | Unification_error (Bound_var error) ->
      Option.map (at_instance ~loc error) ~f:(fun (loc, error) ->
        loc, Kind.Unification_error (Bound_var error))
    | _ -> None
  in
  let loc, kind = Option.value relocated ~default:(loc, kind) in
  { loc; kind; in_scope = None }
;;

let within t ~in_scope =
  match t.in_scope with
  | Some _ -> t
  | None -> { t with in_scope = Some (force in_scope) }
;;

let unification_error ~loc error =
  create ~loc (Unification_error (Unification_error.snapshot error))
;;

let already_reported = create ~loc:Loc.none Already_reported

let is_already_reported t =
  match t.kind with
  | Already_reported -> true
  | _ -> false
;;

let runtime_message error =
  match Runtime_error.of_error error with
  | Some error -> Runtime_error.message error
  | None -> Error.to_string_hum error
;;

let level_name level =
  match level with
  | 0 -> "runtime (level 0)"
  | level when level < 0 -> sprintf "compile time (level %d)" level
  | level -> sprintf "inside %d quote(s) (level %d)" level level
;;

(* Why a type has pointers is given once in a message: [said] are the reasons
   given already. *)
let mode_reasons ?(said = String.Hash_set.create ()) because =
  let candidates =
    List.filter_map because ~f:(fun ({ loc; what; shown } : Mode.Reason.t) ->
      let text =
        if Loc.is_none loc
        then what
        else sprintf "%s (at %s)" what (Loc.to_string loc)
      in
      match shown with
      | Always -> Some (`Shown, text)
      | If_alone -> Some (`If_alone, text)
      | Once ->
        Option.some_if
          (Hash_set.strict_add said text |> Hash_set.Ok_or_duplicate.is_ok)
          (`Shown, text))
  in
  let alone =
    List.for_all candidates ~f:(fun (kind, _) ->
      match kind with
      | `If_alone -> true
      | `Shown -> false)
  in
  List.filter_map candidates ~f:(fun (kind, text) ->
    match kind with
    | `Shown -> Some text
    | `If_alone -> Option.some_if alone text)
  |> List.remove_consecutive_duplicates ~equal:String.equal
  |> String.concat ~sep:", then "
;;

let because_clause = function
  | "" -> ""
  | reasons -> ", because " ^ reasons
;;

let mode_message : Mode.Error.t -> string =
  let point (type a) (axis : a Mode.Axis.t) (point : a) =
    let module C = (val Mode.Axis.chain axis) in
    Sexp.to_string (C.sexp_of_t point) |> String.lowercase
  in
  function
  | Has_pointers { because; bound } ->
    sprintf
      "This type has pointers%s, but must be pointer-free%s"
      (because_clause (mode_reasons because))
      (because_clause (mode_reasons [ bound.reason ]))
  | Mismatch { var = _; axis; lower; upper; lower_because; upper_because } ->
    let said = String.Hash_set.create () in
    let lower_because = mode_reasons ~said lower_because in
    let upper_because = mode_reasons ~said upper_because in
    sprintf
      "This is %s%s, but must be at most %s%s"
      (point axis lower)
      (because_clause lower_because)
      (point axis upper)
      (because_clause upper_because)
  | Unequal { axis; first; second; because } ->
    sprintf
      "The modes %s and %s must be equal%s"
      (point axis first)
      (point axis second)
      (because_clause (mode_reasons [ because ]))
  | Exceeds { axis; lower; upper; because } ->
    sprintf
      "This is %s, but must be at most %s%s"
      (point axis lower)
      (point axis upper)
      (because_clause (mode_reasons [ because ]))
;;

let unification_message (error : Unification_error.t) ~show_type =
  let printer =
    Type.Printer.create ~type_name:show_type ~name_mode_args:true ()
  in
  let ty = Type.Printer.ty printer in
  let row row = ty (TRecord { id = -1; row }) in
  let arg : Type.arg -> string = function
    | ATy t -> ty t
    | ARow r -> "row " ^ row r
    | AMode _ -> "a mode"
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
        | Modes error when String.equal expected actual ->
          "\nTheir modes differ, in ways types don't print: "
          ^ mode_message error
        | Modes error -> "\nTheir modes differ: " ^ mode_message error
        | Unknown_variances (name, error) ->
          sprintf
            "\n\
             The definition of %s is not known yet, as its splice is still \
             being run, so its arguments must be equal, but their modes \
             differ: %s"
            name
            (mode_message error)
      in
      sprintf
        "This expression has type %s but an expression was expected of type \
         %s%s"
        actual
        expected
        reason
    | Row_mismatch (expected, actual, reason) ->
      let row =
        match reason with
        | Assigned _ -> Type.Printer.context printer
        | Missing_label _ | Presence_mismatch _ | Cyclic_row -> row
      in
      let expected = row expected in
      let actual = row actual in
      let reason =
        match reason with
        | Missing_label label -> sprintf "\nThe label %s is missing." label
        | Presence_mismatch label ->
          sprintf
            "\nThe label %s is present in one and absent in the other."
            label
        | Assigned label ->
          sprintf
            "\nThe code of one assigns %s, which the other requires it not to."
            label
        | Cyclic_row -> "\nThe row would be cyclic."
      in
      sprintf
        "The row %s is incompatible with the row %s%s"
        actual
        expected
        reason
    | Occurs_check_failed var ->
      (* Named as the variable of the scheme it would be in, not as weak. *)
      let var : Type.arg =
        match var with
        | ATy (TVar var) -> ATy (TVar (Type.Var.create var.id Generic))
        | ARow (RVar var) -> ARow (RVar (Type.Var.create var.id Generic))
        | ATy _ | ARow _ | AMode _ -> var
      in
      sprintf "The type variable %s occurs inside its own definition" (arg var)
    | Assigned_in_record (name, assigned) ->
      sprintf
        "The row %s has %s assigned by code, but it is also the row of a \
         record or variant, whose entries can't be [mut]"
        (row assigned)
        name
    | Bound_var error -> mode_message error
  in
  message ^ Type.Printer.where printer
;;

let rec kind_message (kind : Kind.t) ~show_type =
  let decl_path : Syntax.Decl_path.t -> string = function
    | (Path _ | Root _) as path -> Syntax.Decl_path.to_string path
    | Resolved name -> show_type name
  in
  match kind with
  | Unification_error error -> unification_message error ~show_type
  | Not_in_scope path -> sprintf "Unbound value %s" (decl_path path)
  | Duplicate_toplevel name ->
    sprintf "The value %s is defined more than once in this module" name
  | Unused_substitution name ->
    sprintf
      "%s is substituted, but the spliced code has no free variable %s"
      name
      name
  | Duplicate_substitution name ->
    sprintf "The variable %s is substituted more than once" name
  | Variable_used_at_wrong_level { name; var_level; used_at } ->
    sprintf
      "The variable %s is bound at %s but used at %s; only toplevels may be \
       used at an earlier level"
      name
      (level_name var_level)
      (level_name used_at)
  | Free_in_spliced_code
      { name; unusable = { var_level; used_at = _; loc = _ } } ->
    sprintf
      "Unbound value %s, which the spliced code uses: %s is a free variable of \
       the quote that made it, as the %s in scope there, %s, is bound in \
       another quote"
      name
      name
      name
      (level_name var_level)
  | Not_embeddable { name; why = Defining } ->
    sprintf
      "The quote can't embed %s, as it is still being defined. Write [forget \
       %s in ...] to make %s a free variable of the quote"
      name
      name
      name
  | Not_embeddable { name; why = Unknown } ->
    sprintf
      "The quote can't embed %s, as it may not be portable. Bind it as (%s @ \
       portable) to embed it, or write [forget %s in ...] to make %s a free \
       variable of the quote"
      name
      name
      name
      name
  | Not_embeddable { name; why = Nonportable } ->
    sprintf
      "The quote can't embed %s, as it isn't portable. Write [forget %s in \
       ...] to make %s a free variable of the quote"
      name
      name
      name
  | Duplicate_forget name -> sprintf "%s is forgotten twice" name
  | Forgotten name ->
    sprintf
      "%s is not in scope here: it was forgotten by [forget %s in ...]"
      name
      name
  | Unbound_module path -> sprintf "Unbound module %s" (decl_path path)
  | Unbound_type path -> sprintf "Unbound type %s" (decl_path path)
  | Unbound_type_variable name -> sprintf "Unbound type variable '%s" name
  | Type_variable_kind_mismatch name ->
    sprintf
      "The variable %s is used as more than one of a type, a row and a mode"
      name
  | Unbound_constructor path ->
    sprintf "Unbound constructor %s" (decl_path path)
  | Unbound_field path -> sprintf "Unbound record field %s" (decl_path path)
  | No_such_constructor { type_name; constructor } ->
    sprintf
      "The variant type %s has no constructor %s"
      (show_type type_name)
      constructor
  | No_such_field { type_name; field } ->
    sprintf "The record type %s has no field %s" (show_type type_name) field
  | Type_arity_mismatch { type_name; expected; got } ->
    sprintf
      "The type %s expects %d argument(s), but is given %d"
      (decl_path type_name)
      expected
      got
  | Type_arg_kind_mismatch { type_name; param } ->
    let expected =
      match param with
      | Ty _ -> "a type"
      | Row _ -> "a row"
      | Mode _ -> "a mode"
    in
    sprintf
      "The parameter %s of type %s expects %s"
      (Syntax.Type_decl.Param.name param)
      (decl_path type_name)
      expected
  | Cyclic_type_alias name -> sprintf "The type abbreviation %s is cyclic" name
  | Non_regular_recursive_alias name ->
    sprintf
      "The recursive type abbreviation %s is used with different arguments in \
       its definition"
      name
  | Infinite_type_layout name ->
    sprintf
      "The type %s contains itself by value; break the cycle with ptr"
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
  | Repeated_field name ->
    sprintf "The field %s is given more than once in this record" name
  | Constructor_arity_mismatch { constructor; expected; got } ->
    sprintf
      "The constructor %s expects %d argument(s), but is applied to %d"
      (show_type constructor)
      expected
      got
  | Missing_field name -> sprintf "Some record fields are undefined: %s" name
  | Fields_of_different_types { field; expected_type; actual_type } ->
    sprintf
      "The record field %s belongs to the type %s but is mixed here with \
       fields of type %s"
      field
      (show_type actual_type)
      (show_type expected_type)
  | Empty_record -> "A record must have at least one field"
  | Misplaced_exception_pattern ->
    "An exception pattern is only allowed at the top of a match case, as in \
     [match e with | exception E -> ...]"
  | Match_without_value_cases ->
    "A match must have a case for the value, not only exception cases"
  | Non_exhaustive_match missing ->
    sprintf
      "This match is not exhaustive [non-exhaustive]: it does not cover `%s`"
      missing
  | Unused_arm ->
    "This case is unused [unused-arm]: the cases before it match every value \
     it does"
  | Unused_exception_arm ->
    "This exception case is unused [unused-arm]: the exception cases before it \
     match every exception it does"
  | Duplicate_pattern_variable name ->
    sprintf "The variable %s is bound several times in this matching" name
  | Or_pattern_variable_missing name ->
    sprintf
      "The variable %s is bound by only one side of this or-pattern, but both \
       sides must bind the same variables"
      name
  | Or_pattern_variable_type { name; error } ->
    sprintf
      "The variable %s has different types on the two sides of this \
       or-pattern: %s"
      name
      (unification_message error ~show_type)
  | Duplicate_parameter name ->
    sprintf "The parameter %s is bound several times in this function" name
  | Duplicate_row_entry name ->
    sprintf "The label %s is given more than once" name
  | Mutable_used_outside_its_code { name; var_level; used_at } ->
    sprintf
      "The mutable variable %s is bound at %s but used at %s: a [let mut] \
       local can only be used by the code that binds it, not in a quote or at \
       compile time"
      name
      (level_name var_level)
      (level_name used_at)
  | Compile_time_use_of_mutable { toplevel; mutable_ } ->
    if String.equal toplevel mutable_
    then
      sprintf
        "The mutable toplevel %s is used at compile time: mutable toplevels \
         exist only at runtime"
        toplevel
    else
      sprintf
        "The toplevel %s is used at compile time, but it may use the mutable \
         toplevel %s when run: mutable toplevels exist only at runtime"
        toplevel
        mutable_
  | Mode_error error -> mode_message error
  | Assign_to_immutable name ->
    sprintf
      "The variable %s is not mutable: only one bound by [let mut] can be \
       assigned to"
      name
  | Exclave_not_in_tail_position ->
    "[exclave_] ends the function's region, so must be the function's result, \
     in tail position"
  | Stack_not_on_allocation ->
    "[stack_] allocates its value on the stack, so must be on an allocation: a \
     closure, a quote, [Ptr.make e], or a record, tuple or constructor holding \
     one (records, tuples and constructors are values)"
  | Store_into_box ->
    "A box is immutable: only a pointer ([ptr]) can be stored to"
  | Store_not_through_pointer ->
    "Only a pointer can be stored to: [p.* <- v], or a field of it, as in \
     [p.*.x <- v]. Records are values, so a field is stored through a pointer \
     to the record"
  | Used_before_defined { name; shadows } ->
    sprintf
      "%s is used before its value is defined (a value may not depend on \
       itself)%s"
      name
      (if shadows
       then
         sprintf
           ". Lets are recursive, so this %s is the one being defined, not the \
            %s outside it: write `let nonrec %s` to use the outer one, or \
            rename one of them"
           name
           name
           name
       else "")
  | Toplevel_initialization_cycle cycle ->
    sprintf
      "The value of %s may be needed to compute itself: %s"
      (List.hd_exn cycle)
      (String.concat cycle ~sep:" -> ")
  | Compile_time_use_of_toplevel_being_checked name ->
    sprintf
      "The toplevel %s is used at compile time before it is fully checked (it \
       is in the same recursive group as this splice)"
      name
  | Expect_outside_expect_test ->
    "[%expect] is only allowed in the body of a let%expect_test where it runs \
     with the test: not in a quote's code nor in a toplevel splice"
  | Splice_evaluation_failed { code = _; error } ->
    sprintf
      "This toplevel splice failed at compile time: %s"
      (runtime_message error)
  | Malformed_syntax_splice error ->
    sprintf "This splice gave malformed syntax: %s" (Error.to_string_hum error)
  | Syntax_splice_with_substitutions ->
    "A splice of syntax (a Stag.Expr.t) is checked where it is, so it takes no \
     substitutions"
  | Expect_in_syntax_splice ->
    "A splice of syntax can't give an [%expect]: its expected output is \
     generated, so it could not be corrected"
  | Toplevel_splice_of_neither ty ->
    let printer =
      Type.Printer.create ~type_name:show_type ~name_mode_args:true ()
    in
    let ty = Type.Printer.ty printer ty in
    sprintf
      "This expression has type %s, but a toplevel splice is of code (of type \
       <'a>{..'r}) or of syntax (of type Stag.Expr.t)%s"
      ty
      (Type.Printer.where printer)
  | In_syntax_splice { generated = _; error } ->
    sprintf
      "In the expression this splice gave: %s"
      (kind_message error ~show_type)
  | Declaration_splice_failed error ->
    sprintf
      "This declaration splice failed at compile time: %s"
      (runtime_message error)
  | Malformed_struct_quote message -> message
  | Spliced_code_assigns_immutable name ->
    sprintf
      "The spliced code assigns %s (its context has %s : mut ...), so it must \
       be provided by a variable bound by [let mut] or a mutable toplevel"
      name
      name
  | Mode_variable_in_declaration "_" ->
    "The anonymous mode _ is not allowed in a declaration: its arrows have \
     fixed modes, written as atoms like [local], or a mode parameter declared \
     ['p : mode]"
  | Anonymous_mode_in_constraint ->
    "The anonymous mode _ can't be constrained: name a mode variable like 'm \
     instead"
  | Mode_variable_in_declaration var ->
    sprintf
      "The mode variable '%s is not allowed in a declaration unless it is a \
       parameter declared ['%s : mode]: its arrows have fixed modes"
      var
      var
  | Invalid_mode why -> sprintf "This mode is invalid: %s" why
  | Invalid_external { symbol; problem } ->
    let printer =
      Type.Printer.create ~type_name:show_type ~name_mode_args:true ()
    in
    let problem =
      match problem with
      | Not_a_function -> "it must be a function"
      | Argument ty ->
        sprintf
          "C functions can't take arguments of type %s, only int, C integer \
           types (C.int etc.), bool, char and string%s"
          (Type.Printer.ty printer ty)
          (Type.Printer.where printer)
      | Result ty ->
        sprintf
          "C functions can't return values of type %s, only int, C integer \
           types (C.int etc.), bool, char and unit%s"
          (Type.Printer.ty printer ty)
          (Type.Printer.where printer)
      | Unit_argument ->
        "unit can only be the only argument, of a C function of no arguments"
      | Pointer ty ->
        sprintf
          "%s would be passed to C as a pointer, which is not supported yet%s"
          (Type.Printer.ty printer ty)
          (Type.Printer.where printer)
      | Pointer_result ty ->
        sprintf
          "%s would be returned by C as a pointer, which is not supported yet%s"
          (Type.Printer.ty printer ty)
          (Type.Printer.where printer)
    in
    sprintf "The external %s has no C signature: %s" symbol problem
  | Invalid_external_symbol symbol ->
    sprintf
      "The C symbol %S is invalid: it must be non-empty, with no NUL character"
      symbol
  | Misplaced_modality ->
    "A modality is only written on a record field's type, a constructor's \
     argument or a pointer's type argument, as in [ptr[string @@ external]]"
  | Locality_in_mode_argument ->
    "A mode argument is on stage and provenance only ([const], [portable], \
     [nonportable], [static], [gc], [external]) or a mode variable, as in \
     [ptr[string @@ external]]: a pointer's cell is always [global], and a \
     type's mode parameters relate only stage and provenance"
  | Relative_locality_modality ->
    "A modality's locality can only be [global]: [regional] and [local] are \
     relative to a region, and a part read from a block returned to the \
     caller's region (with [exclave_]) would outlive the region it was \
     allocated in"
  | Misplaced_mut ->
    "Only an entry of a code type's context can be [mut] or [immut], for a \
     variable the code assigns or doesn't"
  | Misplaced_type_splice ->
    "A type splice runs at compile time, so it may be in a declaration, but \
     not within a quote or a compile-time expression"
  | Type_splice_failed error ->
    sprintf
      "This type splice failed at compile time: %s"
      (runtime_message error)
  | Malformed_type_splice error ->
    sprintf
      "This type splice gave malformed syntax: %s"
      (Error.to_string_hum error)
  | In_type_splice { generated = _; error } ->
    sprintf
      "In the type this type splice gave: %s"
      (kind_message error ~show_type)
  | Via_quote { toplevel; quote = _; error } ->
    sprintf
      "In this instance of %s, a use in its quote is wrong: %s"
      toplevel
      (kind_message error ~show_type)
  | Type_splice_cycle ->
    "This type splice's expression depends on the splice itself (eg. it uses \
     the declaration the splice is in), so it can't be evaluated"
  | Nested_type_splice ->
    "This type splice gave a type or definition that itself contains a type \
     splice: what a splice gives may not contain splices"
  | Type_definition_cycle { name; splice } ->
    sprintf
      "The type %s is needed here while its definition is being computed by \
       the type splice at %s, whose expression uses this code"
      name
      (Loc.to_string splice)
  | Malformed_declarations error ->
    sprintf
      "This declaration splice generated malformed declarations: %s"
      (Error.to_string_hum error)
  | Declaration_splice_cycle
      { name; module_ = None; splice; splice_kind = Type_definition } ->
    sprintf
      "%s can't be resolved here: the type splice at %s comes before this \
       point and may define it, but it is still being computed (its expression \
       uses this code). Move this code before the type or into another module"
      name
      (Loc.to_string splice)
  | Declaration_splice_cycle
      { name; module_ = None; splice; splice_kind = Include } ->
    sprintf
      "%s can't be resolved here: the include at %s comes before this point \
       and may define it, but it is still being generated (its expression uses \
       this code). Move this code before the include or into another module"
      name
      (Loc.to_string splice)
  | Declaration_splice_cycle
      { name; module_ = Some module_; splice; splice_kind = _ } ->
    sprintf
      "A declaration splice depends on what it generates: %s needs the \
       declarations of the module %s, which the splice at %s is generating, as \
       its expression uses this code"
      name
      module_
      (Loc.to_string splice)
  | Generated_after_include { error; _ }
  | Maybe_generated_by_running_include { error; _ } ->
    kind_message error ~show_type
  | Shadowed_prelude_module { error; module_; shadowed_by = _ } ->
    sprintf
      "%s (the prelude's module %s is shadowed by the program's %s)"
      (kind_message error ~show_type)
      module_
      module_
  | In_generated_declaration { declaration; generated = _; error } ->
    sprintf
      "In the declaration `%s` generated by this splice: %s"
      declaration
      (kind_message error ~show_type)
  | Already_reported -> "An error reported already"
  | Internal_error error ->
    sprintf "Internal error in the checker: %s" (Error.to_string_hum error)
;;

let message { kind; loc = _; in_scope } =
  let in_scope =
    match in_scope with
    | Some in_scope -> in_scope
    | None ->
      raise_s
        [%message
          "Typecheck_error.message: not within a module" (kind : Kind.t)]
  in
  kind_message kind ~show_type:(fun name ->
    Prelude_path.to_string_hum
      name
      ~shadowed:(In_scope.shadows in_scope)
      ~root_modules:in_scope.root_modules)
;;

let lift ~loc result = Result.map_error result ~f:(unification_error ~loc)
let error ~loc kind = Error (create ~loc kind)
