open! Import

(** A use of a variable in a quote, at [loc], where its binding, at
    [var_level], was unusable, so it is a free variable of the quote. *)
module Unusable_in_quote : sig
  type t =
    { loc : Loc.t
    ; var_level : Env.Level.t
    ; used_at : Env.Level.t
    }
  [@@deriving sexp_of]
end

module Kind : sig
  (** Names of toplevels, types, constructors and fields are canonical, except
      for paths ([Syntax.Decl_path.t]), which are as written. *)
  type t =
    | Unification_error of Unification_error.t
    | Not_in_scope of Syntax.Decl_path.t
    | Duplicate_toplevel of string
    | Duplicate_substitution of string
    | Variable_used_at_wrong_level of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
    | Free_in_spliced_code of
        { name : string
        ; unusable : Unusable_in_quote.t
        }
    (** Spliced where [name] is unbound, code from a quote in which [name] is
        free, as the binding of [name] there was [unusable]. *)
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
    | Non_regular_recursive_alias of string
    | Duplicate_module of string
    | Duplicate_type of string
    | Duplicate_type_parameter of
        { type_name : string
        ; param : string
        }
    | Duplicate_constructor of string
    | Duplicate_field of string
    | Repeated_field of string (** In a record expression or pattern. *)
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
    | Duplicate_pattern_variable of string
    | Duplicate_parameter of string
    | Duplicate_row_entry of string
    | Compile_time_use_of_toplevel_being_checked of string
    (** It can't be evaluated yet. *)
    | Expect_outside_expect_test
    | Splice_evaluation_failed of
        { code : Typed.Expr.t (** The toplevel splice's. *)
        ; error : Error.t
        }
    | Declaration_splice_failed of Error.t
    (** Evaluating the declaration splice's expression failed. *)
    | Malformed_declarations of Error.t
    (** What the declaration splice's expression evaluated to is not syntax
        (see [Syntax_value]). *)
    | Malformed_struct_quote of string
    | Misplaced_type_splice
    (** A type splice within a quote or compile-time code, where nothing
        evaluates it. *)
    | Type_splice_failed of Error.t
    (** Evaluating the type splice's expression failed. *)
    | Malformed_type_splice of Error.t
    (** What the type splice's expression evaluated to is not syntax. *)
    | In_type_splice of
        { generated : Syntax.Type_expr.t
        ; error : t
        }
    (** An error in the type [generated] the type splice here gave. *)
    | Type_splice_cycle
    (** Evaluating the type splice needs the type it gives. *)
    | Nested_definition_splice
    (** A type splice gave a definition that is itself a splice. *)
    | Type_definition_cycle of
        { name : string
        ; splice : Loc.t
        }
    (** The type [name] is needed here while the splice at [splice] computes
        its definition. *)
    | Declaration_splice_cycle of
        { name : string (** The path looked up. *)
        ; module_ : string option
          (** The spliced module, or [None] for an include. *)
        ; splice : Loc.t
        }
    (** What the declaration splice at [splice] generates is needed here,
        while generating it. *)
    | Generated_after_include of
        { error : t
        ; include_ : Loc.t
        }
    (** [error], for a name the include at [include_], after this point,
        generates. *)
    | Maybe_generated_by_running_include of
        { error : t
        ; include_ : Loc.t
        }
    (** [error], for a name the include at [include_], before this point and
        being generated, may define. *)
    | Shadowed_prelude_module of
        { error : t
        ; module_ : string
        ; shadowed_by : Loc.t
        }
    (** [error], for a path through the program's module [module_], declared
        at [shadowed_by], which the prelude's has. *)
    | In_generated_declaration of
        { declaration : string (** Eg. [let f], [type t]. *)
        ; generated : Syntax.Toplevel.t
        ; error : t
        }
    (** An error in a declaration generated by a declaration splice, located
        at the splice. *)
  [@@deriving sexp_of]
end

(** The names of the program's types and modules that shadow the prelude's
    where an error is: those declared, other than by an include, in the
    modules enclosing it. *)
module In_scope : sig
  type t =
    { types : String.Set.t
    ; modules : String.Set.t
    ; root_modules : String.Set.t (** The program's root modules. *)
    }
end

(** Where in the source the error is, [Loc.none] for code generated by a
    toplevel splice with no better location. [in_scope] is [None] until the
    error leaves the module it is in. *)
type t =
  { loc : Loc.t
  ; kind : Kind.t
  ; in_scope : In_scope.t option
  }

(** As [kind]'s, followed by [@ loc] unless [loc] is [Loc.none]. *)
val sexp_of_t : t -> Sexp.t

val create : loc:Loc.t -> Kind.t -> t
val unification_error : loc:Loc.t -> Unification_error.t -> t

(** [t] with [in_scope], unless it has one. *)
val within : t -> in_scope:In_scope.t Lazy.t -> t

(** A human readable message, without the location, as OCaml's [Error: ...].
    The prelude's names are written with [Prelude_path.root] where [in_scope]
    shadows them, which must be known. *)
val message : t -> string
