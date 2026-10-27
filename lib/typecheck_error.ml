open! Core

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
  | Splice_evaluation_failed of
      { code : Typed.Expr.t
      ; error : Error.t
      }
[@@deriving sexp_of, variants]
