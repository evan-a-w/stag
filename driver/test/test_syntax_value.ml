open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (error : Parser.Error.t)]
;;

let print_error error = print_endline (Error.to_string_hum error)

let prelude_signature =
  lazy
    (let stamps = Stamp_counter.create () in
     Typecheck.prelude_signature
       (Typecheck.create
          ~stamps
          ~interpreter:(Interpreter.create Treewalk.interpreter ~stamps)
          ()))
;;

let%expect_test "the prelude's signature" =
  print_string
    (Signature.to_string_hum ~of_prelude:true (Lazy.force prelude_signature));
  [%expect
    {|
    type box['a]
    type list['a] = Nil | Cons of box['a * list['a]]
    type option['a] = None | Some of 'a
    type result['a, 'b] = Ok of 'a | Error of 'b
    exception Division_by_zero
    exception Invalid_argument of string
    exception Failure of string
    val raise : exn -> 'a
    module Ptr : sig
      val make : 'a -> ptr['a]
    end
    module Box : sig
      val make : 'a -> box['a]
    end
    module Unsafe : sig
      val assume_global : 'a @ local -> 'a
    end
    module Stag : sig
      val map : ('a -> 'b) -> list['a] -> list['b]
      val append : list['a] -> list['a] -> list['a]
      module Type_ref : sig
        type t
        val name : t -> string
        module Path : sig
          type t = Program of list[string] | Prelude of list[string]
        end
        val path : t -> Path.t
      end
      module Constructor_ref : sig
        type t
        val name : t -> string
      end
      module Field_ref : sig
        type t
        val name : t -> string
      end
      module Value_ref : sig
        type t
        val name : t -> string
      end
      module Path : sig
        type t['a] = Name of string | Ref of 'a
      end
      module Internal : sig
        val constructor : <'a>{..'b} -> Path.t[Constructor_ref.t]
        val cons : Path.t[Constructor_ref.t]
        val nil : Path.t[Constructor_ref.t]
        val box_make : Path.t[Value_ref.t]
      end
      module Lit : sig
        type t = Int of int | Bool of bool | String of string | Char of char | Unit
      end
      module Mode_expr : sig
        module Axis : sig
          type t = Stage | Provenance | Locality
        end
        module Atom : sig
          type t = Const | Portable | Nonportable | Static | Gc | External | Global | Regional | Local
        end
        type t = Var of string | Atoms of list[Atom.t]
        module Term : sig
          type t = Mode of Mode_expr.t | Axis of Axis.t * Mode_expr.t | Meet of ptr[t * Atom.t] | Release of ptr[t] | Static_if_const of ptr[t] | Const_if_static of ptr[t]
        end
      end
      module Type_expr : sig
        type t = Var of string | Con of Path.t[Type_ref.t] * list[Arg.t] | Arrow of ptr[t * t] | Tuple of list[t] | Code of ptr[{`context : Row.t; `inner : t}] | Record of Row.t | Variant of Row.t | Mut of ptr[t] | Immut of ptr[t] | Splice of ptr[Expr.t] | Moded of ptr[t * Mode_expr.t] | Constrained of ptr[t * list[Mode_expr.Term.t * Mode_expr.Term.t]] | Modality of ptr[t * Mode_expr.t]
        module Row : sig
          type t = Empty | Extend of ptr[string * Type_expr.t * t] | Var of string
        end
        module Arg : sig
          type t = Ty of Type_expr.t | Row of Row.t | Mode of Mode_expr.t
        end
        val con : string -> list[t] -> t
        val tuple : list[t] -> t
      end
      module Type_decl : sig
        module Param : sig
          type t = Ty of string | Row of string | Mode of string
        end
        module Kind : sig
          type t = Variant of list[string * list[Type_expr.t]] | Record of list[string * Type_expr.t] | Alias of Type_expr.t | Abstract | Splice of Expr.t
        end
        type t = {`kind : Kind.t; `name : string; `params : list[Param.t]}
      end
      module Annot : sig
        type t = {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}
        val none : {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}
        val ty : Type_expr.t -> {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}
      end
      module Binding : sig
        type t = string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}
      end
      module Pattern : sig
        module Rest : sig
          type t = Closed | Open | Bind of string
        end
        type t = Any | Var of string | Lit of Lit.t | Tuple of list[t] | Construct of Path.t[Constructor_ref.t] * list[t] | Record of list[Path.t[Field_ref.t] * t] | Deref of ptr[t] | Poly_variant of ptr[string * option[t]] | Poly_record of {`fields : list[string * t]; `rest : Rest.t} | Exception of ptr[t]
        val construct : string -> list[t] -> t
        val list : list[t] -> t
        val tuple : list[t] -> t
      end
      module Expr : sig
        type t = Lit of Lit.t | Var of Path.t[Value_ref.t] * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]} | App of ptr[t * t] | Let of ptr[string * list[string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}] * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]} * t * t] | Let_mut of ptr[string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]} * t * t] | Assign of ptr[Path.t[Value_ref.t] * t] | Store of ptr[t * t] | Fun of ptr[list[string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}] * t] | Splice of ptr[{`expr : t; `subs : option[list[string * option[t]]]}] | Quote of ptr[t] | Tuple of list[t] | Deref of ptr[t] | Construct of Path.t[Constructor_ref.t] * list[t] | Record of list[Path.t[Field_ref.t] * t] | Field of ptr[t * Path.t[Field_ref.t]] | Record_update of ptr[t * list[Path.t[Field_ref.t] * t]] | Type of Type_expr.t | Match of ptr[t * list[Pattern.t * t]] | Poly_record of list[string * t] | Poly_field of ptr[t * string] | Poly_extend of ptr[list[string * t] * t] | Poly_restrict of ptr[t * string] | Poly_update of ptr[t * list[string * t]] | Poly_variant of ptr[string * option[t]] | Expect of string | Struct_quote of list[Toplevel.t] | Stack of ptr[t] | Exclave of ptr[t] | Region of ptr[t] | Nontail of ptr[t] | Annotated of ptr[t * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}] | Forget of ptr[list[string] * t]
        val var : string -> t
        val construct : string -> list[t] -> t
        val int : int -> t
        val string : string -> t
        val fun_ : list[string] -> t -> t
        val match_ : t -> list[Pattern.t * t] -> t
        val list : list[t] -> t
        val apply : t -> list[t] -> t
        val tuple : list[t] -> t
        val let_ : string -> list[string] -> t -> t -> t
      end
      module Toplevel : sig
        type t = Let of string * list[string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]}] * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]} * Expr.t | Let_mut of string * {`mode : option[Mode_expr.t]; `ty : option[Type_expr.t]} * Expr.t | Type of {`kind : Type_decl.Kind.t; `name : string; `params : list[Type_decl.Param.t]} | Exception of string * list[Type_expr.t] | External of {`name : string; `symbol : string; `ty : Type_expr.t} | Module of string * list[t] | Module_splice of string * Expr.t | Include of Expr.t | Expect_test of {`body : Expr.t; `name : string} | Bench of {`body : Expr.t; `name : string; `param : option[string * list[Expr.t]]}
        val let_ : string -> list[string] -> Expr.t -> t
      end
      module Program : sig
        type t = list[Toplevel.t]
      end
      module Code : sig
        val erase : <'a>{..'b} -> Expr.t
      end
      module Type : sig
        type t['a]
        module Kind : sig
          type t = Variant of list[Constructor_ref.t * list[Type_expr.t]] | Record of list[Field_ref.t * Type_expr.t] | Alias of Type_expr.t
        end
        module Decl : sig
          type t = { params : list[Type_decl.Param.t]; kind : Kind.t }
        end
        val expr : t['a] -> Type_expr.t
        val decl : Type_ref.t -> option[Decl.t]
      end
    end
    module Core : sig
      module Bool : sig
        val not : bool -> bool
        val equal : bool -> bool -> bool
        val compare : bool -> bool -> int
        val to_string : bool -> string
        val of_string : string -> option[bool]
        val of_string_exn : string -> bool
      end
      module Char : sig
        val equal : char -> char -> bool
        val code : char -> int
        val compare : char -> char -> int
        val of_code : int -> option[char]
        val of_code_exn : int -> char
        val to_string : char -> string
        val is_digit : char -> bool
        val is_lowercase : char -> bool
        val is_uppercase : char -> bool
        val is_alpha : char -> bool
        val is_alphanum : char -> bool
        val is_whitespace : char -> bool
        val is_print : char -> bool
        val lowercase : char -> char
        val uppercase : char -> char
        module Internal : sig
          val is_between : char -> char -> char -> bool
        end
      end
      module Int : sig
        val add : int -> int -> int
        val sub : int -> int -> int
        val mul : int -> int -> int
        val div : int -> int -> int
        val rem : int -> int -> int
        val neg : int -> int
        val equal : int -> int -> bool
        val compare : int -> int -> int
        val lt : int -> int -> bool
        val le : int -> int -> bool
        val gt : int -> int -> bool
        val ge : int -> int -> bool
        val succ : int -> int
        val pred : int -> int
        val max_value : int
        val min_value : int
        val abs : int -> int
        val min : int -> int -> int
        val max : int -> int -> int
        val to_string : int -> string
        val of_string : string -> option[int]
        val of_string_exn : string -> int
      end
      module List : sig
        val is_empty : list['a] -> bool
        val singleton : 'a -> list['a]
        val length : list['a] -> int
        val hd : list['a] -> option['a]
        val hd_exn : list['a] -> 'a
        val tl : list['a] -> option[list['a]]
        val tl_exn : list['a] -> list['a]
        val last : list['a] -> option['a]
        val last_exn : list['a] -> 'a
        val nth : list['a] -> int -> option['a]
        val nth_exn : list['a] -> int -> 'a
        val rev_append : list['a] -> list['a] -> list['a]
        val rev : list['a] -> list['a]
        val init : int -> (int -> 'a) -> list['a]
        val append : list['a] -> list['a] -> list['a]
        val fold_left : ('a -> 'b -> 'a) -> 'a -> list['b] -> 'a
        val fold_right : ('a -> 'b -> 'b) -> list['a] -> 'b -> 'b
        val rev_map : ('a -> 'b) -> list['a] -> list['b]
        val map : ('a -> 'b) -> list['a] -> list['b]
        val mapi : (int -> 'a -> 'b) -> list['a] -> list['b]
        val iter : ('a -> unit) -> list['a] -> unit
        val iteri : (int -> 'a -> unit) -> list['a] -> unit
        val filter_map : ('a -> option['b]) -> list['a] -> list['b]
        val filter : ('a -> bool) -> list['a] -> list['a]
        val partition : ('a -> bool) -> list['a] -> list['a] * list['a]
        val concat : list[list['a]] -> list['a]
        val concat_map : ('a -> list['b]) -> list['a] -> list['b]
        val count : ('a -> bool) -> list['a] -> int
        val find : ('a -> bool) -> list['a] -> option['a]
        val find_exn : ('a -> bool) -> list['a] -> 'a
        val find_map : ('a -> option['b]) -> list['a] -> option['b]
        val exists : ('a -> bool) -> list['a] -> bool
        val for_all : ('a -> bool) -> list['a] -> bool
        val mem : ('a -> 'b -> bool) -> 'a -> list['b] -> bool
        val equal : ('a -> 'b -> bool) -> list['a] -> list['b] -> bool
        val compare : ('a -> 'b -> int) -> list['a] -> list['b] -> int
        val take : int -> list['a] -> list['a]
        val drop : int -> list['a] -> list['a]
        val zip : list['a] -> list['b] -> option[list['a * 'b]]
        val zip_exn : list['a] -> list['b] -> list['a * 'b]
        val unzip : list['a * 'b] -> list['a] * list['b]
        val map2 : ('a -> 'b -> 'c) -> list['a] -> list['b] -> option[list['c]]
        val map2_exn : ('a -> 'b -> 'c) -> list['a] -> list['b] -> list['c]
        val iter2 : ('a -> 'b -> unit) -> list['a] -> list['b] -> option[unit]
        val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> list['b] -> list['c] -> option['a]
        val max_elt : ('a -> 'a -> int) -> list['a] -> option['a]
        val min_elt : ('a -> 'a -> int) -> list['a] -> option['a]
        val assoc : ('a -> 'b -> bool) -> 'a -> list['b * 'c] -> option['c]
        val assoc_exn : ('a -> 'b -> bool) -> 'a -> list['b * 'c] -> 'c
        val sort : ('a -> 'a -> int) -> list['a] -> list['a]
        val dedup : ('a -> 'a -> int) -> list['a] -> list['a]
      end
      module Lru : sig
        type link['a, 'b, 'm0 : mode] = Nil | Entry of ptr[entry['a, 'b, 'm0]]
        type entry['a, 'b, 'm0 : mode] = { key : 'a; value : 'b; prev : link['a, 'b, 'm0]; next : link['a, 'b, 'm0] }
        type t['a, 'b, 'm0 : mode] = { capacity : int; compare : 'a -> 'a -> int; index : ptr[Map.t['a, ptr[entry['a, 'b, 'm0]], gc]]; first : ptr[link['a, 'b, 'm0]]; last : ptr[link['a, 'b, 'm0]]; size : ptr[int] }
        val create : int -> ('a -> 'a -> int) -> t['a, 'b, 'm0]
        val length : t['a, 'b, 'm0] -> int
        module Internal : sig
          val unlink : t['a, 'b, 'm0] -> ptr[entry['a, 'b, 'm1]] -> unit
          val push_front : t['a, 'b, 'm0] -> ptr[entry['a, 'b, 'm1]] -> unit
          val use : t['a, 'b, 'm0] -> ptr[entry['a, 'b, 'm1]] -> unit
          val evict_last : t['a, 'b, 'm0] -> unit
        end
        val find : t['a, 'b, 'm0] -> 'a -> option['b]
        val add : t['a, 'b, 'm0] -> 'a -> 'b -> unit
        val to_list : t['a, 'b, 'm0] -> list['a * 'b]
      end
      module Map : sig
        type t['a, 'b, 'm0 : mode] = Empty | Node of box[node['a, 'b, 'm0]]
        type node['a, 'b, 'm0 : mode] = { left : t['a, 'b, 'm0]; key : 'a; value : 'b @@ 'm0; right : t['a, 'b, 'm0]; height : int }
        val empty : t['a, 'b, 'm0]
        val is_empty : t['a, 'b, 'm0] -> bool
        val height : t['a, 'b, 'm0] -> int
        module Internal : sig
          val max : int -> int -> int
          val create : t['a, 'b, 'm0] -> 'a -> 'b -> t['a, 'b, 'm0] -> t['a, 'b, 'm1]
          val bal : t['a, 'b, 'm0] -> 'a -> 'b -> t['a, 'b, 'm1] -> t['a, 'b, 'm2]
          val onto : t['a, 'b, 'm0] -> list['a * 'b] -> list['a * 'b]
          val remove_min : t['a, 'b, 'm0] -> t['a, 'b, 'm1]
          val merge : t['a, 'b, 'm0] -> t['a, 'b, 'm1] -> t['a, 'b, 'm1]
        end
        val singleton : 'a -> 'b -> t['a, 'b, 'm0]
        val add : ('a -> 'a -> int) -> t['a, 'b, 'm0] -> 'a -> 'b -> t['a, 'b, 'm1]
        val find : ('a -> 'b -> int) -> t['b, 'c, 'm0] -> 'a -> option['c]
        val mem : ('a -> 'b -> int) -> t['b, 'c, 'm0] -> 'a -> bool
        val min_binding : t['a, 'b, 'm0] -> option['a * 'b]
        val remove : ('a -> 'b -> int) -> t['b, 'c, 'm0] -> 'a -> t['b, 'c, 'm1]
        val fold : ('a -> 'b -> 'c -> 'c) -> t['a, 'b, 'm0] -> 'c -> 'c
        val iter : ('a -> 'b -> 'c) -> t['a, 'b, 'm0] -> unit
        val cardinal : t['a, 'b, 'm0] -> int
        val to_list : t['a, 'b, 'm0] -> list['a * 'b]
        val of_list : ('a -> 'a -> int) -> list['a * 'b] -> t['a, 'b, 'm0]
      end
      module Option : sig
        val some : 'a -> option['a]
        val is_some : option['a] -> bool
        val is_none : option['a] -> bool
        val value : option['a] -> 'a -> 'a
        val value_exn : option['a] -> 'a
        val value_map : ('a -> 'b) -> 'b -> option['a] -> 'b
        val map : ('a -> 'b) -> option['a] -> option['b]
        val bind : option['a] -> ('a -> option['b]) -> option['b]
        val join : option[option['a]] -> option['a]
        val iter : ('a -> unit) -> option['a] -> unit
        val fold : ('a -> 'b -> 'a) -> 'a -> option['b] -> 'a
        val exists : ('a -> bool) -> option['a] -> bool
        val for_all : ('a -> bool) -> option['a] -> bool
        val filter : ('a -> bool) -> option['a] -> option['a]
        val first_some : option['a] -> option['a] -> option['a]
        val both : option['a] -> option['b] -> option['a * 'b]
        val to_list : option['a] -> list['a]
        val equal : ('a -> 'b -> bool) -> option['a] -> option['b] -> bool
      end
      module Result : sig
        val is_ok : result['a, 'b] -> bool
        val is_error : result['a, 'b] -> bool
        val map : ('a -> 'b) -> result['a, 'c] -> result['b, 'c]
        val map_error : ('a -> 'b) -> result['c, 'a] -> result['c, 'b]
        val bind : result['a, 'b] -> ('a -> result['c, 'b]) -> result['c, 'b]
        val join : result[result['a, 'b], 'b] -> result['a, 'b]
        val iter : ('a -> unit) -> result['a, 'b] -> unit
        val ok : result['a, 'b] -> option['a]
        val error : result['a, 'b] -> option['b]
        val ok_exn : result['a, 'b] -> 'a
        val of_option : option['a] -> 'b -> result['a, 'b]
        val all : list[result['a, 'b]] -> result[list['a], 'b]
        val combine : ('a -> 'b -> 'c) -> ('d -> 'd -> 'd) -> result['a, 'd] -> result['b, 'd] -> result['c, 'd]
        val equal : ('a -> 'b -> bool) -> ('c -> 'd -> bool) -> result['a, 'c] -> result['b, 'd] -> bool
      end
      module Sexp : sig
        type t = Atom of string | List of list[t]
        val equal : t -> t -> bool
        val to_string : t -> string
        val to_string_hum : t -> string
        module Parse_error : sig
          type t = { message : string; offset : int; line : int; col : int }
          val create : string -> int -> string -> t
          val to_string : t -> string
          val sexp_of_t : t -> Sexp.t
          val t_of_sexp_exn : Sexp.t -> t
          val t_of_sexp : Sexp.t -> result[t, Of_sexp_error.t]
        end
        val of_string : string -> result[t, Parse_error.t]
        val of_string_many : string -> result[list[t], Parse_error.t]
        exception Parse_error of Parse_error.t
        val of_string_exn : string -> t
        val of_string_many_exn : string -> list[t]
        module Of_sexp_error : sig
          type t = { message : string; sexp : Sexp.t }
          val create : string -> Sexp.t -> t
          val to_string : t -> string
          val sexp_of_t : t -> Sexp.t
          val t_of_sexp_exn : Sexp.t -> t
          val t_of_sexp : Sexp.t -> result[t, t]
        end
        exception Of_sexp_error of Of_sexp_error.t
        val of_sexp_exn : ('a -> result['b, Of_sexp_error.t]) -> 'a -> 'b
        val sexp_of_unit : unit -> t
        val unit_of_sexp : t -> result[unit, Of_sexp_error.t]
        val sexp_of_bool : bool -> t
        val bool_of_sexp : t -> result[bool, Of_sexp_error.t]
        val sexp_of_int : int -> t
        val int_of_sexp : t -> result[int, Of_sexp_error.t]
        val sexp_of_char : char -> t
        val char_of_sexp : t -> result[char, Of_sexp_error.t]
        val sexp_of_string : string -> t
        val string_of_sexp : t -> result[string, Of_sexp_error.t]
        val sexp_of_list : ('a -> t) -> list['a] -> t
        val sexp_of_ptr : ('a -> 'b) -> ptr['a] -> 'b
        val sexp_of_box : ('a -> 'b) -> box['a] -> 'b
        val list_of_sexp : (t -> result['a, Of_sexp_error.t]) -> t -> result[list['a], Of_sexp_error.t]
        val sexp_of_option : ('a -> t) -> option['a] -> t
        val option_of_sexp : (t -> result['a, Of_sexp_error.t]) -> t -> result[option['a], Of_sexp_error.t]
        val sexp_of_pair : ('a -> t) -> ('b -> t) -> 'a * 'b -> t
        val pair_of_sexp : (t -> result['a, Of_sexp_error.t]) -> (t -> result['b, Of_sexp_error.t]) -> t -> result['a * 'b, Of_sexp_error.t]
        val sexp_of_triple : ('a -> t) -> ('b -> t) -> ('c -> t) -> 'a * 'b * 'c -> t
        val triple_of_sexp : (t -> result['a, Of_sexp_error.t]) -> (t -> result['b, Of_sexp_error.t]) -> (t -> result['c, Of_sexp_error.t]) -> t -> result['a * 'b * 'c, Of_sexp_error.t]
        val sexp_of_result : ('a -> t) -> ('b -> t) -> result['a, 'b] -> t
        val result_of_sexp : (t -> result['a, Of_sexp_error.t]) -> (t -> result['b, Of_sexp_error.t]) -> t -> result[result['a, 'b], Of_sexp_error.t]
        val sexp_of_t : t -> t
        val t_of_sexp : t -> result[t, Of_sexp_error.t]
        val unit_of_sexp_exn : t -> unit
        val bool_of_sexp_exn : t -> bool
        val int_of_sexp_exn : t -> int
        val char_of_sexp_exn : t -> char
        val string_of_sexp_exn : t -> string
        val list_of_sexp_exn : (t -> result['a, Of_sexp_error.t]) -> t -> list['a]
        val ptr_of_sexp : ('a -> result['b, 'c]) -> 'a -> result[ptr['b], 'c]
        val ptr_of_sexp_exn : ('a -> result['b, Of_sexp_error.t]) -> 'a -> ptr['b]
        val box_of_sexp : ('a -> result['b, 'c]) -> 'a -> result[box['b], 'c]
        val box_of_sexp_exn : ('a -> result['b, Of_sexp_error.t]) -> 'a -> box['b]
        val option_of_sexp_exn : (t -> result['a, Of_sexp_error.t]) -> t -> option['a]
        val result_of_sexp_exn : (t -> result['a, Of_sexp_error.t]) -> (t -> result['b, Of_sexp_error.t]) -> t -> result['a, 'b]
        val t_of_sexp_exn : t -> t
        val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
        val sexp_of : Stag.Type.t['a] -> Stag.Expr.t
        val of_sexp : Stag.Type.t['a] -> Stag.Expr.t
        val of_sexp_raising : Stag.Type.t['a] -> Stag.Expr.t
        module Deriving : sig
          val field : string -> t -> t
          val catch : ('a -> 'b) -> 'a -> result['b, Of_sexp_error.t]
          val capitalize : t -> t
          val variant_error : string -> list[string] -> list[string] -> t -> 'a
          val poly_variant_error : string -> list[string] -> list[string] -> t -> 'a
          val tuple_error : string -> int -> t -> 'a
          val record_fields : string -> list[string] -> t -> list[t]
        end
        module Internal : sig
          val of_sexp_error : string -> t -> result['a, Of_sexp_error.t]
          val of_sexp_fail : string -> string -> t -> 'a
          val record_fields_any_order : string -> list[string] -> list[t] -> t -> list[t]
          module Variant_error : sig
            type t = { requires_args : string; unexpected : string; takes_no_args : string; tag : string; invalid : string }
            val sum : t
            val poly : t
            val raise_ : t -> string -> list[string] -> list[string] -> Sexp.t -> Sexp.t -> 'a
          end
        end
      end
      module Sexp_derive : sig
        val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
        val expr : string -> Dir.t -> Stag.Type.t['a] -> Stag.Expr.t
        module Dir : sig
          type t = To_sexp | Of_sexp | Of_sexp_exn
          val conv_name : t -> string -> string
        end
        module Context : sig
          type t = { self : option[list[string]]; name : string; generator : string }
        end
        module Case : sig
          type t = { tag : string; args : list[Stag.Type_expr.t]; construct : list[Stag.Expr.t] -> Stag.Expr.t; pattern : list[Stag.Pattern.t] -> Stag.Pattern.t }
        end
        val failure : string -> string -> exn
        val erase : <'a>{..'b} -> Stag.Expr.t
        val var : string -> Stag.Expr.t
        val pvar : string -> Stag.Pattern.t
        val apply : Stag.Expr.t -> list[Stag.Expr.t] -> Stag.Expr.t
        val atom_c : Stag.Path.t[Stag.Constructor_ref.t]
        val list_c : Stag.Path.t[Stag.Constructor_ref.t]
        val atom : string -> Stag.Expr.t
        val atom_pattern : string -> Stag.Pattern.t
        val sexp_list : list[Stag.Expr.t] -> Stag.Expr.t
        val list_pattern : list[Stag.Pattern.t] -> Stag.Pattern.t
        val strings : list[string] -> Stag.Expr.t
        val fst : 'a * 'b -> 'a
        val snd : 'a * 'b -> 'b
        val vars : string -> int -> list[string]
        val param_conv : string -> string
        val is_prelude_type : list[string] -> Stag.Type_expr.t -> bool
        module Builtin : sig
          type t = { path : list[string]; to_sexp : Stag.Expr.t; of_sexp : Stag.Expr.t; of_sexp_exn : Stag.Expr.t }
          val create : list[string] -> <'a>{..'b} -> <'c>{..'d} -> <'e>{..'f} -> t
          val all : list[t]
          val find : list[string] -> option[t]
          val conv : t -> Dir.t -> Stag.Expr.t
        end
        val is_sexp : Stag.Type_expr.t -> bool
        val named : Context.t -> Dir.t -> Stag.Type_ref.t -> Stag.Expr.t
        val conv : Context.t -> Dir.t -> Stag.Type_expr.t -> Stag.Expr.t
        val conv_in_place : Context.t -> Dir.t -> Stag.Type_expr.t -> Stag.Expr.t
        val to_sexp : Context.t -> Stag.Type_expr.t -> Stag.Expr.t -> string -> Stag.Expr.t
        val bind : Context.t -> Stag.Type_expr.t -> string -> Stag.Pattern.t * Stag.Expr.t
        val bind_tuple : Context.t -> list[Stag.Type_expr.t] -> string -> Stag.Pattern.t * Stag.Expr.t
        val of_sexp : Context.t -> Stag.Type_expr.t -> string -> string -> Stag.Expr.t
        val of_sexps : Context.t -> list[Stag.Type_expr.t] -> list[string] -> list[Stag.Expr.t]
        val tuple_of_sexp : Context.t -> list[Stag.Type_expr.t] -> string -> string -> Stag.Expr.t
        val nominal_case : Stag.Constructor_ref.t * list[Stag.Type_expr.t] -> Case.t
        val poly_cases : Context.t -> Stag.Type_expr.Row.t -> list[Case.t]
        val variant_to_sexp : Context.t -> list[Case.t] -> Stag.Expr.t -> string -> Stag.Expr.t
        val variant_of_sexp : Context.t -> bool -> list[Case.t] -> string -> string -> Stag.Expr.t
        val record_to_sexp : Context.t -> list[Stag.Field_ref.t * Stag.Type_expr.t] -> Stag.Expr.t
        val record_of_sexp : Context.t -> list[Stag.Field_ref.t * Stag.Type_expr.t] -> string -> Stag.Expr.t
        val conv_type : Dir.t -> Stag.Type_expr.t -> Stag.Type_expr.t
        val has_variant : Stag.Type_expr.t -> bool
        val derive_failure : string -> exn
        val declared : Stag.Type.t['a] -> string * Stag.Type.Decl.t * list[string]
        val params : Stag.Type.Decl.t -> list[string]
        val decl_bodies : Context.t -> Stag.Type.Decl.t -> Stag.Expr.t * Stag.Expr.t
        val decl_of_sexp : Context.t -> Stag.Type.Decl.t -> string -> list[string] -> Stag.Expr.t
      end
      module Sexp_parser : sig
        val one : string -> result[Sexp.t, Sexp.Parse_error.t]
        val many : string -> result[list[Sexp.t], Sexp.Parse_error.t]
        val fail : string -> int -> string -> result['a, Sexp.Parse_error.t]
        module Context : sig
          type t['a] = { single : bool; frames : list[list['a]]; depth : int; ignoring : list[int]; full : int }
          val create : bool -> t['a]
          val ignoring : t['a] -> bool
          val check_new : string -> int -> t['a] -> result[t['a], Sexp.Parse_error.t]
          val added : string -> int -> t['a] -> list[list['b]] -> int -> result[t['b], Sexp.Parse_error.t]
          val add : string -> int -> t['a] -> 'a -> result[t['a], Sexp.Parse_error.t]
          val open_ : string -> int -> t['a] -> result[t['a], Sexp.Parse_error.t]
          val close : string -> int -> t[Sexp.t] -> result[t[Sexp.t], Sexp.Parse_error.t]
          val start_comment : t['a] -> t['a]
          val finish : string -> t['a] -> result[list['a], Sexp.Parse_error.t]
        end
        val at_end : string -> int -> bool
        val next_char : string -> int -> option[char]
        val is_blank : char -> bool
        val ends_atom : char -> bool
        val escape_digits : string -> int -> int -> int -> string -> string -> result[int * int, Sexp.Parse_error.t]
        val decimal_escape : string -> int -> string -> result[int * int, Sexp.Parse_error.t]
        val hex_escape : string -> int -> string -> result[int * int, Sexp.Parse_error.t]
        val between : string -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val after_cr : string -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val line_comment : string -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val atom : string -> int -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val after_hash : string -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val quoted : string -> int -> int -> list[string] -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val escape : string -> int -> list[string] -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val escaped_cr : string -> int -> list[string] -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val continuation : string -> int -> list[string] -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val block_comment : string -> int -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
        val comment_string : string -> int -> int -> Context.t[Sexp.t] -> result[list[Sexp.t], Sexp.Parse_error.t]
      end
      module Sexp_printer : sig
        val to_string : Sexp.t -> string
        val to_string_hum : Sexp.t -> string
        val must_escape : string -> bool
        val quote : string -> string
        module Pretty : sig
          module Box : sig
            type t = Hov | B | Fits
          end
          module Token : sig
            type t = Text of string | Begin of Box.t * int * int * int | End | Break of int * int | Newline
            val length : t -> int
          end
          val margin : int
          val max_indent : int
          val infinity : int
          module State : sig
            type t = { space_left : int; indent : int; new_line : bool; boxes : list[Box.t * int]; front : list[Token.t]; back : list[Token.t]; left : int; right : int; out : list[string] }
            val init : t
            val output : t -> string -> t
            val new_line : t -> int -> int -> t
            val same_line : t -> t
          end
          val tokens : Sexp.t -> list[Token.t] -> int -> list[Token.t] * int * int
          val format : State.t -> Token.t -> int -> State.t
          val advance : State.t -> int -> State.t
          val run : State.t -> list[Token.t] -> int -> State.t
        end
      end
      module String : sig
        val length : string -> int
        val get : string -> int -> char
        val sub : string -> int -> int -> string
        val append : string -> string -> string
        val equal : string -> string -> bool
        val compare : string -> string -> int
        val is_empty : string -> bool
        val of_char : char -> string
        val concat : string -> list[string] -> string
        val init : int -> (int -> char) -> string
        val make : int -> char -> string
        val to_list : string -> list[char]
        val of_list : list[char] -> string
        val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
        val iter : (char -> unit) -> string -> unit
        val map : (char -> char) -> string -> string
        val lowercase : string -> string
        val uppercase : string -> string
        val capitalize : string -> string
        val for_all : (char -> bool) -> string -> bool
        val exists : (char -> bool) -> string -> bool
        val index : string -> char -> option[int]
        val rindex : string -> char -> option[int]
        val index_exn : string -> char -> int
        val rindex_exn : string -> char -> int
        val contains : string -> char -> bool
        val prefix : string -> int -> string
        val suffix : string -> int -> string
        val drop_prefix : string -> int -> string
        val drop_suffix : string -> int -> string
        val starts_with : string -> string -> bool
        val ends_with : string -> string -> bool
        val drop_while : (char -> bool) -> string -> string
        val strip : string -> string
        val split_on_char : char -> string -> list[string]
        val split_on_string : string -> string -> list[string]
        val escaped : string -> string
        module Internal : sig
          val fold_range : ('a -> int -> 'a) -> 'a -> int -> int -> 'a
          val escape_char : char -> string
        end
      end
    end
    |}]
;;

(* The refs of the constructors in [value], by name. *)
let rec constructors_of (value : Treewalk.Value.t) =
  let union values =
    List.map values ~f:constructors_of
    |> List.fold ~init:String.Map.empty ~f:(fun acc refs ->
      Map.merge_skewed acc refs ~combine:(fun ~key:_ ref _ -> ref))
  in
  match value with
  | Construct (ref, args) -> Map.set (union args) ~key:ref.name ~data:ref
  | Tuple values -> union values
  | Poly_record fields -> union (Map.data fields)
  | Ptr cell -> constructors_of !cell
  | Box value -> constructors_of value
  | _ -> String.Map.empty
;;

let round_trip_sources =
  [ {|
type nat = Z | S of nat
type list['a] = Nil | Cons of box['a * list['a]]
type with_x[{..'r}] = {`x : int; ..'r}
type point = {x : int; y : M.t}
type void = |
type t
type f = int -> bool * string
type tags[{..'r}] = [`A of int | `B; ..'r]
type closed = [`A | `B of {}]
type code['a] = <'a>{x : int; y : bool; ..'r} -> <int>{}
type assigns = <unit>{x : mut int; y : mut (int -> int); z : immut int}
type args = with_x[{`y : int}] * list[{}] * list['a] * with_x[{..'r}]
|}
  ; {|
let f x (y : int) (_ : bool) : t = x
let lits = (1, true, false, 'c', "s\n", ())
let g = M.N.x
let h = (x : list['a])
let app = f x y
let fns = (fun x -> x, fun (x : int) _ -> fun y -> x)
let l = let z = 1 in let w (a : int) : int = a in z; w
let c = (C, C 1, C (1, 2), C ((1, 2)), M.C x)
let r = ({x = 1; M.y = 2}, r.x, r.M.x, {r with x = 1; M.y = 2})
let m = match e with | _ -> 1 | x -> 2 | 1 -> 3 | 'c' -> 4 | "s" -> 5 | true -> 6 | () -> 7
let p = match e with (a, b) -> a | C -> 1 | C p -> 2 | M.C (p, q) -> 3 | C ((p, q)) -> 3 | {x = p; M.y = q} -> 4
let q = match e with `A -> 1 | `A p -> 2 | {`x = p} -> 3 | {`x = p; ..} -> 4 | {`x = p; ..rest} -> 5
let poly = ({`x = 1; `y = true}, r.`x, {`x = v; `y = w | r}, r \ `x, {r with `x = v}, `A, `A e)
let staged = .< .~e >.
let splices = (.~(f x), .~e.{}, .~e.{x = e1; y})
let types = (type t, type M.t, type list['a], type (int * t), f (type t))
let exns = match f x with y -> y | exception E -> 1 | exception M.F (a, b) -> 2 | exception e -> raise e
let%bench "fib" = fib 20
let%bench "sort" n in [100; f 1000] = sort n
let mut counter = 0
let mut xs : list[int] = []
let mutation = let mut x = 1 in let mut y : int = 2 in x <- y; M.counter <- x
|}
  ; {stag|
module M = struct
  let x = 1
  module N = struct
    type t = A
  end
end
let%expect_test "power" =
  print_int (Int.add 3 2);
  [%expect {| 5 |}];
  [%expect]
module G = .~(gen 1)
include .~gen
exception E
exception F of int * list['a]
exception G of (int * string)
|stag}
  ; {|
type spliced = .~kind
let quoted = .< struct type t = .~kind let x : .~ty = .~e include .~items end >.
|}
  ; {|
type r = {x : int @@ global; y : (int @ local -> int @ 'm) @@ global; z : int}
type v = A of int @@ global * int | B of (int -> int) @@ global
let f (x @ local) (y : int @ 'm) : int @ global portable = exclave_ stack_ (x, y)
let in_region (u : unit) = region_ (let x = stack_ (Some 1) in exclave_ stack_ (Some 2))
let g (h : int @ local -> int @ 'm -> (int -> int @ local) @ global) = h 1 [@nontail]
let annotated = ((f : (int -> int) @ local), (f @ local portable gc))
let annotated_exprs = ((f 1 : int @ local), (stack_ (Some 1) @ local), (((x : int)) : int))
let x @ const = stack_ (Some (f x [@nontail]), `A (g y [@nontail]))
let mut counter : int @ static = 0
let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c with 'm <= 'n and portable <= 'm = compose
let h (x : int @ 'm with 'm <= portable) = (x : int with const <= 'm)
|}
  ]
;;

(* By name, those [to_value] makes from [round_trip_sources]. *)
let round_trip_constructors =
  List.map round_trip_sources ~f:(fun source ->
    constructors_of (Syntax_value.Program.to_value (parse source)))
  |> List.reduce_exn ~f:(Map.merge_skewed ~combine:(fun ~key:_ ref _ -> ref))
;;

let%expect_test "round trips, modulo locations" =
  List.iter round_trip_sources ~f:(fun source ->
    let program = parse source in
    match
      Syntax_value.Program.of_value
        (Syntax_value.Program.to_value program)
        ~loc:Loc.none
    with
    | Error error -> print_error error
    | Ok program' ->
      print_s
        [%message "" ~equal:(Syntax.Program.equal program program' : bool)]);
  [%expect
    {|
    (equal true)
    (equal true)
    (equal true)
    (equal true)
    (equal true)
    |}]
;;

let%expect_test "constructors are made as the typechecker resolves them" =
  let signature =
    List.filter (Lazy.force prelude_signature) ~f:(function
      | Module { name = "Core"; _ } | Type { name = "result"; _ } -> false
      | _ -> true)
  in
  (* A function matching every constructor of each variant of the prelude that
     syntax values may use. *)
  let rec probes path (items : Signature.t) =
    List.concat_map items ~f:(function
      | Module { name; items } -> probes (path @ [ name ]) items
      | Type { kind = Variant constructors; _ } ->
        [ List.map constructors ~f:(fun { name; args } ->
            let args =
              match List.length args with
              | 0 -> ""
              | 1 -> " _"
              | n ->
                sprintf
                  " (%s)"
                  (String.concat ~sep:", " (List.init n ~f:(Fn.const "_")))
            in
            sprintf "%s%s -> 0" (String.concat ~sep:"." (path @ [ name ])) args)
          |> String.concat ~sep:" | "
        ]
      | Type _ | Exception _ | Value _ -> [])
  in
  let source =
    List.mapi
      (probes [] signature)
      ~f:(sprintf "let probe%d x = match x with %s")
    |> String.concat ~sep:"\n"
  in
  let typechecker =
    match Driver.check_files [ { path = "probe.stag"; contents = source } ] with
    | Error error -> raise_s [%sexp (error : Driver.Error.t list)]
    | Ok driver ->
      List.concat_map (Driver.checked driver).program.sccs ~f:(fun scc ->
        List.concat_map scc.members ~f:(fun (member : Typed.Toplevel.t) ->
          match member.body.desc with
          | Match { cases; _ }
            when String.is_prefix member.ident.name ~prefix:"Probe." ->
            List.map cases ~f:(fun ((pattern : Typed.Pattern.t), _) ->
              match pattern.desc with
              | Construct (ref, _) -> ref.name, ref
              | _ -> raise_s [%message "Not a constructor pattern"])
          | _ -> []))
      |> String.Map.of_alist_exn
  in
  (* Those not made from source are made only by reflection. *)
  let sexp_of_ref = [%sexp_of: Typed.Constructor_ref.t] in
  let reflected =
    Map.keys typechecker
    |> List.filter ~f:(fun name -> not (Map.mem round_trip_constructors name))
  in
  print_s
    [%message
      ""
        ~constructors:(Map.length typechecker : int)
        (reflected : string list)
        ~others_same_as_to_value:
          (Map.for_alli round_trip_constructors ~f:(fun ~key ~data ->
             Option.exists (Map.find typechecker key) ~f:(fun ref ->
               Sexp.equal (sexp_of_ref ref) (sexp_of_ref data)))
           : bool)];
  [%expect
    {|
    ((constructors 118)
     (reflected
      (prelude.Stag.Expr.Deref prelude.Stag.Expr.Forget prelude.Stag.Expr.Store
       prelude.Stag.Mode_expr.Atom.External
       prelude.Stag.Mode_expr.Atom.Nonportable
       prelude.Stag.Mode_expr.Atom.Regional prelude.Stag.Mode_expr.Axis.Locality
       prelude.Stag.Mode_expr.Axis.Provenance prelude.Stag.Mode_expr.Axis.Stage
       prelude.Stag.Mode_expr.Term.Axis
       prelude.Stag.Mode_expr.Term.Const_if_static
       prelude.Stag.Mode_expr.Term.Meet prelude.Stag.Mode_expr.Term.Release
       prelude.Stag.Mode_expr.Term.Static_if_const prelude.Stag.Pattern.Deref
       prelude.Stag.Toplevel.External prelude.Stag.Type.Kind.Alias
       prelude.Stag.Type.Kind.Record prelude.Stag.Type.Kind.Variant
       prelude.Stag.Type_decl.Param.Mode prelude.Stag.Type_expr.Arg.Mode
       prelude.Stag.Type_ref.Path.Prelude prelude.Stag.Type_ref.Path.Program))
     (others_same_as_to_value true))
    |}]
;;

let%expect_test "values" =
  let show source =
    print_endline
      (Treewalk.Value.to_string_hum
         (Syntax_value.Program.to_value (parse source)))
  in
  show {|let f (x : int) = `A x|};
  show {|type t['a] = A | B of 'a * t['a]|};
  [%expect
    {|
    Cons (Box.make (Stag.Toplevel.Let ("f", Cons (Box.make (("x", {`mode = None; `ty = Some (Stag.Type_expr.Con (Stag.Path.Name "int", Nil))}), Nil)), {`mode = None; `ty = None}, Stag.Expr.Poly_variant (Ptr.make ("A", Some (Stag.Expr.Var (Stag.Path.Name "x", {`mode = None; `ty = None}))))), Nil))
    Cons (Box.make (Stag.Toplevel.Type {`kind = Stag.Type_decl.Kind.Variant (Cons (Box.make (("A", Nil), Cons (Box.make (("B", Cons (Box.make (Stag.Type_expr.Var "a", Cons (Box.make (Stag.Type_expr.Con (Stag.Path.Name "t", Cons (Box.make (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var "a"), Nil))), Nil))))), Nil))))); `name = "t"; `params = Cons (Box.make (Stag.Type_decl.Param.Ty "a", Nil))}, Nil))
    |}]
;;

let%expect_test "nodes are located where given" =
  let loc = Loc.start_of_file (Loc.File.of_string "splice.stag") in
  (match
     Syntax_value.Expr.of_value
       (Syntax_value.Expr.to_value
          (match Parser.expr ~file:"t.stag" "f (x : int)" with
           | Ok expr -> expr
           | Error error -> raise_s [%sexp (error : Parser.Error.t)]))
       ~loc
   with
   | Ok expr -> print_s [%sexp (expr : Syntax.Expr.t)]
   | Error error -> print_error error);
  [%expect
    {|
    ((App ((Var f ()) @ splice.stag:1:0-0)
      ((Var x ((ty ((Con (int @ splice.stag:1:0-0) ()) @ splice.stag:1:0-0)))) @
       splice.stag:1:0-0))
     @ splice.stag:1:0-0)
    |}]
;;

let%expect_test "an update of fields by ref round trips" =
  let at txt = Located.create ~loc:Loc.none txt in
  let expr desc : Syntax.Expr.t = at desc in
  let update =
    expr
      (Record_update
         ( expr (Var (Path (Syntax.Path.name "v"), Syntax.Annot.none))
         , [ at (Syntax.Decl_path.Resolved "A.N.x"), expr (Lit (Int 1))
           ; at (Syntax.Decl_path.Path (Syntax.Path.name "y")), expr (Lit Unit)
           ] ))
  in
  let value = Syntax_value.Expr.to_value update in
  print_endline (Treewalk.Value.to_string_hum value);
  (match Syntax_value.Expr.of_value value ~loc:Loc.none with
   | Ok update' ->
     print_s [%sexp (update' : Syntax.Expr.t)];
     print_s [%message "" ~equal:(Syntax.Expr.equal update update' : bool)]
   | Error error -> print_error error);
  [%expect
    {|
    Stag.Expr.Record_update (Ptr.make (Stag.Expr.Var (Stag.Path.Name "v", {`mode = None; `ty = None}), Cons (Box.make ((Stag.Path.Ref <field A.N.x>, Stag.Expr.Lit (Stag.Lit.Int 1)), Cons (Box.make ((Stag.Path.Name "y", Stag.Expr.Lit Stag.Lit.Unit), Nil))))))
    (Record_update (Var v ()) ((<A.N.x> (Lit (Int 1))) (y (Lit Unit))))
    (equal true)
    |}]
;;

(* [file], checked and evaluated, and the value of each toplevel by canonical
   name. *)
let eval_values ?(file = "gen.stag") source =
  match
    Driver.check_files
      ~limits:Source_helpers.limits
      [ { path = file; contents = source } ]
  with
  | Error error -> raise_s [%sexp (error : Driver.Error.t list)]
  | Ok driver ->
    Driver.eval ~limits:Source_helpers.limits driver |> ok_exn;
    fun name ->
      let ident =
        List.find_map_exn (Driver.checked driver).program.sccs ~f:(fun scc ->
          List.find_map scc.members ~f:(fun (member : Typed.Toplevel.t) ->
            Option.some_if (String.equal member.ident.name name) member.ident))
      in
      Treewalk.find (Treewalk.of_interpreter (Driver.interpreter driver)) ~ident
      |> ok_exn
;;

let generator =
  {|
let arm name = (Stag.Pattern.construct name Nil, Stag.Expr.string name)
let constructor name = (name, Nil)

(* type t = <names>  let to_string x = match x with <name> -> "<name>" ... *)
let enum names =
  Cons (Box.make (
    ( Stag.Toplevel.Type
        {`name = "t"; `params = Nil; `kind = Stag.Type_decl.Kind.Variant (Stag.map constructor names)}
    , Cons (Box.make (
        ( Stag.Toplevel.let_
            "to_string"
            (Cons (Box.make ( ("x", Nil))))
            (Stag.Expr.Match (Ptr.make ( (Stag.Expr.var "x", Stag.map arm names))))
        , Nil ))) )))

let names = Cons (Box.make ( ("Red", Cons (Box.make ( ("Green", Cons (Box.make ( ("Blue", Nil)))))))))
let decls = enum names
let bad = enum (Cons (Box.make ( ("Red", Cons (Box.make ( ("light blue", Nil)))))))
let keyword = Cons (Box.make ( (Stag.Toplevel.let_ "match" Nil (Stag.Expr.var "x"), Nil)))
let bad_path = Cons (Box.make ( (Stag.Toplevel.let_ "x" Nil (Stag.Expr.var "M..y"), Nil)))

(* Shapes the parser does not make itself, eg. a constructor of one tuple. *)
let make e = Stag.Expr.apply (Stag.Expr.var "Ptr.make") (Cons (Box.make ( (e, Nil))))
let pair = Stag.Expr.Tuple (Cons (Box.make ( (Stag.Expr.int 1, Cons (Box.make ( (make (Stag.Expr.construct "D" (Cons (Box.make ( (Stag.Expr.int 2, Nil))))), Nil)))))))
let one_tuple = Stag.Expr.construct "C" (Cons (Box.make ( (pair, Nil))))
let var x = Stag.Pattern.Var x
let pair_pattern = Stag.Pattern.Tuple (Cons (Box.make ( (var "a", Cons (Box.make ( (var "b", Nil)))))))
let first =
  Stag.Expr.Match (Ptr.make (
    ( Stag.Expr.var "one_tuple"
    , Cons (Box.make ( ((Stag.Pattern.construct "C" (Cons (Box.make ( (pair_pattern, Nil)))), Stag.Expr.var "a"), Nil))) )))
let int = Stag.Type_expr.con "int" Nil
let shapes =
  Cons (Box.make (
    ( Stag.Toplevel.Type
        {`name = "t"; `params = Nil; `kind = Stag.Type_decl.Kind.Variant (Cons (Box.make ( (("C", Cons (Box.make ( (Stag.Type_expr.Tuple (Cons (Box.make ( (int, Cons (Box.make ( (Stag.Type_expr.con "ptr" (Cons (Box.make ( (Stag.Type_expr.con "t" Nil, Nil)))), Nil))))))), Nil)))), Cons (Box.make ( (("D", Cons (Box.make ( (int, Nil)))), Nil)))))))}
    , Cons (Box.make (
        ( Stag.Toplevel.let_ "one_tuple" Nil one_tuple
        , Cons (Box.make (
            ( Stag.Toplevel.let_ "first" Nil first
            , Nil ))) ))) )))
|}
;;

let%expect_test "declarations computed by a stag program" =
  let value = eval_values generator "Gen.decls" in
  let program = Syntax_value.Program.of_value value ~loc:Loc.none |> ok_exn in
  print_s [%sexp (program : Syntax.Program.t)];
  Round_trip.program program;
  print_string (Printer.program program);
  [%expect
    {|
    ((Type
      ((name t) (params ()) (kind (Variant ((Red ()) (Green ()) (Blue ()))))))
     (Let to_string ((x ())) ()
      (Match (Var x ())
       (((Construct Red ()) (Lit (String Red)))
        ((Construct Green ()) (Lit (String Green)))
        ((Construct Blue ()) (Lit (String Blue)))))))
    type t = Red | Green | Blue

    let to_string x =
      match x with
      | Red -> "Red"
      | Green -> "Green"
      | Blue -> "Blue"
    |}];
  (* The generated declarations check, and work, as written. *)
  Source_helpers.run_program
    ~prelude:false
    (program
     @ parse
         {|
let print_all = print_endline (to_string Red); print_endline (to_string Blue)
|}
    );
  [%expect
    {|
    type t = Red | Green | Blue
    val to_string : t -> string
    val print_all : unit
    -- evaluated at compile time
    -- evaluated
    Red
    Blue
    (to_string <fun>)
    (print_all "()")
    |}]
;;

(* Generated syntax is also checked to round trip through [Printer], as it may
   have shapes the parser does not make. *)
let print_program value =
  match Syntax_value.Program.of_value value ~loc:Loc.none with
  | Ok program ->
    print_s [%sexp (program : Syntax.Program.t)];
    Round_trip.program program
  | Error error -> print_error error
;;

let%expect_test "generated syntax the parser does not make" =
  let program =
    Syntax_value.Program.of_value
      (eval_values generator "Gen.shapes")
      ~loc:Loc.none
    |> ok_exn
  in
  print_s (Round_trip.strip_locs [%sexp (program : Syntax.Program.t)]);
  Round_trip.program program;
  print_string (Printer.program program);
  [%expect
    {|
    ((Type
      ((name t) (params ())
       (kind
        (Variant
         ((C ((Tuple ((Con int ()) (Con ptr ((Ty (Con t ()))))))))
          (D ((Con int ()))))))))
     (Let one_tuple () ()
      (Construct C
       ((Tuple
         ((Lit (Int 1)) (App (Var Ptr.make ()) (Construct D ((Lit (Int 2))))))))))
     (Let first () ()
      (Match (Var one_tuple ())
       (((Construct C ((Tuple ((Var a) (Var b))))) (Var a ()))))))
    type t = C of (int * ptr[t]) | D of int

    let one_tuple = C ((1, Ptr.make (D 2)))
    let first = match one_tuple with C ((a, b)) -> a
    |}];
  Source_helpers.run_program ~prelude:false program;
  [%expect
    {|
    type t = C of (int * ptr[t]) | D of int
    val one_tuple : t
    val first : int
    -- evaluated at compile time
    -- evaluated
    (one_tuple (C (Tuple 1 (Ptr (D 2)))))
    (first 1)
    |}]
;;

let%expect_test "reflected types print as paths" =
  let value =
    eval_values
      {|
module M = struct type t['a] = A of 'a end
module N = struct type option = Local end
let reflected = Stag.Type.expr (type (option[M.t[int]] * N.option * list[int]))
|}
      "Gen.reflected"
  in
  let ty = Syntax_value.Type_expr.of_value value ~loc:Loc.none |> ok_exn in
  print_s (Round_trip.strip_locs [%sexp (ty : Syntax.Type_expr.t)]);
  print_endline (Printer.type_expr ty);
  Round_trip.type_expr ty;
  [%expect
    {|
    (Tuple
     ((Con <option> ((Ty (Con <Gen.M.t> ((Ty (Con <int> ())))))))
      (Con <Gen.N.option> ()) (Con <list> ((Ty (Con <int> ()))))))
    .option[.Gen.M.t[.int]] * .Gen.N.option * .list[.int]
    |}]
;;

let%expect_test "malformed values" =
  let values = eval_values generator in
  List.iter [ "Gen.bad"; "Gen.keyword"; "Gen.bad_path" ] ~f:(fun name ->
    print_program (values name));
  [%expect
    {|
    Malformed syntax: expected a capitalized identifier, got "light blue", within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Variant > element 1 > component 0
    Malformed syntax: expected a lowercase identifier or _, got "match", within element 0 > Stag.Toplevel.Let argument 0
    Malformed syntax: expected a path, optionally from the root ([.M.x]), of capitalized identifiers ending in a lowercase identifier, got "M..y", within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 0 > Stag.Path.Name
    |}];
  (* A value of another type, eg. one with constructors of the same names. *)
  let values =
    eval_values
      ~file:"other.stag"
      {|
module Mine = struct type option['a] = None | Some of 'a end
let decls = Cons (Box.make ( (Stag.Toplevel.Let ("x", Nil, Stag.Annot.none, Stag.Expr.Lit (Stag.Lit.Int 1)), Nil)))
let other = Mine.Some 1
|}
  in
  let other = values "Other.other" in
  let rec replace_none (value : Treewalk.Value.t) : Treewalk.Value.t =
    match value with
    | Construct ({ name = "prelude.None"; _ }, []) -> other
    | Construct (ref, args) -> Construct (ref, List.map args ~f:replace_none)
    | value -> value
  in
  print_program (replace_none (values "Other.decls"));
  [%expect {| ((Let x () () (Lit (Int 1)))) |}];
  print_program (Int 1);
  [%expect {| Malformed syntax: expected list, got 1 |}]
;;

let print_expr value =
  match Syntax_value.Expr.of_value value ~loc:Loc.none with
  | Ok expr ->
    print_s [%sexp (expr : Syntax.Expr.t)];
    Round_trip.expr expr
  | Error error -> print_error error
;;

let%expect_test "malformed values made otherwise" =
  let expr source =
    match Parser.expr ~file:"t.stag" source with
    | Ok expr -> Syntax_value.Expr.to_value expr
    | Error error -> raise_s [%sexp (error : Parser.Error.t)]
  in
  (* [f] applied to the first constructor named [name] in [value]. *)
  let rec edit (value : Treewalk.Value.t) ~name ~f : Treewalk.Value.t =
    match value with
    | Construct (ref, args) when String.equal ref.name name -> f ref args
    | Construct (ref, args) -> Construct (ref, List.map args ~f:(edit ~name ~f))
    | Tuple values -> Tuple (List.map values ~f:(edit ~name ~f))
    | Poly_record fields -> Poly_record (Map.map fields ~f:(edit ~name ~f))
    | Ptr cell -> Ptr (ref (edit !cell ~name ~f))
    | Box value -> Box (edit value ~name ~f)
    | value -> value
  in
  (* The value a pointer-bearing constructor holds. *)
  let payload (args : Treewalk.Value.t list) ~default =
    match args with
    | [ Ptr cell ] -> !cell
    | [ Box value ] -> value
    | _ -> default
  in
  (* Wrong arity. *)
  print_expr
    (edit (expr "f (g x)") ~name:"prelude.Stag.Expr.App" ~f:(fun ref args ->
       match payload args ~default:(Tuple []) with
       | Tuple [ fn; _ ] ->
         Construct (ref, [ Ptr (Stdlib.ref (Treewalk.Value.Tuple [ fn ])) ])
       | _ -> assert false));
  [%expect
    {| Malformed syntax: expected an indirect constructor's tuple, got (Stag.Expr.Var (Stag.Path.Name "f", {`mode = None; `ty = None})) |}];
  (* A wrong payload. *)
  print_expr
    (edit (expr "(1, 2)") ~name:"prelude.Stag.Lit.Int" ~f:(fun ref _ ->
       Construct (ref, [ String "1" ])));
  [%expect
    {| Malformed syntax: expected an int, got "1", within Stag.Expr.Tuple > element 0 > Stag.Expr.Lit > Stag.Lit.Int |}];
  (* A missing record field. *)
  print_expr
    (edit
       (expr ".< .~e >.")
       ~name:"prelude.Stag.Expr.Splice"
       ~f:(fun ref args ->
         match payload args ~default:Unit with
         | Poly_record fields ->
           Construct
             ( ref
             , [ Ptr
                   (Stdlib.ref
                      (Treewalk.Value.Poly_record (Map.remove fields "subs")))
               ] )
         | _ -> assert false));
  [%expect
    {| Malformed syntax: expected a record with a field `subs, got {`expr = Stag.Expr.Var (Stag.Path.Name "e", {`mode = None; `ty = None})}, within Stag.Expr.Quote > Stag.Expr.Splice |}];
  (* Not a pair. *)
  print_expr
    (edit
       (expr "let f x (y : int) = x in f")
       ~name:"prelude.Cons"
       ~f:(fun ref args ->
         match payload args ~default:Unit with
         | Tuple [ name; rest ] ->
           Construct (ref, [ Box (Treewalk.Value.Tuple [ name; rest; rest ]) ])
         | _ -> assert false));
  [%expect
    {| Malformed syntax: expected a list cell's pair, got (("x", {`mode = None; `ty = None}), Cons (Box.make (("y", {`mode = None; `ty = Some (Stag.Type_expr.Con (Stag.Path.Name "int", Nil))}), Nil)), Cons (Box.make (("y", {`mode = None; `ty = Some (Stag.Type_expr.Con (Stag.Path.Name "int", Nil))}), Nil))), within Stag.Expr.Let argument 1 |}];
  (* A list ending in something else. *)
  print_expr (edit (expr "(a, b, c)") ~name:"prelude.Nil" ~f:(fun _ _ -> Unit));
  [%expect
    {| Malformed syntax: expected list, got (), within Stag.Expr.Tuple |}]
;;

let%expect_test "names" =
  let at txt = Located.create ~loc:Loc.none txt in
  let expr desc : Syntax.Expr.t = at desc in
  let ty desc : Syntax.Type_expr.t = at desc in
  let unit = expr (Lit Unit) in
  let of_expr f name =
    Syntax_value.Expr.of_value
      (Syntax_value.Expr.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  let of_type f name =
    Syntax_value.Type_expr.of_value
      (Syntax_value.Type_expr.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  let of_toplevel f name =
    Syntax_value.Toplevel.of_value
      (Syntax_value.Toplevel.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  (* Each position, by the syntax with the name there. *)
  let positions =
    [ ( "pattern"
      , of_expr (fun name ->
          expr (Match (unit, [ at (Syntax.Pattern.Var name), unit ]))) )
    ; ( "let"
      , of_expr (fun name ->
          expr (Let (at name, [], Syntax.Annot.none, unit, unit))) )
    ; ( "param"
      , of_expr (fun name ->
          expr
            (Let
               ( at "f"
               , [ at name, Syntax.Annot.none ]
               , Syntax.Annot.none
               , unit
               , unit ))) )
    ; ( "module"
      , of_toplevel (fun name -> at (Syntax.Toplevel.Module (at name, []))) )
    ; ( "type"
      , of_toplevel (fun name ->
          at
            (Syntax.Toplevel.Type
               { name = at name; params = []; kind = Abstract })) )
    ; "label", of_expr (fun name -> expr (Poly_variant (at name, None)))
    ; "tyvar", of_type (fun name -> ty (Var name))
    ; ( "context"
      , of_type (fun name ->
          ty
            (Code
               { inner = ty (Tuple [])
               ; context =
                   at
                     (Syntax.Type_expr.Extend
                        (at name, ty (Tuple []), at Syntax.Type_expr.Empty))
               })) )
    ; ( "subs"
      , of_expr (fun name ->
          expr (Splice { expr = unit; subs = Some [ at name, None ] })) )
    ; ( "var path"
      , of_expr (fun name ->
          expr
            (Var (Path (Syntax.Path.of_list_exn [ name ]), Syntax.Annot.none)))
      )
    ; ( "con path"
      , of_expr (fun name ->
          expr
            (Construct
               ( at (Syntax.Decl_path.Path (Syntax.Path.of_list_exn [ name ]))
               , [] ))) )
    ]
  in
  let texts =
    [ "x"
    ; "x'"
    ; "_x"
    ; "_"
    ; "match"
    ; "X"
    ; "X'"
    ; "M.x"
    ; "M.X"
    ; "M..x"
    ; "M."
    ; ".x"
    ; ""
    ; " x"
    ; "x "
    ; "x (* c *)"
    ; "x y"
    ; "'a"
    ; "`A"
    ; "_a"
    ; "a'"
    ]
  in
  let width =
    List.fold texts ~init:0 ~f:(fun width text ->
      max width (String.length (sprintf "%S" text)))
  in
  printf "%*s" width "";
  List.iter positions ~f:(fun (position, _) -> printf " %s" position);
  print_endline "";
  List.iter texts ~f:(fun text ->
    printf "%-*s" width (sprintf "%S" text);
    List.iter positions ~f:(fun (position, of_name) ->
      printf
        " %*s"
        (String.length position)
        (if Result.is_ok (of_name text) then "ok" else "-"));
    print_endline "");
  [%expect
    {|
                pattern let param module type label tyvar context subs var path con path
    "x"              ok  ok    ok      -   ok    ok    ok      ok   ok       ok        -
    "x'"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "_x"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "_"               -  ok    ok      -    -    ok     -       -    -        -        -
    "match"           -   -     -      -    -    ok    ok       -    -        -        -
    "X"               -   -     -     ok    -    ok    ok       -    -        -       ok
    "X'"              -   -     -     ok    -    ok     -       -    -        -       ok
    "M.x"             -   -     -      -    -     -     -       -    -       ok        -
    "M.X"             -   -     -      -    -     -     -       -    -        -       ok
    "M..x"            -   -     -      -    -     -     -       -    -        -        -
    "M."              -   -     -      -    -     -     -       -    -        -        -
    ".x"              -   -     -      -    -     -     -       -    -       ok        -
    ""                -   -     -      -    -     -     -       -    -        -        -
    " x"              -   -     -      -    -     -     -       -    -        -        -
    "x "              -   -     -      -    -     -     -       -    -        -        -
    "x (* c *)"       -   -     -      -    -     -     -       -    -        -        -
    "x y"             -   -     -      -    -     -     -       -    -        -        -
    "'a"              -   -     -      -    -     -     -       -    -        -        -
    "`A"              -   -     -      -    -     -     -       -    -        -        -
    "_a"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "a'"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    |}];
  (* Each says what it expected. *)
  List.iter positions ~f:(fun (position, of_name) ->
    match of_name "x y" with
    | Ok () -> printf "%s: ok\n" position
    | Error error -> printf "%s: %s\n" position (Error.to_string_hum error));
  [%expect
    {|
    pattern: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Expr.Match argument 1 > element 0 > component 0 > Stag.Pattern.Var
    let: Malformed syntax: expected a lowercase identifier or _, got "x y", within Stag.Expr.Let argument 0
    param: Malformed syntax: expected a lowercase identifier or _, got "x y", within Stag.Expr.Let argument 1 > element 0 > component 0
    module: Malformed syntax: expected a capitalized identifier, got "x y", within Stag.Toplevel.Module argument 0
    type: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Toplevel.Type > field `name
    label: Malformed syntax: expected a label, got "x y", within Stag.Expr.Poly_variant argument 0
    tyvar: Malformed syntax: expected a type variable name, got "x y", within Stag.Type_expr.Var
    context: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Type_expr.Code > field `context > Stag.Type_expr.Row.Extend argument 0
    subs: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Expr.Splice > field `subs > element 0 > component 0
    var path: Malformed syntax: expected a path, optionally from the root ([.M.x]), of capitalized identifiers ending in a lowercase identifier, got "x y", within Stag.Expr.Var argument 0 > Stag.Path.Name
    con path: Malformed syntax: expected a path, optionally from the root ([.M.x]), of capitalized identifiers ending in a capitalized identifier, got "x y", within Stag.Expr.Construct argument 0 > Stag.Path.Name
    |}]
;;

let%expect_test "shapes" =
  let at txt = Located.create ~loc:Loc.none txt in
  let unit : Syntax.Expr.t = at (Syntax.Expr.Lit Unit) in
  let int_ty : Syntax.Type_expr.t =
    at
      (Syntax.Type_expr.Con
         (at (Syntax.Decl_path.Path (Syntax.Path.name "int")), []))
  in
  let any : Syntax.Pattern.t = at Syntax.Pattern.Any in
  let show ~to_value ~of_value sexp_of x =
    match of_value (to_value x) ~loc:Loc.none with
    | Ok x -> print_s (sexp_of x)
    | Error error -> print_error error
  in
  let expr =
    show
      ~to_value:Syntax_value.Expr.to_value
      ~of_value:Syntax_value.Expr.of_value
      Syntax.Expr.sexp_of_t
  in
  let pattern p =
    show
      ~to_value:Syntax_value.Pattern.to_value
      ~of_value:Syntax_value.Pattern.of_value
      Syntax.Pattern.sexp_of_t
      p
  in
  let type_expr =
    show
      ~to_value:Syntax_value.Type_expr.to_value
      ~of_value:Syntax_value.Type_expr.of_value
      Syntax.Type_expr.sexp_of_t
  in
  let type_decl =
    show
      ~to_value:Syntax_value.Type_decl.to_value
      ~of_value:Syntax_value.Type_decl.of_value
      Syntax.Type_decl.sexp_of_t
  in
  List.iter
    [ []; [ unit ]; [ unit; unit ] ]
    ~f:(fun es -> expr (at (Syntax.Expr.Tuple es)));
  [%expect
    {|
    Malformed syntax: expected at least two components, got Nil, within Stag.Expr.Tuple
    Malformed syntax: expected at least two components, got Cons (Box.make (Stag.Expr.Lit Stag.Lit.Unit, Nil)), within Stag.Expr.Tuple
    (Tuple ((Lit Unit) (Lit Unit)))
    |}];
  List.iter
    [ []; [ any ]; [ any; any ] ]
    ~f:(fun ps -> pattern (at (Syntax.Pattern.Tuple ps)));
  [%expect
    {|
    Malformed syntax: expected at least two components, got Nil, within Stag.Pattern.Tuple
    Malformed syntax: expected at least two components, got Cons (Box.make (Stag.Pattern.Any, Nil)), within Stag.Pattern.Tuple
    (Tuple (Any Any))
    |}];
  List.iter
    [ []; [ int_ty ]; [ int_ty; int_ty ] ]
    ~f:(fun ts -> type_expr (at (Syntax.Type_expr.Tuple ts)));
  [%expect
    {|
    (Tuple ())
    Malformed syntax: expected no components or at least two, got Cons (Box.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Nil)), within Stag.Type_expr.Tuple
    (Tuple ((Con int ()) (Con int ())))
    |}];
  expr (at (Syntax.Expr.Record []));
  pattern (at (Syntax.Pattern.Record []));
  type_decl { name = at "t"; params = []; kind = Record [] };
  [%expect
    {|
    Malformed syntax: expected at least one field, got Nil, within Stag.Expr.Record
    Malformed syntax: expected at least one field, got Nil, within Stag.Pattern.Record
    Malformed syntax: expected at least one field, got Nil, within field `kind > Stag.Type_decl.Kind.Record
    |}]
;;

let%expect_test "modes and modalities only where the parser puts them" =
  let at txt = Located.create ~loc:Loc.none txt in
  let int : Syntax.Type_expr.t =
    at
      (Syntax.Type_expr.Con
         (at (Syntax.Decl_path.Path (Syntax.Path.name "int")), []))
  in
  let local = at (Syntax.Mode_expr.Atoms [ Local ]) in
  let moded ty = at (Syntax.Type_expr.Moded (ty, local)) in
  let global ty =
    at (Syntax.Type_expr.Modality (ty, at (Syntax.Mode_expr.Atoms [ Global ])))
  in
  let arrow a b = at (Syntax.Type_expr.Arrow (a, b)) in
  let show ~to_value ~of_value print x =
    match of_value (to_value x) ~loc:Loc.none with
    | Ok x -> print_endline (print x)
    | Error error -> print_error error
  in
  let type_expr =
    show
      ~to_value:Syntax_value.Type_expr.to_value
      ~of_value:Syntax_value.Type_expr.of_value
      (fun ty -> Printer.type_expr ty)
  in
  let program =
    show
      ~to_value:Syntax_value.Program.to_value
      ~of_value:Syntax_value.Program.of_value
      (fun program -> Printer.program program)
  in
  let var annot : Syntax.Program.t =
    [ at
        (Syntax.Toplevel.Let
           ( at "v"
           , []
           , Syntax.Annot.none
           , at
               (Syntax.Expr.Var
                  (Syntax.Decl_path.Path (Syntax.Path.name "x"), annot)) ))
    ]
  in
  let type_decl (kind : Syntax.Type_decl.Kind.t) : Syntax.Program.t =
    [ at (Syntax.Toplevel.Type { name = at "t"; params = []; kind }) ]
  in
  type_expr (arrow (moded int) (moded int));
  type_expr (arrow (arrow (moded int) int) (moded (arrow int int)));
  type_expr (at (Syntax.Type_expr.Tuple [ moded int; int ]));
  type_expr (arrow (global int) int);
  type_expr (moded int);
  type_expr (arrow (moded (moded int)) int);
  type_expr (global int);
  [%expect
    {|
    int @ local -> int @ local
    (int @ local -> int) -> (int -> int) @ local
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within Stag.Type_expr.Tuple > element 0
    Malformed syntax: expected a type without a modality: a modality is only on a type argument, a record field or a constructor's argument, as in [ptr[t @@ external]], got Stag.Type_expr.Modality (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Global, Nil))))), within Stag.Type_expr.Arrow argument 0
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil)))))
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within Stag.Type_expr.Arrow argument 0 > Stag.Type_expr.Moded argument 0
    Malformed syntax: expected a type without a modality: a modality is only on a type argument, a record field or a constructor's argument, as in [ptr[t @@ external]], got Stag.Type_expr.Modality (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Global, Nil)))))
    |}];
  program (var { ty = Some (moded int); mode = None });
  program (var { ty = Some (moded int); mode = Some local });
  program (var { ty = Some (arrow int (moded int)); mode = Some local });
  [%expect
    {|
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 1 > field `ty
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 1 > field `ty
    let v = (x : (int -> int @ local) @ local)
    |}];
  program (type_decl (Record [ at "x", global (arrow (moded int) int) ]));
  program (type_decl (Record [ at "x", global (global int) ]));
  program (type_decl (Record [ at "x", moded int ]));
  program (type_decl (Variant [ at "A", [ global int; int ] ]));
  program (type_decl (Variant [ at "A", [ moded int ] ]));
  program (type_decl (Alias (global int)));
  program (type_decl (Alias (moded int)));
  program [ at (Syntax.Toplevel.Exception (at "E", [ global int ])) ];
  [%expect
    {|
    type t = {x : (int @ local -> int) @@ global}

    Malformed syntax: expected a type without a modality: a modality is only on a type argument, a record field or a constructor's argument, as in [ptr[t @@ external]], got Stag.Type_expr.Modality (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Global, Nil))))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Record > element 0 > component 1 > Stag.Type_expr.Modality argument 0
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Record > element 0 > component 1
    type t = A of int @@ global * int

    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Variant > element 0 > component 1 > element 0
    Malformed syntax: expected a type without a modality: a modality is only on a type argument, a record field or a constructor's argument, as in [ptr[t @@ external]], got Stag.Type_expr.Modality (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Global, Nil))))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Alias
    Malformed syntax: expected a type without a mode: a mode is only on a function type's argument or result, got Stag.Type_expr.Moded (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Alias
    Malformed syntax: expected a type without a modality: a modality is only on a type argument, a record field or a constructor's argument, as in [ptr[t @@ external]], got Stag.Type_expr.Modality (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Global, Nil))))), within element 0 > Stag.Toplevel.Exception argument 1 > element 0
    |}];
  let m = at (Syntax.Mode_expr.Var "m") in
  let constrained ?(lower = m) ?(upper = local) ty =
    at
      (Syntax.Type_expr.Constrained
         (ty, [ { lower = Mode lower; upper = Mode upper } ]))
  in
  program (var { ty = Some (constrained int); mode = Some m });
  program (var { ty = Some (constrained ~lower:local int); mode = None });
  program (var { ty = Some (arrow int (constrained int)); mode = None });
  program
    (var
       { ty = Some (at (Syntax.Type_expr.Constrained (int, []))); mode = None });
  type_expr (constrained int);
  program (type_decl (Alias (constrained int)));
  [%expect
    {|
    let v = (x : int @ 'm with 'm <= local)

    Malformed syntax: expected a well-formed mode constraint (a mode constraint relates a mode variable, as in `'m <= 'n` or `'m <= portable`), got (Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil)))), Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 1 > field `ty > Stag.Type_expr.Constrained argument 1 > element 0
    Malformed syntax: expected a type without mode constraints: they are only on an annotation's type, got Stag.Type_expr.Constrained (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Cons (Box.make ((Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Var "m"), Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), Nil)))), within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 1 > field `ty > Stag.Type_expr.Arrow argument 1
    Malformed syntax: expected at least one mode constraint, got Stag.Type_expr.Constrained (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Nil)), within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 1 > field `ty
    Malformed syntax: expected a type without mode constraints: they are only on an annotation's type, got Stag.Type_expr.Constrained (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Cons (Box.make ((Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Var "m"), Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), Nil))))
    Malformed syntax: expected a type without mode constraints: they are only on an annotation's type, got Stag.Type_expr.Constrained (Ptr.make (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Cons (Box.make ((Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Var "m"), Stag.Mode_expr.Term.Mode (Stag.Mode_expr.Atoms (Cons (Box.make (Stag.Mode_expr.Atom.Local, Nil))))), Nil)))), within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Alias
    |}]
;;

(* Shapes accepted as meaningful, though the parser can't produce most. *)
let%expect_test "shapes left to the typechecker" =
  let values =
    eval_values
      {|
let unit = Stag.Type_expr.con "unit" Nil
let int = Stag.Type_expr.con "int" Nil
let decls =
  Cons (Box.make ( (Stag.Toplevel.Type {`name = "void"; `params = Nil; `kind = Stag.Type_decl.Kind.Variant Nil},
  Cons (Box.make ( (Stag.Toplevel.Type {`name = "pair"; `params = Nil;
     `kind = Stag.Type_decl.Kind.Variant (Cons (Box.make ( (("P", Cons (Box.make ( (Stag.Type_expr.Tuple (Cons (Box.make ( (int, Cons (Box.make ( (int, Nil))))))), Nil)))), Nil))))},
  Cons (Box.make ( (Stag.Toplevel.Let ("absurd", Cons (Box.make ( (("v", Stag.Annot.ty (Stag.Type_expr.con "void" Nil)), Nil))), Stag.Annot.ty int,
     Stag.Expr.Match (Ptr.make ( (Stag.Expr.var "v", Nil)))),
  Cons (Box.make ( (Stag.Toplevel.let_ "p" Nil
     (Stag.Expr.construct "P" (Cons (Box.make ( (Stag.Expr.Tuple (Cons (Box.make ( (Stag.Expr.int 1, Cons (Box.make ( (Stag.Expr.int 2, Nil))))))), Nil))))),
  Cons (Box.make ( (Stag.Toplevel.let_ "first" (Cons (Box.make ( ("p", Nil))))
     (Stag.Expr.Match (Ptr.make ( (Stag.Expr.var "p", Cons (Box.make ( ((Stag.Pattern.construct "P" (Cons (Box.make ( (Stag.Pattern.Var "q", Nil)))), Stag.Expr.var "q"), Nil))))))),
  Cons (Box.make ( (Stag.Toplevel.let_ "extended" Nil (Stag.Expr.Poly_extend (Ptr.make ( (Nil, Stag.Expr.Poly_record (Cons (Box.make ( (("x", Stag.Expr.int 1), Nil)))))))),
  Cons (Box.make ( (Stag.Toplevel.let_ "updated" Nil (Stag.Expr.Poly_update (Ptr.make ( (Stag.Expr.var "extended", Nil)))),
  Nil)))))))))))))))))))))
|}
  in
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect
    {|
    ((Type ((name void) (params ()) (kind (Variant ()))))
     (Type
      ((name pair) (params ())
       (kind (Variant ((P ((Tuple ((Con int ()) (Con int ()))))))))))
     (Let absurd ((v ((ty (Con void ()))))) ((ty (Con int ())))
      (Match (Var v ()) ()))
     (Let p () () (Construct P ((Tuple ((Lit (Int 1)) (Lit (Int 2)))))))
     (Let first ((p ())) ()
      (Match (Var p ()) (((Construct P ((Var q))) (Var q ())))))
     (Let extended () () (Poly_extend () (Poly_record ((x (Lit (Int 1)))))))
     (Let updated () () (Poly_update (Var extended ()) ())))
    |}];
  Source_helpers.run_program
    ~prelude:false
    (program
     @ parse
         {|let both = match first p with (a, b) -> print_int (Int.add a b); print_endline ""|}
    );
  [%expect
    {|
    type void = |
    type pair = P of (int * int)
    val absurd : void -> int
    val p : pair
    val first : pair -> int * int
    val extended : {`x : int}
    val updated : {`x : int}
    val both : unit
    -- evaluated at compile time
    -- evaluated
    3
    (absurd <fun>)
    (p (P (Tuple 1 2)))
    (first <fun>)
    (extended ((x 1)))
    (updated ((x 1)))
    (both "()")
    |}];
  (* A match of no arms is only of an empty type. *)
  Source_helpers.run_program
    ~prelude:false
    (program
     @ [ Located.create
           ~loc:Loc.none
           (Syntax.Toplevel.Let
              ( Located.create ~loc:Loc.none "bad"
              , []
              , Syntax.Annot.none
              , Located.create
                  ~loc:Loc.none
                  (Syntax.Expr.Match
                     ( Located.create
                         ~loc:Loc.none
                         (Syntax.Expr.Var
                            (Path (Syntax.Path.name "p"), Syntax.Annot.none))
                     , [] )) ))
       ]);
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon pair ()) Structural_mismatch))
    |}]
;;

let%expect_test "negative int literals" =
  let values =
    eval_values
      {|
let decls = Cons (Box.make ( (Stag.Toplevel.let_ "minus_one" Nil (Stag.Expr.int (Int.neg 1)), Nil)))
|}
  in
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect {| ((Let minus_one () () (Lit (Int -1)))) |}];
  Source_helpers.run_program
    ~prelude:false
    (program @ parse {|let printed = print_int minus_one; print_endline ""|});
  [%expect
    {|
    val minus_one : int
    val printed : unit
    -- evaluated at compile time
    -- evaluated
    -1
    (minus_one -1)
    (printed "()")
    |}]
;;

let%expect_test "prelude helpers" =
  let values =
    eval_values
      {|
let int = Stag.Type_expr.con "int" Nil
let types =
  Cons (Box.make ( (Stag.Type_expr.tuple Nil,
  Cons (Box.make ( (Stag.Type_expr.tuple (Cons (Box.make ( (int, Nil)))),
  Cons (Box.make ( (Stag.Type_expr.tuple (Cons (Box.make ( (int, Cons (Box.make ( (int, Nil))))))),
  Cons (Box.make ( (Stag.Type_expr.con "list" (Cons (Box.make ( (int, Nil)))), Nil))))))))))))
let patterns =
  Cons (Box.make ( (Stag.Pattern.tuple Nil,
  Cons (Box.make ( (Stag.Pattern.tuple (Cons (Box.make ( (Stag.Pattern.Any, Nil)))),
  Cons (Box.make ( (Stag.Pattern.tuple (Cons (Box.make ( (Stag.Pattern.Var "a", Cons (Box.make ( (Stag.Pattern.Var "b", Nil))))))), Nil)))))))))
let one = Stag.Expr.int 1
let exprs =
  Cons (Box.make ( (Stag.Expr.tuple Nil,
  Cons (Box.make ( (Stag.Expr.tuple (Cons (Box.make ( (one, Nil)))),
  Cons (Box.make ( (Stag.Expr.tuple (Cons (Box.make ( (one, Cons (Box.make ( (Stag.Expr.string "s", Nil))))))),
  Cons (Box.make ( (Stag.Expr.apply (Stag.Expr.var "f") Nil,
  Cons (Box.make ( (Stag.Expr.apply (Stag.Expr.var "f") (Cons (Box.make ( (one, Cons (Box.make ( (Stag.Expr.var "x", Nil))))))), Nil)))))))))))))))
let decls =
  Cons (Box.make ( (Stag.Toplevel.let_ "add" (Cons (Box.make ( ("x", Cons (Box.make ( ("y", Nil)))))))
    (Stag.Expr.apply (Stag.Expr.var "Int.add") (Cons (Box.make ( (Stag.Expr.var "x", Cons (Box.make ( (Stag.Expr.var "y", Nil)))))))),
  Cons (Box.make ( (Stag.Toplevel.let_ "seven" Nil
    (Stag.Expr.let_ "double" (Cons (Box.make ( ("n", Nil))))
       (Stag.Expr.apply (Stag.Expr.var "add") (Cons (Box.make ( (Stag.Expr.var "n", Cons (Box.make ( (Stag.Expr.var "n", Nil))))))))
       (Stag.Expr.Match (Ptr.make ( (Stag.Expr.tuple (Cons (Box.make ( (Stag.Expr.int 3, Cons (Box.make ( (one, Nil))))))),
          Cons (Box.make ( ((Stag.Pattern.tuple (Cons (Box.make ( (Stag.Pattern.Var "a", Cons (Box.make ( (Stag.Pattern.Var "b", Nil))))))),
                      Stag.Expr.apply (Stag.Expr.var "add") (Cons (Box.make ( (Stag.Expr.App (Ptr.make ( (Stag.Expr.var "double", Stag.Expr.var "a"))), Cons (Box.make ( (Stag.Expr.var "b", Nil)))))))),
                     Nil)))))))),
  Nil))))))
|}
  in
  let each of_value sexp_of name =
    let rec elements (value : Treewalk.Value.t) =
      match value with
      | Construct ({ name = "prelude.Cons"; _ }, [ Box cell ]) ->
        (match cell with
         | Tuple [ x; rest ] -> x :: elements rest
         | _ -> [])
      | _ -> []
    in
    List.iter
      (elements (values name))
      ~f:(fun value ->
        match of_value value ~loc:Loc.none with
        | Ok x -> print_s (sexp_of x)
        | Error error -> print_error error)
  in
  each Syntax_value.Type_expr.of_value Syntax.Type_expr.sexp_of_t "Gen.types";
  [%expect
    {|
    (Con unit ())
    (Con int ())
    (Tuple ((Con int ()) (Con int ())))
    (Con list ((Ty (Con int ()))))
    |}];
  each Syntax_value.Pattern.of_value Syntax.Pattern.sexp_of_t "Gen.patterns";
  [%expect {|
    (Lit Unit)
    Any
    (Tuple ((Var a) (Var b)))
    |}];
  each Syntax_value.Expr.of_value Syntax.Expr.sexp_of_t "Gen.exprs";
  [%expect
    {|
    (Lit Unit)
    (Lit (Int 1))
    (Tuple ((Lit (Int 1)) (Lit (String s))))
    (Var f ())
    (App (App (Var f ()) (Lit (Int 1))) (Var x ()))
    |}];
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect
    {|
    ((Let add ((x ()) (y ())) ()
      (App (App (Var Int.add ()) (Var x ())) (Var y ())))
     (Let seven () ()
      (Let double ((n ())) () (App (App (Var add ()) (Var n ())) (Var n ()))
       (Match (Tuple ((Lit (Int 3)) (Lit (Int 1))))
        (((Tuple ((Var a) (Var b)))
          (App (App (Var add ()) (App (Var double ()) (Var a ()))) (Var b ()))))))))
    |}];
  Source_helpers.run_program
    ~prelude:false
    (program @ parse {|let printed = print_int seven; print_endline ""|});
  [%expect
    {|
    val add : int -> int -> int
    val seven : int
    val printed : unit
    -- evaluated at compile time
    -- evaluated
    7
    (add <fun>)
    (seven 7)
    (printed "()")
    |}]
;;

let%expect_test "mode syntax round trips through values" =
  let decls =
    eval_values
      {|
let decls = .< struct
  type r = { x : int @@ global; y : int }
  type t = A of int @@ global * int
  let f (x @ local) (y : int @ 'm) : int @ global portable = exclave_ stack_ (x, y)
  let g (h : int @ local -> int @ 'm) = h 1 [@nontail]
  let annotated = (f : int @ local -> int -> int * int)
  let var = (g @ local)
  let app = (g 1 : int @ local)
  let mut counter : int @ static = 0
  let h (x : int @ 'm with 'm <= portable) : int @ 'n with 'm <= 'n and 'n <= nonportable = x
end >.
|}
      "Gen.decls"
  in
  print_program decls;
  (match Syntax_value.Program.of_value decls ~loc:Loc.none with
   | Ok program -> print_string (Printer.program program)
   | Error error -> print_error error);
  [%expect
    {|
    ((Type
      ((name r) (params ())
       (kind
        (Record ((x (Modality (Con int ()) (Atoms (Global)))) (y (Con int ())))))))
     (Type
      ((name t) (params ())
       (kind
        (Variant ((A ((Modality (Con int ()) (Atoms (Global))) (Con int ()))))))))
     (Let f ((x ((mode (Atoms (Local))))) (y ((ty (Con int ())) (mode (Var m)))))
      ((ty (Con int ())) (mode (Atoms (Global Portable))))
      (Exclave (Stack (Tuple ((Var x ()) (Var y ()))))))
     (Let g
      ((h
        ((ty
          (Arrow (Moded (Con int ()) (Atoms (Local)))
           (Moded (Con int ()) (Var m)))))))
      () (Nontail (App (Var h ()) (Lit (Int 1)))))
     (Let annotated () ()
      (Var f
       ((ty
         (Arrow (Moded (Con int ()) (Atoms (Local)))
          (Arrow (Con int ()) (Tuple ((Con int ()) (Con int ())))))))))
     (Let var () () (Var g ((mode (Atoms (Local))))))
     (Let app () ()
      (Annotated (App (Var g ()) (Lit (Int 1)))
       ((ty (Con int ())) (mode (Atoms (Local))))))
     (Let_mut counter ((ty (Con int ())) (mode (Atoms (Static)))) (Lit (Int 0)))
     (Let h
      ((x
        ((ty
          (Constrained (Con int ())
           (((lower (Mode (Var m))) (upper (Mode (Atoms (Portable))))))))
         (mode (Var m)))))
      ((ty
        (Constrained (Con int ())
         (((lower (Mode (Var m))) (upper (Mode (Var n))))
          ((lower (Mode (Var n))) (upper (Mode (Atoms (Nonportable))))))))
       (mode (Var n)))
      (Var x ())))
    type r = {x : int @@ global; y : int}
    type t = A of int @@ global * int

    let f (x @ local) (y : int @ 'm) : int @ global portable =
      exclave_ stack_ (x, y)

    let g (h : int @ local -> int @ 'm) = h 1 [@nontail]
    let annotated = (f : int @ local -> int -> int * int)
    let var = (g @ local)
    let app = (g 1 : int @ local)
    let mut counter : int @ static = 0

    let h
        (x : int @ 'm with 'm <= portable) : int @ 'n
          with 'm <= 'n and 'n <= nonportable =
      x
    |}]
;;
