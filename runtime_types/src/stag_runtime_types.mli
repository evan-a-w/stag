open! Import

(** Shapes of concrete types, for laying out their values in runtime memory.

    A shape describes a value, not the blocks it points to: strings, [exn],
    functions and meta values (code, types and refs) are all [Shape.Ptr], as is
    [ptr] whatever it points to.

    Laying out a value's type needs the type to be concrete: a type variable no
    declaration binds, an open row, or a declaration the resolver does not have
    is an [Error], never a guess. So is a type that reaches itself without a
    [ptr], which has no finite layout. *)

module Error : module type of Shape_error

(** Where declarations come from. *)
module Resolver : module type of Resolver

(** The canonical name [ptr] is recognised by, unless [create] says otherwise.

    [ptr] is checked before the builtins, so [~ptr_name:"int"] makes [int]
    [ptr]. *)
val ptr_name : string

(** Keeps the shapes of the declarations it has expanded, for its later calls. *)
type t

val create : ?ptr_name:string -> Resolver.t -> t

(** [shape].
    - [int] is [I64], [char] and [bool] [I8] ([bool] 0 or 1 only), [unit] empty,
      [string] and [exn] [Ptr].
    - A tuple is a [Struct] of its components, in order.
    - A nominal record or variant is a [Struct] or [Union] of its fields or
      constructors, in declaration order.
    - A structural record or variant ([TRecord], [TVariant]) has its present
      entries sorted by label, which is the order the maps holding them iterate.
    - [TArrow] and [TCode] are [Ptr]: a closure or a meta value.
    - An alias is expanded, and a declaration applied to arguments is expanded
      once per instantiation however it was reached: [type u['a] = A of 'a] with
      [type r['a] = u['a]] has a finite layout at [r[r[int]]].
    - A declaration whose expansion does not terminate, as under polymorphic
      recursion, is [Error.Too_deep]. One whose shape grows exponentially with
      the depth of its expansions, as under nested polymorphic recursion
      ([type t['a] = L of 'a | N of t[('a * 'a)]]), or whose shape is too large
      as a tree however much of it is shared, is [Error.Too_large], whatever
      earlier calls expanded. *)
val shape : t -> Type.ty -> (Shape.t, Error.t) Result.t

(** [shape], laid out by [Layout.of_shape]. *)
val layout : t -> Type.ty -> (Layout.t, Error.t) Result.t
