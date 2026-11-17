(** Int comparisons and bit operations as compiler primitives, which the
    [zero_alloc] check sees through without cross-module information (unlike
    [Core]'s). *)

external ( = ) : int -> int -> bool = "%equal"
external ( <> ) : int -> int -> bool = "%notequal"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( land ) : int -> int -> int = "%andint"
