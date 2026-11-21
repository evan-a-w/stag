open! Import

module Kind = struct
  type t =
    | Scanned
    | Bytes
    | Free
  [@@deriving sexp_of, equal, enumerate]

  let[@inline] [@zero_alloc] to_int = function
    | Scanned -> 0
    | Bytes -> 1
    | Free -> 2
  ;;
end

module Color = struct
  type t =
    | Unmarked
    | Marked
    | Immortal
  [@@deriving sexp_of, equal, enumerate]

  let[@inline] [@zero_alloc] to_int = function
    | Unmarked -> 0
    | Marked -> 1
    | Immortal -> 2
  ;;
end

type t = int64_u

let max_size_words = (1 lsl 32) - 1
let max_scalar_words = (1 lsl 24) - 1
let max_length = (1 lsl 56) - 1

(* Not [raise_s], which the zero_alloc checker can't see raises. *)
let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let[@inline] [@zero_alloc] to_int64 t = t
let[@inline] [@zero_alloc] of_int64 t = t
let[@inline] [@zero_alloc] equal a b = Int64_u.equal a b

let[@inline] [@zero_alloc] low_byte ~kind color =
  (Kind.to_int kind lsl 2) lor Color.to_int color
;;

let[@inline] [@zero_alloc] field t ~pos ~bits =
  Int64_u.to_int
    (Int64_u.logand
       (Int64_u.shift_right_logical t pos)
       (Int64_u.of_int ((1 lsl bits) - 1)))
;;

let[@inline] [@zero_alloc] make ~kind color ~upper =
  Int64_u.logor
    (Int64_u.shift_left (Int64_u.of_int upper) 8)
    (Int64_u.of_int (low_byte ~kind color))
;;

let[@inline] [@zero_alloc] make_sized ~kind color ~size_words ~low =
  Int64_u.logor
    (Int64_u.shift_left (Int64_u.of_int size_words) 32)
    (Int64_u.logor
       (Int64_u.shift_left (Int64_u.of_int low) 8)
       (Int64_u.of_int (low_byte ~kind color)))
;;

let[@inline] [@zero_alloc] scanned ~size_words ~scalar_words color =
  if not
       Int.(
         0 <= scalar_words
         && scalar_words <= size_words
         && size_words <= max_size_words
         && scalar_words <= max_scalar_words)
  then fail [%message "Header.scanned" (size_words : int) (scalar_words : int)];
  make_sized ~kind:Scanned color ~size_words ~low:scalar_words
;;

let[@inline] [@zero_alloc] bytes ~length color =
  if not Int.(0 <= length && length <= max_length)
  then fail [%message "Header.bytes" (length : int)];
  make ~kind:Bytes color ~upper:length
;;

let[@inline] [@zero_alloc] free ~size_words =
  if not Int.(0 <= size_words && size_words <= max_size_words)
  then fail [%message "Header.free" (size_words : int)];
  make_sized ~kind:Free Unmarked ~size_words ~low:0
;;

let[@inline] [@zero_alloc] kind t : Kind.t =
  match field t ~pos:2 ~bits:6 with
  | 0 -> Scanned
  | 1 -> Bytes
  | 2 -> Free
  | kind -> fail [%message "Header: unknown kind" (kind : int)]
;;

let[@inline] [@zero_alloc] color t : Color.t =
  match field t ~pos:0 ~bits:2 with
  | 0 -> Unmarked
  | 1 -> Marked
  | 2 -> Immortal
  | color -> fail [%message "Header: unknown colour" (color : int)]
;;

let[@inline] [@zero_alloc] with_color t color =
  Int64_u.logor
    (Int64_u.logand t (Int64_u.lognot #3L))
    (Int64_u.of_int (Color.to_int color))
;;

let[@inline] [@zero_alloc] raw_length t = field t ~pos:8 ~bits:56

let[@inline] [@zero_alloc] size_words t =
  match kind t with
  | Scanned | Free -> field t ~pos:32 ~bits:32
  | Bytes -> (raw_length t + 7) / 8
;;

let[@inline] [@zero_alloc] scalar_words t =
  match kind t with
  | Scanned -> field t ~pos:8 ~bits:24
  | Bytes | Free -> size_words t
;;

let[@inline] [@zero_alloc] length t =
  match kind t with
  | Bytes -> raw_length t
  | (Scanned | Free) as kind ->
    fail [%message "Header.length: not Bytes" (kind : Kind.t)]
;;

let sexp_of_t t =
  let color = color t in
  match kind t with
  | Scanned ->
    [%message
      "Scanned"
        ~size_words:(size_words t : int)
        ~scalar_words:(scalar_words t : int)
        (color : Color.t)]
  | Bytes -> [%message "Bytes" ~length:(raw_length t : int) (color : Color.t)]
  | Free -> [%message "Free" ~size_words:(size_words t : int) (color : Color.t)]
;;

let[@inline] [@zero_alloc] load addr =
  Memory.load_64 (Memory.Addr.add addr (-8))
;;

let[@inline] [@zero_alloc] store addr t =
  Memory.store_64 (Memory.Addr.add addr (-8)) t
;;
