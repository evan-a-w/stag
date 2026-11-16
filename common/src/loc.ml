open! Import

module File = struct
  type t = int

  let ids = String.Table.create ()
  let names = Queue.create ()

  let of_string name =
    Hashtbl.find_or_add ids name ~default:(fun () ->
      Queue.enqueue names name;
      Queue.length names - 1)
  ;;

  let to_string t = Queue.get names t
  let equal = Int.equal

  let compare a b =
    if a = b then 0 else String.compare (to_string a) (to_string b)
  ;;

  let hash = Int.hash
  let hash_fold_t = Int.hash_fold_t
  let sexp_of_t t = Sexp.Atom (to_string t)
end

module Pos = struct
  type t =
    #{ line : int
     ; col : int
     ; offset : int
     }
  [@@deriving sexp_of, compare, equal, hash]

  let start () = #{ line = 1; col = 0; offset = 0 }
end

type t =
  { file : File.t
  ; start : Pos.t
  ; stop : Pos.t
  }
[@@deriving compare, equal, hash]

let create ~file ~start ~stop = { file = File.of_string file; start; stop }
let file_name t = File.to_string t.file
let sort_by l ~f = List.stable_sort l ~compare:(Comparable.lift compare ~f)
let start_of_file file = { file; start = Pos.start (); stop = Pos.start () }
let none = start_of_file (File.of_string "_none_")
let is_none t = equal t none

let merge a b =
  if is_none a
  then b
  else if is_none b
  then a
  else (
    if not (File.equal a.file b.file)
    then
      raise_s
        [%message
          "Loc.merge: different files" (a.file : File.t) (b.file : File.t)];
    { file = a.file
    ; start = (if Pos.compare a.start b.start <= 0 then a.start else b.start)
    ; stop = (if Pos.compare a.stop b.stop >= 0 then a.stop else b.stop)
    })
;;

let to_string t =
  let file = file_name t in
  if is_none t
  then file
  else if t.start.#line = t.stop.#line
  then sprintf "%s:%d:%d-%d" file t.start.#line t.start.#col t.stop.#col
  else
    sprintf
      "%s:%d:%d-%d:%d"
      file
      t.start.#line
      t.start.#col
      t.stop.#line
      t.stop.#col
;;

let sexp_of_t t = Sexp.Atom (to_string t)

include functor Hashable.Make_plain
