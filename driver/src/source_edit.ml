open! Import

type t =
  { start : int
  ; stop : int
  ; replacement : string
  }
[@@deriving sexp_of]

let apply contents edits =
  let edits =
    List.stable_sort edits ~compare:(fun a b -> Int.compare a.start b.start)
  in
  let buffer = Buffer.create (String.length contents) in
  let last =
    List.fold edits ~init:0 ~f:(fun pos { start; stop; replacement } ->
      Buffer.add_substring buffer contents ~pos ~len:(start - pos);
      Buffer.add_string buffer replacement;
      stop)
  in
  Buffer.add_substring
    buffer
    contents
    ~pos:last
    ~len:(String.length contents - last);
  Buffer.contents buffer
;;
