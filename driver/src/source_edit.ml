open! Import

type t =
  { start : int
  ; stop : int
  ; replacement : string
  }
[@@deriving sexp_of]

let crlf contents =
  match String.index contents '\n' with
  | Some i -> i > 0 && Char.equal contents.[i - 1] '\r'
  | None -> false
;;

let apply contents edits =
  let edits =
    List.stable_sort edits ~compare:(fun a b -> Int.compare a.start b.start)
  in
  let newline = if crlf contents then "\r\n" else "\n" in
  let buffer = Buffer.create (String.length contents) in
  let last =
    List.fold
      edits
      ~init:0
      ~f:(fun pos ({ start; stop; replacement } as edit) ->
        if start < pos || stop < start
        then raise_s [%message "Source_edit.apply: overlapping edit" (edit : t)];
        Buffer.add_substring buffer contents ~pos ~len:(start - pos);
        Buffer.add_string
          buffer
          (String.substr_replace_all replacement ~pattern:"\n" ~with_:newline);
        stop)
  in
  Buffer.add_substring
    buffer
    contents
    ~pos:last
    ~len:(String.length contents - last);
  Buffer.contents buffer
;;

let leading_whitespace line =
  String.prefix
    line
    (String.lfindi line ~f:(fun _ c -> not (Char.is_whitespace c))
     |> Option.value ~default:(String.length line))
;;

let before_on_line contents ~offset =
  let start =
    match String.rindex_from contents (offset - 1) '\n' with
    | Some i -> i + 1
    | None | (exception _) -> 0
  in
  String.sub contents ~pos:start ~len:(offset - start)
;;

let indentation contents ~offset =
  leading_whitespace (before_on_line contents ~offset)
;;

let first_on_line contents ~offset =
  String.for_all (before_on_line contents ~offset) ~f:Char.is_whitespace
;;
