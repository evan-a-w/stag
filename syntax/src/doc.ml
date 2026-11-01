open! Import

type t =
  | Empty
  | Text of string
  | Block of
      { text : string
      ; col : int
      }
  | Line of string
  | Hardline
  | Fresh_line
  | End_line
  | Blank_line
  | Leading of t
  | Space
  | If_break of string
  | Cat of t * t
  | Nest of int * t
  | Align of t
  | Group of t
  | Alt of t * t

let empty = Empty
let text s = Text s

let block text ~col =
  if String.mem text '\n' then Block { text; col } else Text text
;;

let ( ^^ ) a b =
  match a, b with
  | Empty, t | t, Empty -> t
  | a, b -> Cat (a, b)
;;

let concat ts = List.fold ts ~init:Empty ~f:( ^^ )

let separate sep = function
  | [] -> Empty
  | first :: rest ->
    List.fold rest ~init:first ~f:(fun acc t -> acc ^^ sep ^^ t)
;;

let line = Line " "
let cut = Line ""
let hardline = Hardline
let fresh_line = Fresh_line
let end_line = End_line
let blank_line = Blank_line
let leading t = Leading t
let space = Space
let if_break s = If_break s
let nest n t = Nest (n, t)
let align t = Align t
let group t = Group t
let alt a b = Alt (a, b)

module Leaf = struct
  type t =
    | Text of string
    | If_break of string
end

let rec fold_map_leaves t ~init ~f =
  match t with
  | Empty
  | Block _
  | Line _
  | Hardline
  | Fresh_line
  | End_line
  | Blank_line
  | Space -> init, t
  | Leading t ->
    let acc, t = fold_map_leaves t ~init ~f in
    acc, Leading t
  | Text s -> f init (Leaf.Text s)
  | If_break s -> f init (Leaf.If_break s)
  | Cat (a, b) ->
    let acc, a = fold_map_leaves a ~init ~f in
    let acc, b = fold_map_leaves b ~init:acc ~f in
    acc, Cat (a, b)
  | Nest (n, t) ->
    let acc, t = fold_map_leaves t ~init ~f in
    acc, Nest (n, t)
  | Align t ->
    let acc, t = fold_map_leaves t ~init ~f in
    acc, Align t
  | Group t ->
    let acc, t = fold_map_leaves t ~init ~f in
    acc, Group t
  | Alt (a, b) ->
    let acc, a = fold_map_leaves a ~init ~f in
    let _, b = fold_map_leaves b ~init ~f in
    acc, Alt (a, b)
;;

module Mode = struct
  type t =
    | Flat
    | Break
  [@@deriving equal]
end

(* [text], written at column [from], with its later lines moved alike as its
   first is to [col], as far as they can go. Not if one is indented with a
   tab, whose width is not known. *)
let shift_block text ~from ~col =
  let blank line = String.is_empty (String.strip line) in
  let leading line =
    String.length line
    - String.length (String.lstrip line ~drop:(Char.equal ' '))
  in
  match String.split text ~on:'\n' with
  | [] -> text
  | _ :: rest
    when List.exists rest ~f:(fun line ->
           (not (blank line))
           && Char.is_whitespace (String.get line (leading line))) -> text
  | first :: rest ->
    let shift =
      List.filter rest ~f:(Fn.non blank)
      |> List.map ~f:leading
      |> List.min_elt ~compare:Int.compare
      |> Option.value_map ~default:0 ~f:(fun min -> Int.max (col - from) (-min))
    in
    String.concat
      ~sep:"\n"
      (first
       :: List.map rest ~f:(fun line ->
         if blank line
         then line
         else
           String.make (leading line + shift) ' '
           ^ String.lstrip line ~drop:(Char.equal ' ')))
;;

(* After [s], whether a [Space] prints nothing, given whether it did before. *)
let spaced_after s ~spaced =
  if String.is_empty s
  then spaced
  else Char.equal (String.get s (String.length s - 1)) ' '
;;

(* Whether the commands fit in [width], up to the first newline (of a broken
   [line] or a text). After an [End_line], only if a newline comes before any
   more of the flat text. [spaced]: whether a [Space] would print nothing, as
   at the start of a line or after a space. *)
let rec fits ?(ended = false) ~spaced width (commands : (int * Mode.t * t) list)
  =
  width >= 0
  &&
  match commands with
  | [] -> true
  | (indent, mode, t) :: rest ->
    (match t with
     | Empty -> fits ~ended ~spaced width rest
     | Block { text; _ } ->
       fits ~ended ~spaced width ((indent, mode, Text text) :: rest)
     | Text s when ended ->
       (String.is_empty (String.strip s) && fits ~ended ~spaced width rest)
       || Mode.equal mode Break
     | Text s ->
       (match String.index s '\n' with
        | None ->
          fits ~spaced:(spaced_after s ~spaced) (width - String.length s) rest
        | Some i -> Mode.equal mode Break && i <= width)
     | Line s ->
       (match mode with
        | Flat ->
          (* A flat line after an ended one would be broken anyway: the group
             is broken instead, to lay it out as when the comment ending the
             line is already there. *)
          (not ended)
          && fits
               ~ended
               ~spaced:(spaced_after s ~spaced)
               (width - String.length s)
               rest
        | Break -> true)
     | Space ->
       if spaced || ended
       then fits ~ended ~spaced width rest
       else fits ~spaced:true (width - 1) rest
     | Hardline | Fresh_line | Blank_line -> Mode.equal mode Break
     | End_line -> fits ~ended:true ~spaced width rest
     | If_break s ->
       (match mode with
        | Flat -> fits ~ended ~spaced width rest
        | Break ->
          ended
          || fits
               ~spaced:(spaced_after s ~spaced)
               (width - String.length s)
               rest)
     | Cat (a, b) ->
       fits ~ended ~spaced width ((indent, mode, a) :: (indent, mode, b) :: rest)
     | Nest (_, t) | Align t | Group t | Leading t ->
       fits ~ended ~spaced width ((indent, mode, t) :: rest)
     | Alt (a, b) ->
       let t =
         match mode with
         | Flat -> a
         | Break -> b
       in
       fits ~ended ~spaced width ((indent, mode, t) :: rest))
;;

(* [Leading]s at the start of a group, before it instead. *)
let rec hoist t =
  match t with
  | Empty
  | Text _
  | Block _
  | Line _
  | Hardline
  | Fresh_line
  | End_line
  | Blank_line
  | Space
  | If_break _ -> t
  | Leading t -> Leading (hoist t)
  | Cat (a, b) -> Cat (hoist a, hoist b)
  | Nest (n, t) -> Nest (n, hoist t)
  | Align t -> Align (hoist t)
  | Alt (a, b) -> Alt (hoist a, hoist b)
  | Group t ->
    let leading, t = split_leading (hoist t) in
    leading ^^ Group t

and split_leading t =
  match t with
  | Leading _ -> t, Empty
  | Cat (a, b) ->
    (match split_leading a with
     | leading, Empty ->
       let leading', b = split_leading b in
       leading ^^ leading', b
     | leading, a -> leading, a ^^ b)
  | _ -> Empty, t
;;

let fits_flat ~width t = fits ~spaced:true width [ 0, Flat, hoist t ]

(* Whether [t], starting at [col] and laid out with its groups flat (its other
   lines broken), fits in [width] on each of its lines: the column it ends at
   if so, and whether a [Space] there would print nothing. Only the lines of [t] are looked at, each up to the width. *)
let rec is_blank = function
  | Empty -> true
  | Cat (a, b) -> is_blank a && is_blank b
  | Nest (_, t) | Align t | Group t -> is_blank t
  | _ -> false
;;

let fits_all ~width ~col ~spaced ~indent t =
  let rec go col ~spaced (commands : (int * Mode.t * t) list) =
    if col > width
    then None
    else (
      match commands with
      | [] -> Some (col, spaced)
      | (indent, mode, t) :: rest ->
        (match t with
         | Empty -> go col ~spaced rest
         | Block { text; col = from } ->
           go
             col
             ~spaced
             ((indent, mode, Text (shift_block text ~from ~col)) :: rest)
         | Text s ->
           let spaced = spaced_after s ~spaced in
           (match String.rindex s '\n' with
            | None -> go (col + String.length s) ~spaced rest
            | Some i ->
              (match mode, String.index s '\n' with
               | Break, Some first when col + first <= width ->
                 go (String.length s - i - 1) ~spaced rest
               | _ -> None))
         | Line s ->
           (match mode with
            | Flat ->
              go (col + String.length s) ~spaced:(spaced_after s ~spaced) rest
            | Break -> go indent ~spaced:true rest)
         (* A comment ending [t] ends its last line, flat or not. *)
         | End_line when List.for_all rest ~f:(fun (_, _, t) -> is_blank t) ->
           go col ~spaced []
         | Hardline | Fresh_line | End_line | Blank_line ->
           (match mode with
            | Flat -> None
            | Break -> go indent ~spaced:true rest)
         | If_break s ->
           (match mode with
            | Flat -> go col ~spaced rest
            | Break ->
              go (col + String.length s) ~spaced:(spaced_after s ~spaced) rest)
         | Cat (a, b) ->
           go col ~spaced ((indent, mode, a) :: (indent, mode, b) :: rest)
         | Leading t -> go col ~spaced ((indent, mode, t) :: rest)
         | Space ->
           if spaced
           then go col ~spaced rest
           else go (col + 1) ~spaced:true rest
         | Nest (n, t) -> go col ~spaced ((indent + n, mode, t) :: rest)
         | Align t -> go col ~spaced ((col, mode, t) :: rest)
         | Group t -> go col ~spaced ((indent, Flat, t) :: rest)
         | Alt (a, _) -> go col ~spaced ((indent, Flat, a) :: rest)))
  in
  go col ~spaced [ indent, Break, t ]
;;

let render ?(width = 80) t =
  let buf = Buffer.create 256 in
  let col = ref 0 in
  (* Indentation is written before the next text, so blank lines are empty. *)
  let pending = ref 0 in
  let line_empty = ref true in
  let end_line = ref false in
  let add s =
    if not (String.is_empty s)
    then (
      Buffer.add_string buf (String.make !pending ' ');
      pending := 0;
      line_empty := false;
      Buffer.add_string buf s;
      match String.rindex s '\n' with
      | None -> col := !col + String.length s
      | Some i -> col := String.length s - i - 1)
  in
  let newline indent =
    Buffer.add_char buf '\n';
    pending := indent;
    col := indent;
    line_empty := true;
    end_line := false
  in
  (* Spaces before a newline that [Fresh_line] or [End_line] adds, eg. between
     a comment and the comment it is followed by. *)
  let strip_and_newline indent =
    let rec strip () =
      let n = Buffer.length buf in
      if n > 0 && Char.equal (Buffer.nth buf (n - 1)) ' '
      then (
        Stdlib.Buffer.truncate buf (n - 1);
        strip ())
    in
    strip ();
    newline indent
  in
  let spaced () =
    let n = Buffer.length buf in
    !line_empty || (n > 0 && Char.equal (Buffer.nth buf (n - 1)) ' ')
  in
  let rec go (commands : (int * Mode.t * t) list) =
    match commands with
    | [] -> ()
    | (indent, mode, t) :: rest ->
      (match t with
       | Empty -> go rest
       | Block { text; col = from } ->
         if !end_line then strip_and_newline indent;
         add (shift_block text ~from ~col:!col);
         go rest
       | Text s ->
         if !end_line && not (String.is_empty (String.strip s))
         then (
           strip_and_newline indent;
           add (String.lstrip s))
         else add s;
         go rest
       | Line s ->
         (match mode with
          | Flat -> add s
          | Break -> newline indent);
         go rest
       | Hardline ->
         newline indent;
         go rest
       | Fresh_line ->
         if !line_empty
         then (
           pending := indent;
           col := indent)
         else strip_and_newline indent;
         go rest
       | End_line ->
         end_line := true;
         go rest
       | Blank_line ->
         if !line_empty
         then (
           pending := indent;
           col := indent)
         else strip_and_newline indent;
         let n = Buffer.length buf in
         if n > 0 && not (n >= 2 && Char.equal (Buffer.nth buf (n - 2)) '\n')
         then newline indent;
         go rest
       | If_break s ->
         (match mode with
          | Flat -> ()
          | Break -> add s);
         go rest
       | Cat (a, b) -> go ((indent, mode, a) :: (indent, mode, b) :: rest)
       | Leading t -> go ((indent, mode, t) :: rest)
       | Space ->
         if not (spaced ()) then add " ";
         go rest
       | Nest (n, t) -> go ((indent + n, mode, t) :: rest)
       | Align t -> go ((!col, mode, t) :: rest)
       | Group t ->
         let mode : Mode.t =
           match mode with
           | Flat -> Flat
           | Break ->
             if
               fits
                 ~spaced:(spaced ())
                 (width - !col)
                 ((indent, Flat, t) :: rest)
             then Flat
             else Break
         in
         go ((indent, mode, t) :: rest)
       | Alt (a, b) ->
         let chosen : t * Mode.t =
           match mode with
           | Flat -> a, Flat
           | Break ->
             (match fits_all ~width ~col:!col ~spaced:(spaced ()) ~indent a with
              | Some (col, spaced) when fits ~spaced (width - col) rest ->
                a, Break
              | _ -> b, Break)
         in
         go ((indent, snd chosen, fst chosen) :: rest))
  in
  go [ 0, Break, hoist t ];
  Buffer.contents buf
;;
