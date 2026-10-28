open! Import

type t =
  | Empty
  | Text of string
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
  | Empty | Line _ | Hardline | Fresh_line | End_line | Blank_line | Space ->
    init, t
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

(* Whether the commands fit in [width], up to the first newline (of a broken
   [line] or a text). After an [End_line], only if a newline comes before any
   more of the flat text. *)
let rec fits ?(ended = false) width (commands : (int * Mode.t * t) list) =
  width >= 0
  &&
  match commands with
  | [] -> true
  | (indent, mode, t) :: rest ->
    (match t with
     | Empty -> fits ~ended width rest
     | Text s when ended ->
       (String.is_empty (String.strip s) && fits ~ended width rest)
       || Mode.equal mode Break
     | Text s ->
       (match String.index s '\n' with
        | None -> fits (width - String.length s) rest
        | Some i -> Mode.equal mode Break && i <= width)
     | Line s ->
       (match mode with
        | Flat -> fits ~ended (width - String.length s) rest
        | Break -> true)
     | Space -> fits ~ended (width - 1) rest
     | Hardline | Fresh_line | Blank_line -> Mode.equal mode Break
     | End_line -> fits ~ended:true width rest
     | If_break s ->
       (match mode with
        | Flat -> fits ~ended width rest
        | Break -> ended || fits (width - String.length s) rest)
     | Cat (a, b) ->
       fits ~ended width ((indent, mode, a) :: (indent, mode, b) :: rest)
     | Nest (_, t) | Align t | Group t | Leading t ->
       fits ~ended width ((indent, mode, t) :: rest)
     | Alt (a, b) ->
       let t =
         match mode with
         | Flat -> a
         | Break -> b
       in
       fits ~ended width ((indent, mode, t) :: rest))
;;

(* [Leading]s at the start of a group, before it instead. *)
let rec hoist t =
  match t with
  | Empty
  | Text _
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

let fits_flat ~width t = fits width [ 0, Flat, hoist t ]

(* Whether [t], starting at [col] and laid out with its groups flat (its other
   lines broken), fits in [width] on each of its lines: the column it ends at
   if so. Only the lines of [t] are looked at, each up to the width. *)
let fits_all ~width ~col ~indent t =
  let rec go col (commands : (int * Mode.t * t) list) =
    if col > width
    then None
    else (
      match commands with
      | [] -> Some col
      | (indent, mode, t) :: rest ->
        (match t with
         | Empty -> go col rest
         | Text s ->
           (match String.rindex s '\n' with
            | None -> go (col + String.length s) rest
            | Some i ->
              (match mode, String.index s '\n' with
               | Break, Some first when col + first <= width ->
                 go (String.length s - i - 1) rest
               | _ -> None))
         | Line s ->
           (match mode with
            | Flat -> go (col + String.length s) rest
            | Break -> go indent rest)
         | Hardline | Fresh_line | End_line | Blank_line ->
           (match mode with
            | Flat -> None
            | Break -> go indent rest)
         | If_break s ->
           (match mode with
            | Flat -> go col rest
            | Break -> go (col + String.length s) rest)
         | Cat (a, b) -> go col ((indent, mode, a) :: (indent, mode, b) :: rest)
         | Leading t -> go col ((indent, mode, t) :: rest)
         | Space -> go (col + 1) rest
         | Nest (n, t) -> go col ((indent + n, mode, t) :: rest)
         | Align t -> go col ((col, mode, t) :: rest)
         | Group t -> go col ((indent, Flat, t) :: rest)
         | Alt (a, _) -> go col ((indent, Flat, a) :: rest)))
  in
  go col [ indent, Break, t ]
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
  let rec go (commands : (int * Mode.t * t) list) =
    match commands with
    | [] -> ()
    | (indent, mode, t) :: rest ->
      (match t with
       | Empty -> go rest
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
         if !line_empty then pending := indent else strip_and_newline indent;
         go rest
       | End_line ->
         end_line := true;
         go rest
       | Blank_line ->
         if !line_empty then pending := indent else strip_and_newline indent;
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
         let n = Buffer.length buf in
         if
           not
             (!line_empty || (n > 0 && Char.equal (Buffer.nth buf (n - 1)) ' '))
         then add " ";
         go rest
       | Nest (n, t) -> go ((indent + n, mode, t) :: rest)
       | Align t -> go ((!col, mode, t) :: rest)
       | Group t ->
         let mode : Mode.t =
           match mode with
           | Flat -> Flat
           | Break ->
             if fits (width - !col) ((indent, Flat, t) :: rest)
             then Flat
             else Break
         in
         go ((indent, mode, t) :: rest)
       | Alt (a, b) ->
         let chosen : t * Mode.t =
           match mode with
           | Flat -> a, Flat
           | Break ->
             (match fits_all ~width ~col:!col ~indent a with
              | Some col when fits (width - col) rest -> a, Break
              | _ -> b, Break)
         in
         go ((indent, snd chosen, fst chosen) :: rest))
  in
  go [ 0, Break, hoist t ];
  Buffer.contents buf
;;
