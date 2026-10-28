open! Import

type t =
  | Empty
  | Text of string
  | Line of string
  | Hardline
  | If_break of string
  | Cat of t * t
  | Nest of int * t
  | Align of t
  | Group of t

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
let if_break s = If_break s
let nest n t = Nest (n, t)
let align t = Align t
let group t = Group t

module Mode = struct
  type t =
    | Flat
    | Break
  [@@deriving equal]
end

(* Whether the commands fit in [width], up to the first newline (of a broken
   [line] or a text). *)
let rec fits width (commands : (int * Mode.t * t) list) =
  width >= 0
  &&
  match commands with
  | [] -> true
  | (indent, mode, t) :: rest ->
    (match t with
     | Empty -> fits width rest
     | Text s ->
       (match String.index s '\n' with
        | None -> fits (width - String.length s) rest
        | Some i -> Mode.equal mode Break && i <= width)
     | Line s ->
       (match mode with
        | Flat -> fits (width - String.length s) rest
        | Break -> true)
     | Hardline -> Mode.equal mode Break
     | If_break s ->
       (match mode with
        | Flat -> fits width rest
        | Break -> fits (width - String.length s) rest)
     | Cat (a, b) -> fits width ((indent, mode, a) :: (indent, mode, b) :: rest)
     | Nest (_, t) | Align t | Group t -> fits width ((indent, mode, t) :: rest))
;;

let render ?(width = 80) t =
  let buf = Buffer.create 256 in
  let col = ref 0 in
  (* Indentation is written before the next text, so blank lines are empty. *)
  let pending = ref 0 in
  let add s =
    if not (String.is_empty s)
    then (
      Buffer.add_string buf (String.make !pending ' ');
      pending := 0;
      Buffer.add_string buf s;
      match String.rindex s '\n' with
      | None -> col := !col + String.length s
      | Some i -> col := String.length s - i - 1)
  in
  let newline indent =
    Buffer.add_char buf '\n';
    pending := indent;
    col := indent
  in
  let rec go (commands : (int * Mode.t * t) list) =
    match commands with
    | [] -> ()
    | (indent, mode, t) :: rest ->
      (match t with
       | Empty -> go rest
       | Text s ->
         add s;
         go rest
       | Line s ->
         (match mode with
          | Flat -> add s
          | Break -> newline indent);
         go rest
       | Hardline ->
         newline indent;
         go rest
       | If_break s ->
         (match mode with
          | Flat -> ()
          | Break -> add s);
         go rest
       | Cat (a, b) -> go ((indent, mode, a) :: (indent, mode, b) :: rest)
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
         go ((indent, mode, t) :: rest))
  in
  go [ 0, Break, t ];
  Buffer.contents buf
;;
