open! Import
open Syntax

(* Random syntax, of the forms the parser produces, printed at several widths
   and checked to round trip. *)

let loc txt = Located.create txt

module Gen = struct
  type t =
    { random : Random.State.t
    ; mutable size : int
    ; deep : bool
      (** Whether to take the recursive choices while size is left. *)
    }

  let int t n = Random.State.int t.random n
  let bool t = Random.State.bool t.random
  let pick t xs = List.nth_exn xs (int t (List.length xs))

  let list t ~min ~max f =
    List.init (min + int t (max - min + 1)) ~f:(fun _ -> f t)
  ;;

  (* Each choice is weighted, and the recursive ones are only taken while
     there is size left, which they use up. *)
  let choose t ~leaves ~nodes =
    if t.size <= 0 || ((not t.deep) && int t 4 = 0)
    then (pick t leaves) t
    else (
      t.size <- t.size - 1;
      (pick t nodes) t)
  ;;

  let lident t =
    pick t [ "x"; "y"; "long_name"; "f"; "x'"; "_x"; "in_"; "letx"; "with'" ]
  ;;

  let uident t = pick t [ "A"; "B"; "Some_constructor"; "A'"; "B_" ]
  let label t = loc (pick t [ "a"; "b"; "long_label"; "A"; "Some_tag"; "_x'" ])
  let name t = loc (lident t)

  let path t ~last =
    Path.of_list_exn
      (List.init (int t 3 / 2) ~f:(fun _ -> pick t [ "M"; "N" ]) @ [ last t ])
  ;;

  let string t =
    pick
      t
      [ ""
      ; "s"
      ; "a\"b\\c\n\t\001é"
      ; "|}"
      ; "{|"
      ; "a |x} b |} c"
      ; "\r\n"
      ; "\255\254 bytes"
      ; "a long string literal"
      ; "\n\nleading and trailing blank lines\n\n"
      ; "\n  indented\n    more\n  less\n"
      ; "line one\nline two"
      ]
  ;;

  let lit t : Lit.t =
    pick
      t
      [ Lit.Int 0
      ; Int 42
      ; Int (-1)
      ; Int Int.min_value
      ; Bool true
      ; Bool false
      ; String (string t)
      ; Char (Char.of_int_exn (int t 256))
      ; Unit
      ]
  ;;

  let rec row ?(label = label) t ~entry : Type_expr.row =
    loc
      (match int t 3 with
       | 0 -> Type_expr.Empty
       | 1 -> Var (loc "r")
       | _ -> Extend (label t, entry t, row ~label t ~entry))
  ;;

  let rec type_expr t : Type_expr.t =
    let unit = loc (Type_expr.Con (loc (Path.name "unit"), [])) in
    loc
      (choose
         t
         ~leaves:
           [ (fun t : Type_expr.desc -> Var (pick t [ "a"; "b" ]))
           ; (fun t -> Con (loc (path t ~last:lident), []))
           ; (fun _ -> Tuple [])
           ]
         ~nodes:
           [ (fun t ->
               Con
                 ( loc (path t ~last:lident)
                 , list t ~min:1 ~max:3 (fun t : Type_expr.arg ->
                     if bool t
                     then Ty (type_expr t)
                     else Row (row t ~entry:type_expr)) ))
           ; (fun t -> Arrow (type_expr t, type_expr t))
           ; (fun t -> Tuple (list t ~min:2 ~max:3 type_expr))
           ; (fun t ->
               Code
                 { inner = type_expr t
                 ; context = row ~label:name t ~entry:type_expr
                 })
           ; (fun t -> Record (row t ~entry:type_expr))
           ; (fun t ->
               Variant
                 (row t ~entry:(fun t -> if bool t then unit else type_expr t)))
           ])
  ;;

  let rec pattern t : Pattern.t =
    loc
      (choose
         t
         ~leaves:
           [ (fun _ -> Pattern.Any)
           ; (fun t -> Var (lident t))
           ; (fun t -> Lit (lit t))
           ; (fun t -> Construct (loc (path t ~last:uident), []))
           ; (fun t -> Poly_variant (label t, None))
           ]
         ~nodes:
           [ (fun t -> Tuple (list t ~min:2 ~max:3 pattern))
           ; (fun t -> Construct (loc (path t ~last:uident), [ pattern t ]))
           ; (fun t ->
               Construct
                 (loc (path t ~last:uident), list t ~min:2 ~max:3 pattern))
           ; (fun t -> Poly_variant (label t, Some (pattern t)))
           ; (fun t ->
               Record
                 (list t ~min:1 ~max:3 (fun t ->
                    loc (path t ~last:lident), pattern t)))
           ; (fun t ->
               Poly_record
                 { fields = list t ~min:0 ~max:3 (fun t -> label t, pattern t)
                 ; rest = pick t [ Pattern.Rest.Closed; Open; Bind (name t) ]
                 })
           ])
  ;;

  let binding t : Binding.t = name t, Option.some_if (bool t) (type_expr t)

  let rec expr t : Expr.t =
    loc
      (choose
         t
         ~leaves:
           [ (fun t -> Expr.Lit (lit t))
           ; (fun t -> Var (path t ~last:lident, None))
           ; (fun t -> Var (path t ~last:lident, Some (type_expr t)))
           ; (fun t -> Construct (loc (path t ~last:uident), []))
           ; (fun t -> Poly_variant (label t, None))
           ; (fun t -> Expect (loc (string t)))
           ; (fun _ -> Poly_record [])
           ]
         ~nodes:
           [ (fun t -> App (expr t, expr t))
           ; (fun t -> App (expr t, expr t))
           ; (fun t ->
               Let
                 ( name t
                 , list t ~min:0 ~max:2 binding
                 , Option.some_if (bool t) (type_expr t)
                 , expr t
                 , expr t ))
           ; (fun t -> Let (loc "_", [], None, expr t, expr t))
           ; (fun t ->
               Splice
                 { expr = expr t
                 ; subs =
                     Option.some_if
                       (bool t)
                       (list t ~min:0 ~max:2 (fun t ->
                          name t, Option.some_if (bool t) (expr t)))
                 })
           ; (fun t -> Quote (expr t))
           ; (fun t -> Tuple (list t ~min:2 ~max:3 expr))
           ; (fun t -> Construct (loc (path t ~last:uident), [ expr t ]))
           ; (fun t ->
               Construct (loc (path t ~last:uident), list t ~min:2 ~max:3 expr))
           ; (fun t ->
               Record
                 (list t ~min:1 ~max:3 (fun t ->
                    loc (path t ~last:lident), expr t)))
           ; (fun t -> Field (expr t, loc (path t ~last:lident)))
           ; (fun t ->
               Match (expr t, list t ~min:1 ~max:3 (fun t -> pattern t, expr t)))
           ; (fun t ->
               Poly_record (list t ~min:1 ~max:3 (fun t -> label t, expr t)))
           ; (fun t -> Poly_field (expr t, label t))
           ; (fun t ->
               Poly_extend
                 (list t ~min:1 ~max:3 (fun t -> label t, expr t), expr t))
           ; (fun t -> Poly_restrict (expr t, label t))
           ; (fun t ->
               Poly_update
                 (expr t, list t ~min:1 ~max:3 (fun t -> label t, expr t)))
           ; (fun t -> Poly_variant (label t, Some (expr t)))
           ])
  ;;

  let type_decl t : Type_decl.t =
    { name = name t
    ; params =
        list t ~min:0 ~max:2 (fun t ->
          loc (if bool t then Type_decl.Param.Ty "a" else Row "r"))
    ; kind =
        (match int t 4 with
         | 0 -> Abstract
         | 1 -> Alias (type_expr t)
         | 2 ->
           Variant
             (list t ~min:0 ~max:3 (fun t ->
                loc (uident t), list t ~min:0 ~max:3 type_expr))
         | _ -> Record (list t ~min:1 ~max:3 (fun t -> name t, type_expr t)))
    }
  ;;

  let rec toplevel t : Toplevel.t =
    loc
      (match int t 6 with
       | 0 ->
         Toplevel.Let
           ( name t
           , list t ~min:0 ~max:2 binding
           , Option.some_if (bool t) (type_expr t)
           , expr t )
       | 1 | 2 -> Type (type_decl t)
       | 3 -> Expect_test { name = loc (string t); body = expr t }
       | 4 when t.size > 0 ->
         t.size <- t.size - 1;
         Module (loc (uident t), list t ~min:0 ~max:3 toplevel)
       | _ -> Let (name t, [], None, expr t))
  ;;
end

module Line_counts = struct
  type t =
    { lines : int
    ; over_width : int
    }
  [@@deriving sexp_of]

  let zero = { lines = 0; over_width = 0 }

  let ( + ) a b =
    { lines = a.lines + b.lines; over_width = a.over_width + b.over_width }
  ;;

  let of_string s ~width =
    let lines = String.split_lines s in
    { lines = List.length lines
    ; over_width = List.count lines ~f:(fun line -> String.length line > width)
    }
  ;;
end

(* Each width's line counts, summed. *)
module Counts = struct
  type t = Line_counts.t Int.Map.t [@@deriving sexp_of]

  let add t ~width counts =
    Map.update t width ~f:(fun existing ->
      Line_counts.(Option.value existing ~default:zero + counts))
  ;;
end

(* Round trips [t] at each width, counting the printed lines. *)
let check
      counts
      ~widths
      ~(round_trip : ?widths:int list -> 'a -> unit)
      ~(print : ?width:int -> 'a -> string)
      t
  =
  round_trip ~widths t;
  List.fold widths ~init:counts ~f:(fun counts width ->
    if width < 10
    then counts
    else
      Counts.add counts ~width (Line_counts.of_string (print ~width t) ~width))
;;

let check_all counts ~widths (gen : unit -> Gen.t) =
  let type_expr = Gen.type_expr (gen ()) in
  let pattern = Gen.pattern (gen ()) in
  let expr = Gen.expr (gen ()) in
  let program =
    let gen = gen () in
    List.init 3 ~f:(fun _ -> Gen.toplevel gen)
  in
  let counts =
    check
      counts
      ~widths
      ~round_trip:Round_trip.type_expr
      ~print:Printer.type_expr
      type_expr
  in
  let counts =
    check
      counts
      ~widths
      ~round_trip:Round_trip.pattern
      ~print:Printer.pattern
      pattern
  in
  let counts =
    check counts ~widths ~round_trip:Round_trip.expr ~print:Printer.expr expr
  in
  check
    counts
    ~widths
    ~round_trip:Round_trip.program
    ~print:Printer.program
    program
;;

(* Lines over the width are mostly from nesting deeper than the width, and
   chains of long constructor names. *)
let%expect_test "random syntax round trips" =
  let random = Random.State.make [| 0 |] in
  let counts = ref Int.Map.empty in
  for size = 0 to 5000 do
    counts
    := check_all !counts ~widths:[ 80; 30; 1 ] (fun () ->
         { Gen.random; size = size % 60; deep = false })
  done;
  print_s [%sexp (!counts : Counts.t)];
  [%expect
    {|
    ((30 ((lines 521108) (over_width 266269)))
     (80 ((lines 397110) (over_width 64579))))
    |}]
;;

let%expect_test "deep random syntax round trips" =
  let random = Random.State.make [| 1 |] in
  let start = Time_ns.now () in
  let time_limit = Time_ns.Span.of_sec 60. in
  let counts = ref Int.Map.empty in
  let count = ref 0 in
  while
    !count < 200
    && Time_ns.Span.( < ) (Time_ns.diff (Time_ns.now ()) start) time_limit
  do
    counts
    := check_all !counts ~widths:[ 80; 30 ] (fun () ->
         { Gen.random; size = 20 + !count; deep = true });
    incr count
  done;
  print_s [%message (count : int ref) (!counts : Counts.t)];
  [%expect
    {|
    ((count 200)
     (!counts
      ((30 ((lines 95172) (over_width 79873)))
       (80 ((lines 88529) (over_width 57231))))))
    |}]
;;
