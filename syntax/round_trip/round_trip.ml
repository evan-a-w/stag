open! Import

let rec strip_locs : Sexp.t -> Sexp.t = function
  | List [ txt; Atom "@"; _ ] -> strip_locs txt
  | List sexps -> List (List.map sexps ~f:strip_locs)
  | Atom _ as atom -> atom
;;

let check
      ?(widths = [ 80; 40; 10 ])
      ~(print : ?width:int -> 'a -> string)
      ~(parse : file:string -> string -> ('a, Parser.Error.t) Result.t)
      ~equal
      ~sexp_of
      t
  =
  let sexp_of t = strip_locs (sexp_of t) in
  List.iter widths ~f:(fun width ->
    let printed = print ~width t in
    match parse ~file:"printed.stag" printed with
    | Error error ->
      print_s
        [%message
          "printed syntax does not parse"
            (width : int)
            (printed : string)
            ~error:(Parser.Error.to_string error : string)]
    | Ok parsed when not (equal t parsed) ->
      print_s
        [%message
          "printed syntax parses differently"
            (width : int)
            (printed : string)
            ~syntax:(sexp_of t : Sexp.t)
            ~parsed:(sexp_of parsed : Sexp.t)]
    | Ok parsed ->
      let reprinted = print ~width parsed in
      if not (String.equal printed reprinted)
      then
        print_s
          [%message
            "printing is not idempotent"
              (width : int)
              (printed : string)
              (reprinted : string)])
;;

let type_expr ?widths =
  check
    ?widths
    ~print:Printer.type_expr
    ~parse:Parser.type_expr
    ~equal:Syntax.Type_expr.equal
    ~sexp_of:Syntax.Type_expr.sexp_of_t
;;

let pattern ?widths =
  check
    ?widths
    ~print:Printer.pattern
    ~parse:Parser.pattern
    ~equal:Syntax.Pattern.equal
    ~sexp_of:Syntax.Pattern.sexp_of_t
;;

let expr ?widths =
  check
    ?widths
    ~print:Printer.expr
    ~parse:Parser.expr
    ~equal:Syntax.Expr.equal
    ~sexp_of:Syntax.Expr.sexp_of_t
;;

let program ?widths =
  check
    ?widths
    ~print:Printer.program
    ~parse:Parser.program
    ~equal:Syntax.Program.equal
    ~sexp_of:Syntax.Program.sexp_of_t
;;
