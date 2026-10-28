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

let source_errors ?(widths = [ 80; 40; 10 ]) source =
  let errors = Queue.create () in
  let print_s sexp = Queue.enqueue errors sexp in
  (match Parser.program_with_comments ~file:"source.stag" source with
   | Error error ->
     print_s
       [%message
         "source does not parse" ~error:(Parser.Error.to_string error : string)]
   | Ok (program, comments) ->
     (* The formatter moves a comment's later lines with its first. *)
     let texts comments =
       List.map comments ~f:(fun (c : Comment.t) ->
         match String.split c.text ~on:'\n' with
         | [] -> c.text
         | first :: rest ->
           let indent line =
             String.length line - String.length (String.lstrip line)
           in
           let nonblank =
             List.filter rest ~f:(fun line ->
               not (String.is_empty (String.strip line)))
           in
           let common =
             List.map nonblank ~f:indent
             |> List.min_elt ~compare:Int.compare
             |> Option.value ~default:0
           in
           String.concat
             ~sep:"\n"
             (first
              :: List.map rest ~f:(fun line ->
                if String.is_empty (String.strip line)
                then line
                else String.drop_prefix line common)))
     in
     let format ~width source =
       match Formatter.format ~width ~file:"printed.stag" source with
       | Ok formatted -> Some formatted
       | Error error ->
         print_s
           [%message
             "formatted source does not parse"
               (width : int)
               (source : string)
               ~error:(Parser.Error.to_string error : string)];
         None
     in
     List.iter widths ~f:(fun width ->
       Option.iter (format ~width source) ~f:(fun printed ->
         match Parser.program_with_comments ~file:"printed.stag" printed with
         | Error _ -> ignore (format ~width printed : string option)
         | Ok (parsed, parsed_comments) ->
           if not (Syntax.Program.equal program parsed)
           then
             print_s
               [%message
                 "formatted source parses differently"
                   (width : int)
                   (printed : string)
                   ~syntax:
                     (strip_locs (Syntax.Program.sexp_of_t program) : Sexp.t)
                   ~parsed:
                     (strip_locs (Syntax.Program.sexp_of_t parsed) : Sexp.t)];
           if
             not
               (List.equal
                  String.equal
                  (texts comments)
                  (texts parsed_comments))
           then
             print_s
               [%message
                 "formatting changes the comments"
                   (width : int)
                   (printed : string)
                   ~comments:(texts comments : string list)
                   ~formatted:(texts parsed_comments : string list)];
           Option.iter (format ~width printed) ~f:(fun reprinted ->
             if not (String.equal printed reprinted)
             then
               print_s
                 [%message
                   "formatting is not idempotent"
                     (width : int)
                     (printed : string)
                     (reprinted : string)]))));
  Queue.to_list errors
;;

let source ?widths s = List.iter (source_errors ?widths s) ~f:print_s
