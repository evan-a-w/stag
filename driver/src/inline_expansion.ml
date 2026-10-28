open! Import

type t =
  { loc : Loc.t
  ; expected : string
  ; actual : string
  ; indent : int
  }
[@@deriving sexp_of]

let print toplevels ~indent = Printer.toplevels ~indent:(indent + 2) toplevels

let mismatches driver =
  List.filter_map
    (Driver.checked driver).expansions
    ~f:(fun { toplevel; generated } ->
      let expansion =
        match toplevel.txt with
        | Include (_, Some expansion) | Module_splice (_, _, Some expansion) ->
          expansion
        | _ -> raise_s [%message "Inline_expansion: not an expansion"]
      in
      let indent = toplevel.loc.start.col in
      let expected = print expansion.txt ~indent in
      let actual = print generated ~indent in
      Option.some_if
        (not (String.equal expected actual))
        { loc = expansion.loc; expected; actual; indent })
;;

let edit { loc; expected = _; actual; indent } =
  let newline_indented = "\n" ^ String.make indent ' ' in
  { Source_edit.start = loc.start.offset
  ; stop = loc.stop.offset
  ; replacement =
      (if String.is_empty actual
       then newline_indented
       else "\n" ^ actual ^ newline_indented)
  }
;;
