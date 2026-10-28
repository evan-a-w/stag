open! Import

module Status = struct
  type t =
    | Up_to_date
    | Stale of { generated : string }
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t
  ; expansion : Syntax.Toplevel.t list Located.t
  ; status : Status.t
  }
[@@deriving sexp_of]

let status ~written generated ~column ~path =
  let print = Printer.program ~width:(Int.max 1 (78 - column)) in
  let generated =
    List.map generated ~f:(fun toplevel ->
      Decl_paths.map toplevel ~f:(fun namespace decl_path ->
        match decl_path.txt with
        | Path _ -> decl_path
        | Resolved name ->
          let path =
            match path namespace name with
            | Some path -> path
            | None -> Syntax.Path.name (sprintf "<%s>" name)
          in
          { decl_path with txt = Path path }))
    |> print
  in
  if String.equal (print written) generated
  then Status.Up_to_date
  else Stale { generated }
;;
