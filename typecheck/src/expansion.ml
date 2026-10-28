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
      Decl_paths.map toplevel ~f:(fun namespace ~locals decl_path ->
        match decl_path.txt with
        | Path _ | Root _ -> decl_path
        | Resolved name ->
          let path : Syntax.Decl_path.t =
            match path namespace ~locals name with
            | Some path -> path
            | None ->
              let name =
                if Option.is_some (Prelude_path.relative name)
                then Prelude_path.canonical (Prelude_path.root_path name)
                else name
              in
              Path (Syntax.Path.name (sprintf "<%s>" name))
          in
          { decl_path with txt = path }))
    |> print
  in
  if String.equal (print written) generated
  then Status.Up_to_date
  else Stale { generated }
;;
