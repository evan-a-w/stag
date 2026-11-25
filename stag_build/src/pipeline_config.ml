open! Import

module Prelude_choice = struct
  type t =
    | Embedded
    | Checkout of Stdlib_checkout.t
  [@@deriving sexp_of]

  let canonical path =
    Option.value
      (Option.try_with (fun () -> Filename_unix.realpath path))
      ~default:path
  ;;

  let of_paths paths =
    let checkouts =
      List.map paths ~f:(fun path ->
        Stdlib_checkout.find (Filename.dirname (canonical path)))
    in
    match checkouts with
    | Some checkout :: rest
      when List.for_all rest ~f:(function
             | Some other ->
               String.equal
                 (Stdlib_checkout.root other)
                 (Stdlib_checkout.root checkout)
             | None -> false) -> Checkout checkout
    | _ -> Embedded
  ;;

  let prelude ?contents = function
    | Embedded -> Ok None
    | Checkout checkout ->
      Stdlib_checkout.prelude ?contents checkout |> Result.map ~f:Option.some
  ;;
end

type t =
  { prelude : Prelude_choice.t
  ; limits : Limits.t
  }

let create ~limits paths = { prelude = Prelude_choice.of_paths paths; limits }

let limits_param ?(default = Limits.unlimited) () =
  let limit name ~arg ~doc ~of_string ~to_string default =
    let%map_open.Command value =
      flag
        name
        (optional
           (Command.Arg_type.create (function
             | "none" -> None
             | string -> Some (of_string string))))
        ~doc:
          (sprintf
             "%s|none %s (default: %s)"
             arg
             doc
             (Option.value_map default ~default:"none" ~f:to_string))
    in
    Option.value value ~default
  in
  let%map_open.Command fuel =
    limit
      "fuel"
      ~arg:"N"
      ~doc:"evaluation steps allowed in each phase"
      ~of_string:Int.of_string
      ~to_string:Int.to_string
      default.fuel
  and max_depth =
    limit
      "max-depth"
      ~arg:"N"
      ~doc:"nesting of evaluation allowed"
      ~of_string:Int.of_string
      ~to_string:Int.to_string
      default.max_depth
  and time_limit =
    limit
      "time-limit"
      ~arg:"SPAN"
      ~doc:"time allowed for each phase, eg. 10s"
      ~of_string:Time_ns.Span.of_string
      ~to_string:Time_ns.Span.to_string
      default.time_limit
  in
  { Limits.fuel; max_depth; time_limit }
;;

let limits_readme ~phases =
  sprintf "Limits apply to each phase of evaluation on its own: %s." phases
;;
