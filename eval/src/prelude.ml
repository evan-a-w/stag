open! Import

let path = "<prelude>"
let source = Prelude_source.source

let toplevels =
  lazy
    (match Parser.program ~file:path source with
     | Ok toplevels -> toplevels
     | Error error ->
       raise_s [%message "Prelude failed to parse" (error : Parser.Error.t)])
;;
