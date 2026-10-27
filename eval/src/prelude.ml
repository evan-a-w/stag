open! Import

let module_name = "Stag"

let file =
  { Source_file.path = String.uncapitalize module_name ^ Source_file.extension
  ; contents = Prelude_source.source
  }
;;

let canonical path = module_name ^ "." ^ path
