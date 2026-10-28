open! Import

let root = "prelude"
let prefix = root ^ "."
let canonical path = prefix ^ path
let relative name = String.chop_prefix name ~prefix
let to_string_hum name = Option.value (relative name) ~default:name
