open! Import

type ('a, 'e) t = ('a, 'e) Result.t

let return = (Result.return [@mode local])
let bind = (Result.bind [@mode local])
let map = (Result.map [@mode local])

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
  end
end
