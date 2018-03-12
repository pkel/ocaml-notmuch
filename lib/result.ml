type ('ok , 'err) t =
  | Ok of 'ok
  | Err of 'err

let ok a = Ok a
let err a = Err a
