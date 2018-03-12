type ('ok , 'err) t =
  | Ok of 'ok
  | Err of 'err

val ok  : 'ok -> ('ok, _) t
val err : 'err -> (_, 'err) t
