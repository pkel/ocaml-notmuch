let default ~v = function
  | None -> v
  | Some a -> a

let map ~f = function
  | None -> None
  | Some a -> Some (f a)

let and_then ~f = function
  | None -> None
  | Some v -> f v

