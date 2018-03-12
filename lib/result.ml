let of_option ~err = function
  | None -> Error err
  | Some v -> Ok v

let map ~f = function
  | Error e -> Error e
  | Ok v -> Ok (f v)

let map_opt ~err ~f =
  map ~f:(fun x -> f x |> of_option ~err)

let and_then ~f = function
  | Error e -> Error e
  | Ok v -> f v

let and_then_opt ~err ~f =
  and_then ~f:(fun x -> f x |> of_option ~err)
