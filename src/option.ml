module Option = struct
  type 'a t = 'a option

  let map f v =
    match v with
    | None -> None
    | Some v -> Some (f v)

  let andThen f v =
    match v with
    | None -> None
    | Some v -> f v

  let orElse f v =
   match v with
   | None -> f ()
   | Some v -> v

  let is_some =
    function
    | None -> false
    | _ -> true
end
