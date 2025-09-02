open Option
open Sequence

module type ArrayElt = sig
  type t
end

module ArraySequence(T : ArrayElt) = struct
  type c = T.t array
  type e = T.t
  type t = (int * int * T.t array)

  let start a = (0, 0, a)
  let circular a n = (n, n, a)
  let next (s, n, a) =
    let next_n = (n + 1) mod (Array.length a) in
    if next_n == s then
      None
    else
      Some (Array.get a n, (s, n + 1, a))
end

module ArrayExtra = struct
  let remove_opt (a : 'a option array): 'a array =
    let ro = Array.fold_left
      (fun pv a ->
        match a with
        | Some v -> v :: pv
        | None -> pv
      ) [] a
    in
    match ro with
    | [] -> [| |]
    | pv -> Array.of_list (List.rev pv)

  let decodeJson (jd: Js.Json.t -> 'a option) (json: Js.Json.t): 'a array option =
    json
    |> Js.Json.decodeArray
    |> Option.andThen
        (fun arr ->
          let alen = Array.length arr in
          if alen == 0 then
            Some (Array.of_list [])
          else
            jd (Array.get arr 0)
            |> Option.andThen
                (fun first_elt ->
                  let result: 'a array = Array.make alen first_elt in
                  let rec build_inner i =
                    if i >= alen then
                      Some result
                    else
                      jd (Array.get arr i)
                      |> Option.andThen
                          (fun elt ->
                            Array.set result i elt ;
                            build_inner (i + 1)
                          )
                  in
                  build_inner 1
                )
        )
end
