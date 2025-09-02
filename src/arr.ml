open Option
open Sequence

module type ArrayElt = sig
  type t
end

module ArraySequence(T : ArrayElt) = struct
  type c = T.t array
  type e = T.t
  type t = (bool * int * int * T.t array)

  let circular a n = (false, (n + (Array.length a) - 1) mod (Array.length a), n, a)
  let start a = circular a 0
  let next (started, s, n, a) =
    if Array.length a == 0 || (started && n == s) then
      None
    else
      Some (Array.get a n, (true, s, (n + 1) mod (Array.length a), a))
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
