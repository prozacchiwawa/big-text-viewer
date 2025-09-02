open Option

module type SeqT = sig
  type c
  type e
  type t

  val next : t -> (e * t) option
end

module type MapT = sig
  type t
end

module Map(T: SeqT)(E : MapT) = struct
  type c = T.c
  type e = E.t
  type f = T.e -> E.t
  type t = (f * T.t)
  let map f s: t = (f, s)
  let real_next (f: f) (seq: T.t): (e * t) option =
    T.next seq
    |> Option.map (fun (e, s) -> (f e, (f, s)))
  let next (f, seq): (e * t) option = real_next f seq
end

module Make(T: SeqT) = struct
  type t = T.t

  let to_list seq =
    let rec to_list_inner lst seq =
      match T.next seq with
      | None -> List.rev lst
      | Some (e, s) -> to_list_inner (e :: lst) s
    in
    to_list_inner [] seq

  let fold_left f start seq =
    let rec fold_left_inner acc seq =
      match T.next seq with
      | None -> acc
      | Some (elt, news) -> fold_left_inner (f acc elt) news
    in
    fold_left_inner start seq
end
