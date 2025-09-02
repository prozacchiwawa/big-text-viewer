module type EltT = sig
  type t
end

module ListSequence(E : EltT) = struct
  type c = E.t list
  type e = E.t
  type t = E.t list

  let start l = l

  let next =
    function
    | [] -> None
    | hd :: tl -> Some (hd, tl)
end

