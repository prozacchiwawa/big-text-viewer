open Sequence

module type Ord = sig
  type t
  val compare : t -> t -> int
end

module SetExtra(T : Ord) (E : SeqT with type e = T.t) = struct
  module SetType = Set.Make(T)
  module SeqType = Sequence.Make(E)
  let from_seq (s: SeqType.t): SetType.t =
    s |> SeqType.fold_left (fun s elt -> SetType.add elt s) SetType.empty
end
