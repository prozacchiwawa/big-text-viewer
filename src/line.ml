open Option
open Arr

module LineNumber = struct
  type t = { low: int ; high: int }

  let int_max = 0xfffffff
  let zero = { high = 0 ; low = 0 }
  let chunk_size = 0x400

  let compare a b =
    if a.high > b.high then
      -1
    else if a.high < b.high then
      1
    else
      compare a.low b.low

  let equal a b = compare a b == 0

  let line_block ln =
    let trunc = ln.low - (ln.low mod chunk_size) in
    { ln with low = trunc }

  let next ln =
    if ln.low == int_max then
      { high = ln.high + 1 ; low = 0 }
    else
      { ln with low = ln.low + 1 }

  let prev ln =
    if equal ln zero then
      zero
    else if ln.low == 0 then
      { high = ln.high - 1 ; low = int_max }
    else
      { ln with low = ln.low - 1 }

  let rec sub ln v =
    if v < 0 then
      add ln (v * -1)
    else if v == 0 then
      ln
    else
      sub (prev ln) (v - 1)

  and add ln v =
    if v < 0 then
      sub ln (v * -1)
    else if v == 0 then
      ln
    else
      add (next ln) (v - 1)

  let decode json =
    Js.Json.decodeObject json
    |> Option.andThen (fun dict ->
        match (Js.Dict.get dict "low", Js.Dict.get dict "high") with
        | (Some low, Some high) ->
            (match (Js.Json.decodeNumber low, Js.Json.decodeNumber high) with
             | (Some low, Some high) -> Some { low = int_of_float low ; high = int_of_float high }
             | _ -> None
            )
        | _ -> None
       )

  let from_string l =
    let number = float_of_string l in
    let num_div = float_of_int (1 lsl 28) in
    let high_part = floor (number /. num_div) in
    let low_part = int_of_float (number -. (high_part *. num_div)) in
    { low = low_part ; high = int_of_float high_part }

  let to_string ln =
    let num_div = float_of_int (1 lsl 28) in
    let high = (float_of_int ln.high) *. num_div in
    let low = float_of_int ln.low in
    let s = String.split_on_char '.' (string_of_float (low +. high)) in
    match s with
    | [] -> "0"
    | hd :: _ -> hd
end

module LineNumberMap = Map.Make(LineNumber)
module LineNumberSet = Set.Make(LineNumber)

module LineData = struct
  type info = { content: string }
  type t = { map: info array LineNumberMap.t }

  let empty = { map = LineNumberMap.empty }

  let decode_info (json: Js.Json.t): info option =
    json
    |> Js.Json.decodeObject
    |> Option.andThen (fun dict -> Js.Dict.get dict "content")
    |> Option.andThen Js.Json.decodeString
    |> Option.map (fun content -> { content = content })

  let have_blocks self =
    LineNumberMap.bindings self.map
    |> List.map (fun (k, v) -> k)

  type block_report = { number: LineNumber.t ; lines : info array }
  module BlockArraySeqT = ArraySequence(struct type t = block_report end)
  module BlockArraySeq = Sequence.Make(BlockArraySeqT)

  let new_blocks blocks self =
    let seq = BlockArraySeqT.start blocks in
    let new_map = BlockArraySeq.fold_left
      (fun m bs -> LineNumberMap.add bs.number bs.lines m) self.map seq
    in
    { self with map = new_map }

  let get_line l self: info option =
    let nearest_chunk = LineNumber.line_block l in
    self.map
    |> LineNumberMap.find_opt nearest_chunk
    |> Option.map
        (fun chunk ->
          let inside_chunk = l.low mod LineNumber.chunk_size in
          Array.get chunk inside_chunk
        )
end

