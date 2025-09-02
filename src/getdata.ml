open Webapi
open Line
open Option
open Prom
open Arr
open Setextra
open Listextra

module GetLineData = struct
  (* Figure out what we need to retrieve to fill the display *)
  module LineNumberArraySeq = ArraySequence(LineNumber)
  module LineNumberMapSeq = Sequence.Map(LineNumberArraySeq)(LineNumber)
  module LineNumberSetArrExtra = SetExtra(LineNumber)(LineNumberMapSeq)
  module LineNumberListSeq = ListSequence(LineNumber)
  module LineNumberSetListExtra = SetExtra(LineNumber)(LineNumberListSeq)

  let getlines (line_in : LineNumber.t): LineData.block_report option Js.Promise.t =
    let l = LineNumber.line_block line_in in
    Webapi.Fetch.fetch ("/lines?low=" ^ (string_of_int l.low) ^ "&high=" ^ (string_of_int l.high))
    |> Js.Promise.then_ (Webapi.Fetch.Response.json)
    |> PromiseExtra.map (ArrayExtra.decodeJson LineData.decode_info)
    |> PromiseExtra.map (Option.map (fun block -> { LineData.number = line_in ; lines = block }))

  let compute_retrieval line_data shown_line_numbers: LineData.block_report array Js.Promise.t =
    let want_lines = LineNumberArraySeq.start shown_line_numbers in
    let want_lines_block = LineNumberMapSeq.map LineNumber.line_block want_lines in
    let want_lines_set = LineNumberSetArrExtra.from_seq want_lines_block in
    let have_lines_list = LineData.have_blocks line_data in
    let have_lines_seq = LineNumberListSeq.start have_lines_list in
    let have_lines_set = LineNumberSetListExtra.from_seq have_lines_seq in
    let want_blocks = LineNumberSet.diff want_lines_set have_lines_set in
    want_blocks
    |> LineNumberSet.elements
    |> List.map getlines
    |> Array.of_list
    |> Js.Promise.all
    |> PromiseExtra.map ArrayExtra.remove_opt
end
