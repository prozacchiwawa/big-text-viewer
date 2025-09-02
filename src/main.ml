open Tea.App
open Tea.Html
open Tea_promise
open Line
open View
open Getdata
open Option
open Prom
open Msg

let init _jsdata =
  let empty = { windows = [] ; menu_open = None ; line_data = LineData.empty ; window_drag = None } in
  (empty, Tea.Cmd.none)

let new_window model =
  let spec = { x = 10 ; y = 10 ; width = 120 ; height = 50 ; title = "test window" } in
  let the_new_window =
    { spec = spec ;
      start_ptr = 0 ;
      shown_line_numbers = Array.init 50 (fun i -> { LineNumber.low = i ; LineNumber.high = 0 })
    }
  in
  ({ model with windows = the_new_window :: model.windows }, Some the_new_window)

let take_blocks bs model =
  (model, None)

let update_window model msg =
  match msg with
  | OpenMenu -> ({ model with menu_open = Some { items = [ "Open" ; "Find" ] } }, None)
  | CloseMenu -> ({ model with menu_open = None }, None)
  | HaveBlocks bs -> take_blocks bs model
  | NewWindow -> new_window model

let update model _msg =
  let updated = update_window model _msg in
  let retrieval: msg Tea.Cmd.t =
    updated
    |> snd
    |> Option.map (fun w -> GetLineData.compute_retrieval model.line_data w.shown_line_numbers)
    |> Option.map (fun p -> Tea_promise.cmd p (fun report -> Some (HaveBlocks report)))
    |> Option.orElse (fun () -> Tea.Cmd.none)
  in
  (fst updated, retrieval)

let subscriptions _model = Tea.Sub.none

let main =
  let program: (unit, display, msg) standardProgram = {
    init = init ;
    update = update ;
    view = view ;
    subscriptions = subscriptions
  } in
  standardProgram program
