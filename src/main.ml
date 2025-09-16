open Tea.App
open Tea.Html
open Tea_promise
open Line
open View
open Getdata
open Option
open Prom
open Msg

type updateResult = JustModel of display | ModelAndCmd of (display * msg Tea.Cmd.t)

let init _jsdata =
  let empty = { windows = [] ; menu_open = None ; line_data = LineData.empty ; window_drag = None ; focus = None ; retrieving = false } in
  (empty, Tea.Cmd.none)

let defocus model =
  model.focus
  |> Option.map (fun w -> { model with focus = None ; windows = { w with spec = { w.spec with z_index = None } } :: model.windows })
  |> Option.orElse (fun () -> model)

let focus model w =
  let defocused = defocus model in
  { defocused with focus = Some { w with spec = { w.spec with z_index = Some 1000 } } ; windows = List.filter (fun tw -> tw <> w) defocused.windows }

let with_focus f model =
  model.focus
  |> Option.map f
  |> Option.orElse (fun () -> JustModel model)

let cycle_windows model =
  let defocused = defocus model in
  let first_window =
    match List.rev defocused.windows with
    | [] -> None
    | hd :: _ -> Some hd
  in
  first_window
  |> Option.map (fun f -> focus defocused f)
  |> Option.orElse (fun () -> model)

let new_window model =
  let height = 25 in
  let spec = { x = 10 ; y = 10 ; width = 120 ; height = height ; title = "test window" ; z_index = None } in
  let the_new_window =
    { spec = spec ;
      track = "main" ;
      depth = 0 ;
      edit_line_number = "0" ;
      shown_line_numbers = Array.init height (fun i -> { LineNumber.low = i ; LineNumber.high = 0 })
    }
  in
  focus model the_new_window

let redisplay_line at_end retrieve f model =
  model |> with_focus
    (fun w ->
       let (next_line, w) = f model w in
       ModelAndCmd (
         { model with focus = Some w },
         (retrieve model w)
         |> (fun prom -> Tea_promise.cmd prom (Option.map (fun p -> DisplayLines (at_end, p))))
       )
    )

let page_up model =
  model |> redisplay_line
    true
    (fun model w ->
      let next_line = window_first_line w in
      GetLineData.page_up_track w.track w.depth next_line
    )
    (fun model w ->
      let next_line = window_first_line w in
      (next_line, w)
    )

let page_down model =
  model |> redisplay_line
    false
    (fun model w ->
       let next_line = LineNumber.add (window_last_line w) 1 in
       GetLineData.page_down_track w.track w.depth next_line
    )
    (fun model w ->
       let next_line = LineNumber.add (window_last_line w) 1 in
       (next_line, w)
    )

let set_line model ln =
  model |> redisplay_line
    false
    (fun model w -> GetLineData.track w.track w.depth ln)
    (fun model w -> (ln, w))

let depth_adj n model =
  model |> redisplay_line
    false
    (fun model w ->
      let first_line = window_first_line w in
      GetLineData.page_down_track w.track w.depth first_line
    )
    (fun model w ->
      let first_line = window_first_line w in
      let new_w = window_depth_adj n w in
      (first_line, new_w)
     )

let move_window model tx =
  model |> with_focus
    (fun w -> JustModel { model with focus = Some { w with spec = tx w.spec } })

let drag_up model = move_window model (fun spec -> { spec with y = spec.y - 22 })
let drag_down model = move_window model (fun spec -> { spec with y = spec.y + 22 })
let drag_left model = move_window model (fun spec -> { spec with x = spec.x - 8 })
let drag_right model = move_window model (fun spec -> { spec with x = spec.x + 8 })

let take_blocks bs model =
  let model = { model with line_data = LineData.new_blocks bs model.line_data ; retrieving = false } in
  JustModel model

let set_display at_end model ln lines =
  model.focus
  |> Option.map (fun w -> { model with focus = Some (window_set_lines at_end w ln lines) })
  |> Option.orElse (fun () -> model)

let update_window model msg: updateResult =
  match msg with
  | Nop -> JustModel model
  | OpenMenu -> JustModel { model with menu_open = Some { items = [ "Open" ; "Find" ] } }
  | CloseMenu -> JustModel { model with menu_open = None }
  | HaveBlocks bs -> take_blocks bs model
  | NewWindow -> JustModel (new_window model)
  | PageUp -> page_up model
  | PageDown -> page_down model
  | ArrowUp -> drag_up model
  | ArrowDown -> drag_down model
  | ArrowLeft -> drag_left model
  | ArrowRight -> drag_right model
  | CycleWindows -> JustModel (cycle_windows model)
  | SetLine n -> set_line model n
  | MoreDepth -> depth_adj 1 model
  | LessDepth -> depth_adj (-1) model
  | DisplayLines (at_end, (l, lines)) -> JustModel (set_display at_end model l lines)

let update model _msg =
  let (updated, cmd) =
    match update_window model _msg with
    | JustModel m -> (m, Tea.Cmd.none)
    | ModelAndCmd p -> p
  in
  let retrieval: msg Tea.Cmd.t option =
    if updated.retrieving then
      None
    else
      updated.focus
      |> Option.andThen (fun w -> GetLineData.compute_retrieval model.line_data w.shown_line_numbers)
      |> Option.map (fun p -> Tea_promise.cmd p (fun report -> Some (HaveBlocks report)))
  in
  retrieval
  |> Option.map (fun r -> ({ updated with retrieving = true }, Tea.Cmd.batch [r ; cmd]))
  |> Option.orElse (fun () -> (updated, cmd))

let subscriptions _model = Tea.Sub.none

let main =
  let program: (unit, display, msg) standardProgram = {
    init = init ;
    update = update ;
    view = view ;
    subscriptions = subscriptions
  } in
  standardProgram program
