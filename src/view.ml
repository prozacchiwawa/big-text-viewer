open Tea
open Tea.Html
open Tea.Html.Events
open Tea.Json
open Line
open Setextra
open Listextra
open Arr
open Getdata
open Msg
open Tea.Html.Attributes
open Option

type window_spec =
  { x: int ;
    y: int ;
    width: int ;
    height: int ;
    title : string ;
    z_index : int option
  }

type display_window =
  { spec: window_spec ;

    track: string ;
    edit_line_number: string ;
    depth: int ;
    shown_line_numbers: LineNumber.t array
  }

type menu =
  { items: string list
  }

type drag = Started of (int * int) | DragTo of (int * int * int * int)

type display =
  { menu_open: menu option ;
    line_data: LineData.t ;
    window_drag: drag option ;
    focus: display_window option ;
    windows: display_window list ;
    retrieving: bool
  }

let em v = (string_of_int v) ^ "em"
let px v = (string_of_int v) ^ "px"

let window_styles w =
  [ ("display", "flex") ;
    ("flex-direction", "column") ;
    ("position", "absolute") ;
    ("left", (px w.spec.x)) ;
    ("top", (px w.spec.y)) ;
    ("width", (px (w.spec.width * 8))) ;
    ("height", (px (w.spec.height * 22))) ;
    w.spec.z_index
    |> Option.map (fun z -> ("z-index", (string_of_int z)))
    |> Option.orElse (fun () -> ("z-index", "1"))
  ]

module LineArraySequence = ArraySequence(struct type t = LineNumber.t end)
module LineToDivSequence = Sequence.Map(LineArraySequence)(struct type t = msg Vdom.t end)
module LineListSequence = Sequence.Make(LineToDivSequence)

let set_line l =
  SetLine (LineNumber.from_string l)

let view_window d w =
  let _ = Js.log "window first line" in
  let _ = Js.log (LineNumber.to_string (Array.get w.shown_line_numbers 0)) in
  let line_seq = LineArraySequence.circular w.shown_line_numbers 0 in
  let first_line =
    LineNumber.to_string (Array.get w.shown_line_numbers 0)
  in
  let div_seq =
    line_seq
    |> LineToDivSequence.map
      (fun (ln: LineNumber.t) ->
         d.line_data
         |> LineData.get_line ln
         |> Option.map (fun l ->
             let _ = Js.log "show line" in
             let _ = Js.log ln in
             let _ = Js.log l in
             div
               [classList [("window-list-elt", true)]]
               [ div
                   [classList [("window-list-lineno", true)]]
                   [text (LineNumber.to_string ln)] ;
                 div
                   [classList [("window-list-content", true)]]
                   [text l.LineData.content]
               ]
            )
         |> Option.orElse (fun () -> div [classList [("window-list-elt", true)]] [text "<missing>"])
      )
  in
  let div_list: msg Vdom.t list = LineListSequence.to_list div_seq in
  div [styles (window_styles w) ; classList [("window", true)]]
  [
    div [classList [("window-title", true)]] [text "title - (depth "; text (string_of_int w.depth) ; text ")" ; text " - line" ; input' [classList [("title-input", true)] ; value first_line ; onChange set_line] []] ;
    div
      [classList [("window-body", true)]]
      [
        div [classList [("window-list", true)]] div_list
      ]
  ]

let all_windows model =
  model.focus
  |> Option.map (fun f -> f :: model.windows)
  |> Option.orElse (fun () -> model.windows)

let map_key key =
  match key with
  | "PageDown" -> PageDown
  | "PageUp" -> PageUp
  | "ArrowDown" -> ArrowDown
  | "ArrowUp" -> ArrowUp
  | "ArrowLeft" -> ArrowLeft
  | "ArrowRight" -> ArrowRight
  | "n" | "N" -> CycleWindows
  | "+" -> MoreDepth
  | "-" -> LessDepth
  | _ -> Nop

let view_keypress = Decoder.map map_key (Decoder.field "key" Decoder.string)

let view model =
  div
    [ id "root-inner" ;
      classList [("root", true)] ;
      Tea.Html.Events.on ~key:"keydown" "keydown" view_keypress
    ]
    [ div
        [classList [("menubar", true)]]
        [ div [classList [("menu-item", true)] ; onClick OpenMenu] [text "File"] ;
          div [classList [("menu-item", true)] ; onClick NewWindow] [text "New Window"]
        ] ;
      div [classList [("workarea", true)]] (List.map (view_window model) (all_windows model))
    ]

let window_depth_adj n w = { w with depth = w.depth + n }

let window_set_line w ln =
  let alen = Array.length w.shown_line_numbers in
  { w with
    edit_line_number = (LineNumber.to_string ln) ;
    shown_line_numbers = Array.init alen (fun i -> LineNumber.add ln i)
  }

let window_first_line w = Array.get w.shown_line_numbers 0

let window_last_line w =
  let alen = Array.length w.shown_line_numbers in
  let last_ptr = (w.spec.height - 1) mod alen in
  Array.get w.shown_line_numbers last_ptr

let window_set_lines at_end w ln (lines : LineNumber.t array) =
  let line_len = Array.length lines in
  let from_line_number =
    if at_end then
      Pervasives.max 0 (line_len - w.spec.height)
    else
      0
  in
  let show_lines =
      Array.init
        w.spec.height
        (fun i ->
          let iline = i + from_line_number in
          if iline >= line_len then
            LineNumber.zero
          else
            Array.get lines iline
        )
  in
  let _ = Js.log "show lines" in
  let _ = Js.log show_lines in
  { w with shown_line_numbers = show_lines }

let window_page_down model w =
  let alen = Array.length w.shown_line_numbers in
  let min_line = Array.get w.shown_line_numbers 0 in
  let next_page = LineNumber.add min_line w.spec.height in
  { w with shown_line_numbers = Array.init alen (fun i -> LineNumber.add next_page i) }
