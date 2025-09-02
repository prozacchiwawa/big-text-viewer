open Tea
open Tea.Html
open Tea.Html.Events
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
    title : string
  }

type display_window =
  { spec: window_spec ;

    start_ptr: int ;
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
    windows: display_window list
  }

let em v = (string_of_int v) ^ "em"
let px v = (string_of_int v) ^ "px"

let window_styles w =
  [ ("display", "flex") ;
    ("flex-direction", "column") ;
    ("position", "relative") ;
    ("left", (px w.spec.x)) ;
    ("top", (px w.spec.y)) ;
    ("width", (px w.spec.width)) ;
    ("height", (px w.spec.height))
  ]

module LineArraySequence = ArraySequence(struct type t = LineNumber.t end)
module LineToDivSequence = Sequence.Map(LineArraySequence)(struct type t = msg Vdom.t end)
module LineListSequence = Sequence.Make(LineToDivSequence)

let view_window d w =
  let line_seq = LineArraySequence.circular w.shown_line_numbers w.start_ptr in
  let div_seq =
    line_seq
    |> LineToDivSequence.map
      (fun (ln: LineNumber.t) ->
         d.line_data
         |> LineData.get_line ln
         |> Option.map (fun l -> div [classList [("window-list-elt", true)]] [text l.LineData.content])
         |> Option.orElse (fun () -> div [classList [("window-list-elt", true)]] [text "<missing>"])
      )
  in
  let div_list: msg Vdom.t list = LineListSequence.to_list div_seq in
  div [styles (window_styles w) ; classList [("window", true)]]
  [
    div [classList [("window-title", true)]] [text "title"] ;
    div
      [classList [("window-body", true)]]
      [
        div [classList [("window-list", true)]] div_list
      ]
  ]

let view model =
  div
    [classList [("root", true)]]
    [ div
        [classList [("menubar", true)]]
        [ div [classList [("menu-item", true)] ; onClick OpenMenu] [text "File"] ;
          div [classList [("menu-item", true)] ; onClick NewWindow] [text "New Window"]
        ] ;
      div [classList [("workarea", true)]] (List.map (view_window model) model.windows)
    ]
