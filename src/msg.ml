open Line

type msg
  = Nop
  | OpenMenu
  | CloseMenu
  | NewWindow
  | HaveBlocks of LineData.block_report array
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | PageUp
  | PageDown
  | DisplayLines of (LineNumber.t * (LineNumber.t array))
  | CycleWindows
  | MoreDepth
  | LessDepth
  | SetLine of LineNumber.t
