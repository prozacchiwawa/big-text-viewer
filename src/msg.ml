open Line

type msg = OpenMenu | CloseMenu | NewWindow | HaveBlocks of LineData.block_report array
