import PN
import Control.Lens
import Data.Char
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List as List
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart = toRenderable layout
  where
    pns :: Int -> [(Int, Int)]
    pns x = zip [1..](map toDecimal (take x pnNumbers))

    points = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ pns 1000
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot points]
           $ def

main = renderableToFile def "seq.png" chart
