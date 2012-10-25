module CairoHistogram (
  hasCairoHistograms,
  writeHistogram
) where

import           Control.Monad.ST (runST)
import           Data.Accessor ((^=), (^:))
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as ColourNames
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
import qualified Graphics.Rendering.Chart as Chart
import           Statistics.Test.ApproxRand
import           Statistics.Test.Types (TestType(..))
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

import           Histogram

hasCairoHistograms :: Bool
hasCairoHistograms = True

writeHistogram :: TestOptions -> Int -> TestResult -> FP.FilePath -> IO ()
writeHistogram testOptions bins result path =
  case histogram bins result of
    Left err -> hPutStrLn stderr err
    Right  h ->
      let r = createHistogram testOptions result h in
        case snd $ FP.splitExtension path of
          ".pdf" -> Chart.renderableToPDFFile r 800 600 path
          ".png" -> do
            _ <- Chart.renderableToPNGFile r 800 600 path
            return ()
          ".ps"  -> Chart.renderableToPSFile  r 800 600 path
          ".svg" -> Chart.renderableToSVGFile r 800 600 path
          _      -> hPutStrLn stderr "Unknown output format!"

-- Creates a histogram. The histogram is stacked, but the second bar
-- is always empty, except for the bin of the original statistic (if any).
-- There, the first bar is empty and the second bar has the frequency.
-- Yes, this is cheating ;).
createHistogram :: TestOptions -> TestResult -> [(Double, Int)] ->
  Chart.Renderable ()
createHistogram testOptions result his =
  Chart.toRenderable layout
  where
    layout =
        Chart.layout1_background  ^= Chart.solidFillStyle opaqueWhite
      $ Chart.layout1_left_axis   ^: Chart.laxis_override ^= Chart.axisTicksHide
      $ Chart.layout1_right_axis  ^: Chart.laxis_title    ^= "Frequency"
      $ Chart.layout1_bottom_axis ^: Chart.laxis_title    ^= "Statistic"
      $ Chart.layout1_plots       ^= [ Right (Chart.plotBars randomizationBars),
                                       Right statisticLine, Right sigLines ]
      $ Chart.setLayout1Foreground   opaqueBlack
      $ Chart.defaultLayout1
    randomizationBars =
        Chart.plot_bars_style       ^= Chart.BarsStacked
      $ Chart.plot_bars_spacing     ^= Chart.BarsFixGap 6 2
      -- $ Chart.plot_bars_spacing     ^= Chart.BarsFixGap 0 0
      $ Chart.plot_bars_item_styles ^= [
          (Chart.solidFillStyle $ opaqueGreen, Nothing) ]
      $ Chart.plot_bars_values      ^= map (\(b, f) -> (b, [f])) his
      $ Chart.defaultPlotBars
    statisticLine =
      Chart.vlinePlot "Statistic for samples" (Chart.solidLine 2 (opaqueRed)) $ trStat result
    sigLines      =
      vlinesPlot "Significance" (Chart.solidLine 2 opaqueBlack) $
        sigBounds testOptions result

-- Plot vertical lines, adapted from Chart.vlinePlot.
vlinesPlot :: String -> Chart.CairoLineStyle -> [a] -> Chart.Plot a b
vlinesPlot t ls xs = Chart.toPlot Chart.defaultPlotLines {
    Chart.plot_lines_title_        = t,
    Chart.plot_lines_style_        = ls,
    Chart.plot_lines_limit_values_ =
      [[(Chart.LValue v, Chart.LMin),(Chart.LValue v, Chart.LMax)] | v <- xs]
    }

-- Calculate the bounds of significance.
sigBounds :: TestOptions -> TestResult -> [Double]
sigBounds (TestOptions testType _ n pTest) (TestResult _ _ stats) =
  case testType of
    TwoTailed -> [sorted ! (nExtreme - 1), sorted ! (n - nExtreme)]
    OneTailed -> [sorted ! (n - nExtreme)]
  where
    sorted           = sortVector stats
    nExtreme         = floor $ (pVal testType pTest) * (fromIntegral n + 1) - 1
    pVal OneTailed p = p
    pVal TwoTailed p = p / 2
    -- XXX: Fix extreme cases: p-value of 0, small n.

sortVector :: (Ord a, VG.Vector v a) => v a -> v a
sortVector v = runST $ do
  s <- VG.thaw v
  VI.sort s
  VG.freeze s

-- Convenience...
opaqueBlack, opaqueGreen, opaqueRed, opaqueWhite :: Colour.AlphaColour Double
opaqueBlack = Colour.opaque ColourNames.black
opaqueGreen = Colour.opaque ColourNames.green
opaqueRed   = Colour.opaque ColourNames.red
opaqueWhite = Colour.opaque ColourNames.white
