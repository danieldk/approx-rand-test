module CairoHistogram (
  hasCairoHistograms,
  writeHistogram
) where

import           Data.Accessor ((^=), (^:))
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as ColourNames
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
import qualified Graphics.Rendering.Chart as Chart
import qualified Statistics.Sample.Histogram as SSH
import           Statistics.Test.ApproxRand
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

hasCairoHistograms :: Bool
hasCairoHistograms = True

writeHistogram :: Int -> TestResult -> FP.FilePath -> IO ()
writeHistogram bins result path = do
  case snd $ FP.splitExtension path of
    ".pdf" -> Chart.renderableToPDFFile (createHistogram bins result) 800 600 path
    ".png" -> do
      _ <- Chart.renderableToPNGFile (createHistogram bins result) 800 600 path
      return ()
    ".ps"  -> Chart.renderableToPSFile  (createHistogram bins result) 800 600 path
    ".svg" -> Chart.renderableToSVGFile (createHistogram bins result) 800 600 path
    _      -> hPutStrLn stderr "Unknown output format!"

-- Returns the (x,y) points. If the test statistic for one of the original
-- samples is in one of the bins, we make two y's for that bin: one is
-- empty, the other has the frequency of that bin.
dataPoints :: Int -> TestResult -> [(Double, [Int])]
dataPoints bins (TestResult _ stat randomizedStats) =
  map zeroInRange $ VG.toList $ VG.zip ticks numSamples
  where
    (lowerBounds, numSamples) =
      SSH.histogram bins randomizedStats

    -- Compute the middle values of the bins.
    bucketSize = (lowerBounds ! 1) - (lowerBounds ! 0)
    bucketHalf = bucketSize / 2
    ticks = VG.map ((+) bucketHalf) lowerBounds

    -- Mark the bin with the original statistic for the samples.
    inRange t = stat >= t - bucketHalf && stat < t + bucketHalf
    zeroInRange (t, f) = if inRange t then (t, [0, f]) else (t, [f])

-- Creates a histogram. The histogram is stacked, but the second bar
-- is always empty, except for the bin of the original statistic (if any).
-- There, the first bar is empty and the second bar has the frequency.
-- Yes, this is cheating ;).
createHistogram :: Int -> TestResult -> Chart.Renderable ()
createHistogram bins result =
  Chart.toRenderable layout
  where
    layout =
        Chart.layout1_background  ^= Chart.solidFillStyle (Colour.opaque ColourNames.white)
      $ Chart.layout1_left_axis   ^: Chart.laxis_override ^= Chart.axisTicksHide
      $ Chart.layout1_right_axis  ^: Chart.laxis_title    ^= "Frequency"
      $ Chart.layout1_bottom_axis ^: Chart.laxis_title    ^= "Statistic"
      $ Chart.layout1_plots       ^= [ Right $ Chart.plotBars randomizationBars ]
      $ Chart.setLayout1Foreground   (Colour.opaque ColourNames.black)
      $ Chart.defaultLayout1
    randomizationBars =
        Chart.plot_bars_style       ^= Chart.BarsStacked
      $ Chart.plot_bars_spacing     ^= Chart.BarsFixGap 6 2
      $ Chart.plot_bars_item_styles ^= [
          (Chart.solidFillStyle $ Colour.opaque ColourNames.green, Nothing),
          (Chart.solidFillStyle $ Colour.opaque ColourNames.red, Nothing) ]
      $ Chart.plot_bars_values      ^= dataPoints bins result
      $ Chart.defaultPlotBars
