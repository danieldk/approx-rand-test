module CairoHistogram (
  hasCairoHistograms,
  writeHistogram
) where

import           Control.Monad.ST (runST)
import           Data.Accessor ((^=), (^:))
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as ColourNames
import qualified Data.Set as Set
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
import qualified Graphics.Rendering.Chart as Chart
import qualified Statistics.Sample.Histogram as SSH
import           Statistics.Test.ApproxRand
import           Statistics.Test.Types (TestType(..))
import           Statistics.Types
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

hasCairoHistograms :: Bool
hasCairoHistograms = True

writeHistogram :: TestType -> Double -> Int -> TestResult -> FP.FilePath -> IO ()
writeHistogram testType pTest bins result path =
  if maxBins bins (trRandomizedStats result) < 2 then
    hPutStrLn stderr "Refusing to make a histogram, too few different randomization test scores!"
  else
    case snd $ FP.splitExtension path of
      ".pdf" -> Chart.renderableToPDFFile histogram 800 600 path
      ".png" -> do
        _ <- Chart.renderableToPNGFile histogram 800 600 path
        return ()
      ".ps"  -> Chart.renderableToPSFile  histogram 800 600 path
      ".svg" -> Chart.renderableToSVGFile histogram 800 600 path
      _      -> hPutStrLn stderr "Unknown output format!"
  where
    histogram = createHistogram testType pTest bins result

-- Returns the (x,y) points. If the test statistic for one of the original
-- samples is in one of the bins, we make two y's for that bin: one is
-- empty, the other has the frequency of that bin.
dataPoints :: Int -> TestResult -> [(Double, [Int])]
dataPoints bins (TestResult _ _ randomizedStats) =
  map zeroInRange $ VG.toList $ VG.zip ticks numSamples
  where
    (lowerBounds, numSamples) =
      SSH.histogram bins randomizedStats

    -- Compute the middle values of the bins.
    bucketSize = (lowerBounds ! 1) - (lowerBounds ! 0)
    bucketHalf = bucketSize / 2
    ticks = VG.map ((+) bucketHalf) lowerBounds

    -- Mark the bin with the original statistic for the samples.
    --inRange t = stat >= t - bucketHalf && stat < t + bucketHalf
    --zeroInRange (t, f) = if inRange t then (t, [0, f]) else (t, [f])
    zeroInRange (t, f) = (t, [f])

-- Creates a histogram. The histogram is stacked, but the second bar
-- is always empty, except for the bin of the original statistic (if any).
-- There, the first bar is empty and the second bar has the frequency.
-- Yes, this is cheating ;).
createHistogram :: TestType -> Double -> Int -> TestResult -> Chart.Renderable ()
createHistogram testType pTest bins result =
  Chart.toRenderable layout
  where
    layout =
        Chart.layout1_background  ^= Chart.solidFillStyle (Colour.opaque ColourNames.white)
      $ Chart.layout1_left_axis   ^: Chart.laxis_override ^= Chart.axisTicksHide
      $ Chart.layout1_right_axis  ^: Chart.laxis_title    ^= "Frequency"
      $ Chart.layout1_bottom_axis ^: Chart.laxis_title    ^= "Statistic"
      $ Chart.layout1_plots       ^= Right (Chart.plotBars randomizationBars) :
                                     Right statisticLine : sigLines
      $ Chart.setLayout1Foreground   (Colour.opaque ColourNames.black)
      $ Chart.defaultLayout1
    randomizationBars =
        Chart.plot_bars_style       ^= Chart.BarsStacked
      $ Chart.plot_bars_spacing     ^= Chart.BarsFixGap 6 2
      -- $ Chart.plot_bars_spacing     ^= Chart.BarsFixGap 0 0
      $ Chart.plot_bars_item_styles ^= [
          (Chart.solidFillStyle $ Colour.opaque ColourNames.green, Nothing),
          (Chart.solidFillStyle $ Colour.opaque ColourNames.red, Nothing) ]
      $ Chart.plot_bars_values      ^= dataPoints bins result
      $ Chart.defaultPlotBars
    statisticLine =
      Chart.vlinePlot "Statistic for samples" (Chart.solidLine 2 (Colour.opaque ColourNames.red)) $ trStat result
    sigLines      = map Right $ map sigLine $ sigBounds testType (VG.length $ trRandomizedStats result) pTest $ trRandomizedStats result
    sigLine v     =
      Chart.vlinePlot "Significance" (Chart.solidLine 2 (Colour.opaque ColourNames.black)) v

-- Calculate the bounds of significance.
sigBounds :: TestType -> Int -> Double -> Sample -> [Double]
sigBounds testType n pTest stats =
  case testType of
    TwoTailed -> [sorted ! (nExtreme - 1), sorted ! (n - nExtreme)]
    OneTailed -> [sorted ! (n - nExtreme)]
  where
    sorted           = sortVector stats
    nExtreme         = floor $ (pVal testType pTest) * (fromIntegral n + 1) - 1
    pVal OneTailed p = p
    pVal TwoTailed p = p / 2
    -- XXX: Fix extreme cases: p-value of 0, small n.

maxBins :: Int -> Sample -> Int
maxBins pref =
  min pref . Set.size . VG.foldl' (flip Set.insert) Set.empty

sortVector :: (Ord a, VG.Vector v a) => v a -> v a
sortVector v = runST $ do
  s <- VG.thaw v
  VI.sort s
  VG.freeze s
