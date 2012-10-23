module Histogram (
  drawHistogram,
  printHistogram
) where

import           Control.Monad (forM_, when)
import           Data.Monoid (mconcat)
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Option
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Statistics.Sample.Histogram as SSH
import           Statistics.Test.ApproxRand
import           Text.Printf (printf)

printHistogram :: Int -> TestResult -> IO ()
printHistogram bins (TestResult _ score randomizedStats) = do
 forM_ [0..bins - 1] $ \bin -> do
    let blocks = (numSamples ! bin) `div` charsPerDot
    when (blocks > 0) $ do
      let lower = lowerBounds ! bin
      let barChar = if score >= lower && score < lower + bucketSize then
                  '✣'
                else
                  '█'
      putStr $ printf "%12.3e | " $ lower + bucketHalf
      putStrLn $ replicate blocks barChar
  where
    (lowerBounds, numSamples) = SSH.histogram bins randomizedStats
    bucketSize = (lowerBounds ! 1) - (lowerBounds ! 0)
    bucketHalf = bucketSize / 2
    charsPerDot = (VG.maximum numSamples) `div` 50

drawHistogram :: Int -> TestResult -> IO ()
drawHistogram bins result = do
  let his = createGPHistogram bins result
  _ <- Plot.plot (SVG.cons "bla.svg") his
  return ()

createGPHistogram :: Int -> TestResult -> Frame.T (Graph2D.T Double Int)
createGPHistogram bins (TestResult _ stat randomizedStats) =
  Frame.cons (
    Opts.xLabel "Statistic" $
    Opts.yLabel "Frequency" $
    Opts.add (Option.custom "tics scale" "") ["-1,0"] $
    Opts.boxwidthRelative 0.8 $
    Opts.key False $
    OptsStyle.fillSolid $
    OptsStyle.fillBorder False $
    --OptsStyle.fillBorderLineType 1 $
    Opts.deflt) $
  mconcat $ map (\dat -> Plot2D.list Graph2D.boxes dat) [statTicks, otherTicks]-- [statTicks, otherTicks]
  where
    (lowerBounds, numSamples) = SSH.histogram bins randomizedStats
    bucketSize = (lowerBounds ! 1) - (lowerBounds ! 0)
    bucketHalf = bucketSize / 2
    ticks = VG.map ((+) bucketHalf) lowerBounds
    ticksData = VG.toList $ VG.zip ticks numSamples :: [(Double, Int)]
    inRange (t, _) = stat >= t - bucketHalf && stat < t + bucketHalf
    zeroInRange modPred p@(t, _) = if modPred $ inRange p then (t, 0) else p
    statTicks = map (zeroInRange not) ticksData
    otherTicks = map (zeroInRange id) ticksData


