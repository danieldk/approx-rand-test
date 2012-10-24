module TextHistogram (
  printHistogram,
) where

import           Control.Monad (forM_, when)
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
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

