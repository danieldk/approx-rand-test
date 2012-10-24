module Histogram (
  histogram
) where

import qualified Data.Set as Set
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed ((!))
import qualified Statistics.Sample.Histogram as SSH
import           Statistics.Test.ApproxRand
import           Statistics.Types (Sample)

histogram ::
     Int
  -> TestResult
  -> Either String [(Double, Int)]
histogram prefBins (TestResult _ _ randomizedStats) =
  if bins < 2 then
    Left "Cannot make a histogram, because the data cannot be divided in more than one bin."
  else
    Right $ VG.toList $ VG.zip ticks numSamples
  where
    bins = numBins prefBins randomizedStats
    (lowerBounds, numSamples) =
      SSH.histogram prefBins randomizedStats

    -- Compute the middle values of the bins.
    bucketSize = (lowerBounds ! 1) - (lowerBounds ! 0)
    bucketHalf = bucketSize / 2
    ticks = VG.map ((+) bucketHalf) lowerBounds

numBins :: Int -> Sample -> Int
numBins pref =
  min pref . Set.size . VG.foldl' (flip Set.insert) Set.empty
