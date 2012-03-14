-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : BSD3
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- Approximate randomization test (Noreen, 1989)
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module Statistics.Test.ApproxRand (
  -- * Approximate randomization tests
  approxRandPairTest,
  approxRandPairScores,

  -- * Test statistics
  TestStatistic,
  meanDifference,
  varianceRatio,

  -- * Data types
  TestResult(..)
) where

import           Control.Monad (liftM, replicateM)
import           Control.Monad.Random.Class (MonadRandom(..))
import           Data.List (foldl')
import qualified Data.Vector.Generic as VG
import           Statistics.Sample (variance)
import           Statistics.Test.Types (TestType(..))
import           Statistics.Types
import           System.Random (Random)

-- |
-- The result of hypothesis testing.
data TestResult =
    Significant Double     -- ^ The null hypothesis should be rejected
  | NotSignificant Double  -- ^ Data is compatible with the null hypothesis
  deriving (Eq, Ord, Show)

-- |
-- Apply a pair-wise approximate randomization test.
--
-- In pair-wise approximate randomization tests the scores at a given
-- index are swapped between samples with a probability of 0.5. Since
-- swapping is pairwise, the samples should have the same length.
approxRandPairTest :: MonadRandom r =>
     TestType      -- ^ Type of test ('OneTailed' or 'TwoTailed')
  -> TestStatistic -- ^ Test statistic
  -> Int           -- ^ Number of sample permutations to create
  -> Double        -- ^ The p-value at which to test (e.g. 0.05)
  -> Sample        -- ^ First sample
  -> Sample        -- ^ Second sample
  -> r TestResult  -- ^ The test result
approxRandPairTest testType stat n pTest s1 s2 =
  (significance testType pTest n . countExtremes tOrig) `liftM`
    approxRandPairScores stat n s1 s2
  where
    tOrig = stat s1 s2

significance :: TestType -> Double -> Int -> (Int, Int) -> TestResult
significance TwoTailed pTest n (leftR, rightR) =
  if (pRight < pTail) then
    Significant pRight
  else if (pLeft < pTail) then
    Significant pLeft
  else
    NotSignificant $ min pLeft pRight
  where
    pTail = pTest / 2.0 -- Allowed p-value per tail
    pLeft = pValue leftR n
    pRight = pValue rightR n
significance OneTailed pTest n (_, rightR)     =
  if (pRight < pTest) then
    Significant pRight
  else
    NotSignificant pRight
  where
    pRight = pValue rightR n

pValue :: Int -> Int -> Double
pValue r n = ((fromIntegral r) + 1.0) / ((fromIntegral n) + 1.0)

countExtremes ::
     Double
  -> [Double]
  -> (Int, Int)
countExtremes tOrig =
  foldl' count (0, 0)
  where
    count (left, right) tPerm =
      let !newLeft = if tPerm <= tOrig then succ left else left in
      let !newRight = if tPerm >= tOrig then succ right else right in
        (newLeft, newRight)

-- |
-- Generate a given number of pairwise sample permutations, and calculate
-- the test score for each permutation.
--
-- Since the scores at a given index are swapped (with a probability of
-- 0.5), the samples should have the same length.
approxRandPairScores :: (MonadRandom r) =>
     TestStatistic -- ^ Test statistic
  -> Int           -- ^ Number of sample permutations to create
  -> Sample        -- ^ First sample
  -> Sample        -- ^ Second sample
  -> r [Double]    -- ^ The scores of each permutation
approxRandPairScores stat n s1 s2 =
  replicateM n $ (uncurry stat) `liftM` permuteVectors s1 s2

-- | Subtract two vectors.
subVector :: (VG.Vector v n, Num n) => v n -> v n -> v n
subVector = VG.zipWith (-)

randomVector :: (MonadRandom r, Random a, VG.Vector v a) => Int -> r (v a)
randomVector len =
  VG.fromList `liftM` take len `liftM` getRandoms

-- | Permute two vectors.
permuteVectors :: (MonadRandom r, VG.Vector v a, VG.Vector v Bool) =>
  v a -> v a -> r (v a, v a)
permuteVectors vec1 vec2 = do
  randomVec <- randomVector (VG.length vec1)
  let pv1 = VG.zipWith3 permute vec1 vec2 randomVec
  let pv2 = VG.zipWith3 permute vec2 vec1 randomVec
  return (pv1, pv2)
  where
    permute val1 val2 coin =
      if coin then val1 else val2

-- |
-- A test stastic calculates the difference between two samples. See
-- 'meanDifference' and 'varianceRatio' for examples.
type TestStatistic = Sample -> Sample -> Double

-- |
-- Calculates the mean difference of two samples (/mean(s1 - s2)/).
meanDifference :: TestStatistic
meanDifference v1 v2 =
  (/ (fromIntegral $ VG.length v1)) $ VG.sum $ subVector v1 v2

-- |
-- Calculate the ratio of sample variances (/var(s1) : var(s2)/)
varianceRatio :: TestStatistic
varianceRatio v1 v2 =
  (variance v1) / (variance v2)

