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
{-# LANGUAGE UnboxedTuples #-}

module Statistics.Test.ApproxRand (
  -- * Approximate randomization tests
  approxRandTest,
  approxRandScores,

  approxRandPairTest,
  approxRandPairScores,

  -- * Test statistics
  TestStatistic,
  differenceMean,
  meanDifference,
  varianceRatio,

  -- * Data types
  TestResult(..),
  RandWithError
) where

import           Prelude hiding ((++))
import           Control.Monad (liftM, replicateM, when)
import           Control.Monad.Error (ErrorT)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Mersenne.Random (R(..), Rand(..), getBool)
import           Control.Monad.ST (runST)
import           Control.Monad.Trans.Class (lift)
import           Data.List (foldl')
import           Data.Vector.Generic ((++))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Word (Word)
import           Statistics.Sample (variance)
import           Statistics.Test.Types (TestType(..))
import           Statistics.Types
import           System.Random.Mersenne.Pure64 (PureMT, randomInt, randomWord)

-- | Computations with random numbers that can fail.
type RandWithError a = ErrorT String Rand a

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
approxRandPairTest ::
     TestType                 -- ^ Type of test ('OneTailed' or 'TwoTailed')
  -> TestStatistic            -- ^ Test statistic
  -> Int                      -- ^ Number of sample permutations to create
  -> Double                   -- ^ The p-value at which to test (e.g. 0.05)
  -> Sample                   -- ^ First sample
  -> Sample                   -- ^ Second sample
  -> RandWithError TestResult -- ^ The test result
approxRandPairTest testType stat n pTest s1 s2 =
  (significance testType pTest n . countExtremes tOrig) `liftM`
    approxRandPairScores stat n s1 s2
  where
    tOrig = stat s1 s2

-- |
-- Apply an approximate randomization test.
--
-- In approximate randomization tests, the values of two samples are
-- shuffled among those samples. A test statistic is calculated for
-- the original samples and the permutations, to detect whether the
-- difference of the samples is extreme or not.
approxRandTest ::
     TestType
  -> TestStatistic
  -> Int
  -> Double
  -> Sample
  -> Sample
  -> Rand TestResult
approxRandTest testType stat n pTest s1 s2 =
  (significance testType pTest n . countExtremes tOrig) `liftM`
    approxRandScores stat n s1 s2
  where
    tOrig = stat s1 s2

significance :: TestType -> Double -> Int -> (Int, Int) -> TestResult
significance TwoTailed pTest n =
  significant (pTest / 2) . pValue n . (uncurry min) 
significance OneTailed pTest n =
  significant pTest . pValue n . snd

significant :: Double -> Double -> TestResult
significant pTail p =
  if p < pTail then Significant p else NotSignificant p

pValue :: Int -> Int -> Double
pValue n r = ((fromIntegral r) + 1) / ((fromIntegral n) + 1)

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
approxRandPairScores ::
     TestStatistic          -- ^ Test statistic
  -> Int                    -- ^ Number of sample permutations to create
  -> Sample                 -- ^ First sample
  -> Sample                 -- ^ Second sample
  -> RandWithError [Double] -- ^ The scores of each permutation
approxRandPairScores stat n s1 s2 = do
  when (VG.length s1 /= VG.length s2) $
    throwError "Cannot calculate pairwise scores: samples have different sizes"
  lift $ replicateM n $ (uncurry stat) `liftM` permuteVectors s1 s2

-- |
-- Generate a given number of sample permutations, and calculate the test
-- score for each permutation.
--
-- This function does not require the samples to have an equal length.
approxRandScores ::
     TestStatistic
  -> Int
  -> Sample
  -> Sample
  -> Rand [Double]
approxRandScores stat n s1 s2 =
  replicateM n $ uncurry stat `liftM` shuffleVectors s1 s2

-- | Permute two vectors.
permuteVectors :: (VG.Vector v a, VG.Vector v Bool) =>
  v a -> v a -> Rand (v a, v a)
permuteVectors vec1 vec2 = do
  randomVec <- randomVector (VG.length vec1)
  let pv1 = VG.zipWith3 permute vec1 vec2 randomVec
  let pv2 = VG.zipWith3 permute vec2 vec1 randomVec
  return (pv1, pv2)
  where
    permute val1 val2 coin =
      if coin then val1 else val2

randomVector :: (VG.Vector v Bool) => Int -> Rand (v Bool)
randomVector len =
  VG.replicateM len getBool

-- Shuffle values amongst two vectors, keeping the original vector lengths.
shuffleVectors :: VG.Vector v a => v a -> v a -> Rand (v a, v a)
shuffleVectors v1 v2 = do
  shuffledVectors <- shuffleVector $ v1 ++ v2
  return (VG.slice 0 (VG.length v1) shuffledVectors,
    VG.slice (VG.length v1) (VG.length v2) shuffledVectors)

-- Fisher-Yates shuffle in the Rand monad
shuffleVector :: VG.Vector v a => v a -> Rand (v a)
shuffleVector v =
  Rand $ \s -> case shuffleVector' s v of (sv, s') -> R sv s'

-- Fisher-Yates shuffle
shuffleVector' :: VG.Vector v a => PureMT -> v a -> (v a, PureMT)
shuffleVector' gen v = runST $ do
  let maxIdx = VG.length v - 1
  vm   <- VG.thaw v
  gen' <- swaps vm 0 maxIdx gen
  vmf  <- VG.unsafeFreeze vm
  return (vmf, gen')
  where
    swaps vm idx maxIdx gen'
      | idx < maxIdx = do
          let (newIdx, gen'') = randomIntR gen' (idx, maxIdx)
          GM.unsafeSwap vm idx newIdx
          swaps vm (idx + 1) maxIdx gen''
      | otherwise = return gen'

-- |
-- A test stastic calculates the difference between two samples. See
-- 'meanDifference' and 'varianceRatio' for examples.
type TestStatistic = Sample -> Sample -> Double

-- |
-- Calculates the difference mean of two samples (/mean(s1 - s2)/). When the
-- two samples do not have an equal length, the trailing elements of the
-- longer vector are ignored.
differenceMean :: TestStatistic
differenceMean v1 v2 =
  (VG.sum $ subVector v1 v2) / (fromIntegral $ VG.length v1)

-- |
-- Calculates the mean difference of two samples (/mean(s1) - mean(s2)/).
meanDifference :: TestStatistic
meanDifference s1 s2 =
  (mean s1) - (mean s2)

mean :: Sample -> Double
mean = do
  t <- VG.sum
  l <- VG.length
  return $ t / fromIntegral l

-- |
-- Calculate the ratio of sample variances (/var(s1) : var(s2)/)
varianceRatio :: TestStatistic
varianceRatio v1 v2 =
  (variance v1) / (variance v2)

-- | Subtract two vectors.
subVector :: (VG.Vector v n, Num n) => v n -> v n -> v n
subVector = VG.zipWith (-)

-- Generate Int numbers within a range

subIIW :: Int -> Int -> Word
subIIW a b = fromIntegral a - fromIntegral b
{-# INLINE subIIW #-}

addIWI :: Int -> Word -> Int
addIWI a b = a + fromIntegral b
{-# INLINE addIWI #-}

randomIntR :: PureMT -> (Int, Int) -> (Int, PureMT)
randomIntR gen (a, b)
  | n == 0    = randomInt gen
  | otherwise = loop gen
  where
    (# a', b' #) = if a < b then (# a, b #) else (# b, a #)
    -- Number of different Ints that should be generated
    n = 1 + subIIW b' a'
    -- The total range of Word can hold x complete n ranges
    x = (maxBound `div` n)
    -- Pick from a range the is dividable by n without remainders
    s = x * n
    loop gen'
      | r >= s    = loop gen'' -- r is outside the range, discard it...
      | otherwise = (addIWI a' (r `div` x), gen'') 
      where
        (!r, !gen'') = randomWord gen'
{-# INLINE randomIntR #-}
