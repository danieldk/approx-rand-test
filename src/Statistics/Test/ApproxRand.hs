-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : BSD3
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- This module provides functionality to perform approximate randomization
-- tests (Noreen, 1989).


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module Statistics.Test.ApproxRand (
  -- * Description
  -- $description

  -- * Examples
  -- $examples

  -- * Data types
  TestOptions(..),
  TestResult(..),
  Significance(..),
  RandWithError,

  -- * Approximate randomization tests
  approxRandTest,
  approxRandStats,

  approxRandPairTest,
  approxRandPairStats,

  -- * Test statistics
  TestStatistic,
  differenceMean,
  meanDifference,
  varianceRatio
) where

import           Prelude hiding ((++))
import           Control.Monad (liftM, replicateM, when)
import           Control.Monad.Error (ErrorT)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Mersenne.Random (R(..), Rand(..), getBool)
import           Control.Monad.ST (runST)
import           Control.Monad.Trans.Class (lift)
import           Data.Vector.Generic ((++))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Word (Word)
import           Statistics.Sample (variance)
import           Statistics.Test.Types (TestType(..))
import           Statistics.Types
import           System.Random.Mersenne.Pure64 (PureMT, randomInt, randomWord)


-- $description
--
-- Approximate randomization tests rely on a simple premise: given a test
-- statistic, if the null-hypothesis (the samples do not differ) is true,
-- we can randomly swap values between samples without an (extreme) impact
-- on the test statistic. Otherwise, the null-hypothesis must be rejected.
--
-- The test works by generating a given number of sample shuffles and computing
-- the test statistic for each shuffle. If /r/ is the number of shuffled
-- samples where the test statistic is at least as high as the test statistic
-- applied on the original samples; and /N/ the number of shuffles, then
-- the null-hypothesis is rejected iff /(r + 1):(N + 1) < p-value/ (for
-- one-sided tests).
--
-- Two kinds of test are supported:
--
-- * /Paired sample/ ('approxRandPairTest'): values from samples are shuffled
--   pair-wise. This requires the samples to have an equal length.
--
-- * /Unpaired sample/ ('approxRandTest'): values from samples are shuffled
--   among both samples. Consequently the i-th element of one sample does not
--   bear a relationship with the i-th element of the other sample. The
--   shuffled samples retain the sizes of the original samples.
--
-- Both tests can be performed as a one-tailed or two-tailed test.

-- $examples
-- Both unpaired and paired sample tests use the 'Rand' monad to obtain
-- random numbers. We can obtain a pseudo-random number generator that
-- is seeded using the system clock using the
-- 'System.Random.Mersenne.Pure64.newPureMT' function (please refer to
-- the documentation of 'System.Random.Mersenne.Pure64' for more
-- information):
--
-- > prng <- newPureMT
--
-- Suppose that we have the samples 's1' and 's2'. We could now perform
-- a Two-Tailed randomization test with 10,000 shuffles and the mean
-- difference as the test statistic, by running 'approxRandTest' in the 'Rand'
-- monad (at the /p = 0.01/ level):
--
-- > evalRandom (approxRandTest TwoTailed meanDifference 10000 0.01 s1 s2) prng
--
-- It is also possible to obtain the test statistics of the shuffled samples
-- directly (e.g. to inspect the distribution of test statistics) using the
-- 'approxRandStats'/'approxRandPiarStats' functions:
--
-- > evalRandom (approxRandStats meanDifference 10000 0.01 s1 s2) prng

-- | Computations with random numbers that can fail.
type RandWithError a = ErrorT String Rand a

-- | Options for randomization tests
--
data TestOptions = TestOptions {
    toTestType      :: TestType,      -- ^ Type of test ('OneTailed' or 'TwoTailed')
    toTestStatistic :: TestStatistic, -- ^ Test statistic
    toIterations    :: Int,           -- ^ Number of shuffled samples to create
    toPValue        :: Double         -- ^ he p-value at which to test (e.g. 0.05)
}

-- |
-- The result of hypothesis testing.
data TestResult = TestResult {
    trSignificance     :: Significance, -- ^ Significance
    trStat             :: Double,       -- ^ Test statistic for the samples
    trRandomizedStats  :: Sample        -- ^ Test statistics for the
                                        --   randomized samples
  } deriving (Eq, Ord, Show)

-- |
-- Significance.
data Significance =
    Significant    Double -- ^ The null hypothesis should be rejected
  | NotSignificant Double -- ^ Data is compatible with the null hypothesis
  deriving (Eq, Ord, Show)

-- |
-- Apply a pair-wise approximate randomization test.
--
-- In pair-wise approximate randomization tests the data points at a given
-- index are swapped between samples with a probability of 0.5. Since
-- swapping is pairwise, the samples should have the same length.
approxRandPairTest ::
     TestOptions              -- ^ Options for the test
  -> Sample                   -- ^ First sample
  -> Sample                   -- ^ Second sample
  -> RandWithError TestResult -- ^ The test result
approxRandPairTest (TestOptions testType stat n pTest) s1 s2 = do
  stats <- approxRandPairStats stat n s1 s2
  let tOrig = stat s1 s2
  let sig = significance testType pTest n $ countExtremes tOrig $ stats
  return $ TestResult sig tOrig stats

-- |
-- Apply an approximate randomization test.
--
-- In approximate randomization tests, the values of two samples are
-- shuffled among those samples. A test statistic is calculated for
-- the original samples and the shuffled samples, to detect whether the
-- difference of the samples is extreme or not.
approxRandTest ::
     TestOptions     -- ^ Options for the test
  -> Sample          -- ^ First sample
  -> Sample          -- ^ Second sample
  -> Rand TestResult -- ^ The test result
approxRandTest (TestOptions testType stat n pTest) s1 s2 = do
  stats <- approxRandStats stat n s1 s2
  let tOrig = stat s1 s2
  let sig = significance testType pTest n $ countExtremes tOrig stats
  return $ TestResult sig tOrig stats

-- | Determine the significance.
significance ::
     TestType     -- ^ Type of test ('OneTailed' or 'TwoTailed')
  -> Double       -- ^ The p-value at which to test (e.g. 0.05)
  -> Int          -- ^ Number of sample shuffles
  -> (Int, Int)   -- ^ Extreme statistic counts
  -> Significance -- ^ The test result
significance TwoTailed pTest n =
  significant (pTest / 2) . pValue n . uncurry min
significance OneTailed pTest n =
  significant pTest . pValue n . snd

-- | Wrap a p-value in a 'TestResult'.
significant ::
     Double       -- ^ The p-value at which to test
  -> Double       -- ^ The p-value
  -> Significance -- ^ The test result
significant pTail p =
  if p < pTail then Significant p else NotSignificant p

-- | Calculate a p-value
pValue ::
     Int    -- ^ Number of extreme outcomes
  -> Int    -- ^ Number of shuffles
  -> Double -- ^ The p-value
pValue n r = (fromIntegral r + 1) / (fromIntegral n + 1)

-- |
-- Count extreme test statistic values. If the test statistic value of the
-- original sample is in the right tail, we want to count values equal to
-- or larger than that value. If the value is in the left tail, we want to
-- count value smaller than or equal to that value. Since we do not know
-- the tail (yet), we count both.
--
-- Note: we can determine the tail by (1) averaging the test statistics of
-- the randomized samples, or (2) taking the smaller of the two counts.
countExtremes ::
     Double     -- ^ Test statistic value of the original samples
  -> Sample     -- ^ Test statistic values of the randomized samples.
  -> (Int, Int) -- ^ Count of left- and right-tail extremes.
countExtremes tOrig =
  VG.foldl' count (0, 0)
  where
    count (left, right) tPerm =
      let !newLeft = if tPerm <= tOrig then succ left else left in
      let !newRight = if tPerm >= tOrig then succ right else right in
        (newLeft, newRight)

-- |
-- Generate a given number of pairwise shuffled samples, and calculate
-- the test statistic for each shuffle.
--
-- Since the data points at a given index are swapped (with a probability of
-- 0.5), the samples should have the same length.
approxRandPairStats ::
     TestStatistic          -- ^ Test statistic
  -> Int                    -- ^ Number of shuffled samples to create
  -> Sample                 -- ^ First sample
  -> Sample                 -- ^ Second sample
  -> RandWithError Sample   -- ^ The statistics of the shuffles
approxRandPairStats stat n s1 s2 = do
  when (VG.length s1 /= VG.length s2) $
    throwError "Cannot calculate pairwise statistic: samples have different sizes"
  lift $ liftM VG.fromList $ replicateM n $
    uncurry stat `liftM` shuffleVectorsPairwise s1 s2

-- |
-- Generate a given number of shuffled samples, and calculate the test
-- statistic for each shuffle.
--
-- This function does not require the samples to have an equal length.
approxRandStats ::
     TestStatistic -- ^ Test statistic
  -> Int           -- ^ Number of shuffled samples to create
  -> Sample        -- ^ First sample
  -> Sample        -- ^ Second sample
  -> Rand Sample   -- ^ The statistics of the shuffles
approxRandStats stat n s1 s2 =
  liftM VG.fromList $ replicateM n $ uncurry stat `liftM` shuffleVectors s1 s2

-- | Pair-wise shuffle of two vectors.
shuffleVectorsPairwise :: (VG.Vector v a, VG.Vector v Bool) =>
  v a -> v a -> Rand (v a, v a)
shuffleVectorsPairwise vec1 vec2 = do
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
  VG.sum (subVector v1 v2) / fromIntegral (VG.length v1)

-- | Calculates the mean difference of two samples (/mean(s1) - mean(s2)/).
meanDifference :: TestStatistic
meanDifference s1 s2 =
  mean s1 - mean s2

-- | Calculate the mean of a sample.
mean :: Sample -> Double
mean = do
  t <- VG.sum
  l <- VG.length
  return $ t / fromIntegral l

-- | Calculate the ratio of sample variances (/var(s1) : var(s2)/).
varianceRatio :: TestStatistic
varianceRatio v1 v2 =
  variance v1 / variance v2

-- | Subtract two vectors.
subVector :: (VG.Vector v n, Num n) => v n -> v n -> v n
subVector = VG.zipWith (-)

subIIW :: Int -> Int -> Word
subIIW a b = fromIntegral a - fromIntegral b
{-# INLINE subIIW #-}

addIWI :: Int -> Word -> Int
addIWI a b = a + fromIntegral b
{-# INLINE addIWI #-}

-- | Generate Int numbers within a range
randomIntR :: PureMT -> (Int, Int) -> (Int, PureMT)
randomIntR gen (a, b)
  | n == 0    = randomInt gen
  | otherwise = loop gen
  where
    (a', b') = if a < b then (a, b) else (b, a)
    -- Number of different Ints that should be generated
    n = 1 + subIIW b' a'
    -- The total range of Word can hold x complete n ranges
    x = maxBound `div` n
    -- Pick from a range the is dividable by n without remainders
    s = x * n
    loop gen'
      | r >= s    = loop gen'' -- r is outside the range, discard it...
      | otherwise = (addIWI a' (r `div` x), gen'') 
      where
        (!r, !gen'') = randomWord gen'
{-# INLINE randomIntR #-}
