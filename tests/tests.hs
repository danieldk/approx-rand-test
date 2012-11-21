module Main where

import           Control.Monad.Error (runErrorT)
import           Control.Monad.Mersenne.Random (evalRandom)
import           Numeric.IEEE (IEEE, epsilon)
import           System.Random.Mersenne.Pure64 (pureMT)
import qualified Data.Vector.Unboxed as V
import           Statistics.Test.ApproxRand
import           Test.HUnit (Assertion, assertBool, assertEqual)
import           Test.Framework
import           Test.Framework.Providers.HUnit

tests :: Test
tests = testGroup "Paired approximate randomization tests" $
  concat [statTests, randomizationTests]

main :: IO ()
main = defaultMain [ tests ]

-- Statistics tests

statTests :: [Test]
statTests = [meanDifferenceTest]

meanDifferenceTest :: Test
meanDifferenceTest =
  testIEEEEquality "mean difference robot competition"
    1.8 $ meanDifference cohenRobotsAlpha cohenRobotsBeta

-- Approximate andomization tests

randomizationTests :: [Test]
randomizationTests = [pairApproxExactTestScores]

pairApproxExactTestScores :: Test
pairApproxExactTestScores =
  testEquality  "number of extreme values robot competition"
    (Right 21) $ V.length `fmap` V.filter (>= 1.8) `fmap` scores
  where
    test = runErrorT $
      approxRandPairStats differenceMean 1024 cohenRobotsAlpha cohenRobotsBeta
    scores = evalRandom test $ pureMT 42

-- Helper functions

testEquality :: (Show a, Eq a) => String -> a -> a -> Test
testEquality msg a b = testCase msg $ assertEqual msg a b

testIEEEEquality :: IEEE a => String -> a -> a -> Test
testIEEEEquality msg a b = testCase msg $ assertEqualIEEE msg a b

assertEqualIEEE :: IEEE a => String -> a -> a -> Assertion
assertEqualIEEE msg a b = assertBool msg $ fracEq epsilon a b

-- Suggested in The Floating Point Guide: http://floating-point-gui.de/
fracEq :: (Fractional a, Ord a) => a -> a -> a -> Bool
fracEq eps a b
  | a == b     = True
  | a * b == 0 = diff < (eps * eps)
  | otherwise  = diff / (abs a + abs b) < eps
  where diff = abs (a - b)
   

-- Example from Cohen, 1995
cohenRobotsAlpha :: V.Vector Double
cohenRobotsAlpha = V.fromList [8,3,9,6,5,8,7,8,9,9]

cohenRobotsBeta :: V.Vector Double
cohenRobotsBeta  = V.fromList [7,0,9,4,5,9,8,3,4,5]
