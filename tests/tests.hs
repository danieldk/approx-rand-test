module Main where

import Control.Monad.Mersenne.Random (evalRandom)
import System.Random.Mersenne.Pure64 (pureMT)
import qualified Data.Vector.Unboxed as V
import Statistics.Test.ApproxRand
import Test.HUnit (assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

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
	testEquality "mean difference robot competition"
		(meanDifference cohenRobotsAlpha cohenRobotsBeta) 1.8

-- Approximate andomization tests

randomizationTests :: [Test]
randomizationTests = [pairApproxExactTestScores]

pairApproxExactTestScores :: Test
pairApproxExactTestScores =
	testEquality  "number of extreme values robot competition"
		(length $ filter (>= 1.8) scores) 21
	where
		scores = evalRandom
			(approxRandPairScores meanDifference 1024
				cohenRobotsAlpha cohenRobotsBeta) $
			pureMT 42

-- Helper functions

testEquality :: (Show a, Eq a) => String -> a -> a -> Test
testEquality msg a b = testCase msg $ assertEqual msg a b

-- Example from Cohen, 1995
cohenRobotsAlpha :: V.Vector Double
cohenRobotsAlpha = V.fromList [8,3,9,6,5,8,7,8,9,9]

cohenRobotsBeta :: V.Vector Double
cohenRobotsBeta  = V.fromList [7,0,9,4,5,9,8,3,4,5]
