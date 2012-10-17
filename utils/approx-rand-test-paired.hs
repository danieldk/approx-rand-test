-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : Apache 2
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- Approximate randomization test (Noreen, 1989)

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import           Control.Monad (liftM, when)
import           Control.Monad.Error (runErrorT)
import           Control.Monad.Mersenne.Random (evalRandom)
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)
import           Statistics.Test.ApproxRand
import           Statistics.Test.Types (TestType(..))
import           Statistics.Types (Sample)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.Random.Mersenne.Pure64 (PureMT, newPureMT, pureMT)
import           Text.Printf (printf)

import           SampleIO

main :: IO ()
main = do
  -- Read command-line options and arguments.
  (opts, args) <- getOptions

  -- Read score files
  let col = pred $ optColumn opts
  v1 <- liftM V.fromList $ readFileCol (head args) col
  v2 <- liftM V.fromList $ readFileCol (args !! 1) col

  let stat = optTestStatistic opts

  prng <- case optPRNGSeed opts of
    Just seed -> return $ pureMT seed
    Nothing   -> newPureMT

  if optPrintScores opts then
    printScores opts stat prng v1 v2
  else
    applyTest opts stat prng v1 v2


applyTest :: Options -> TestStatistic -> PureMT -> Sample ->
  Sample -> IO ()
applyTest opts stat prng v1 v2 = do
  putStrLn $ printf "Iterations: %d" $ optIterations opts
  putStrLn $ printf "Sample size: %d" $ V.length v1

  -- Calculate test statistic for original score sets.
  let tOrig = stat v1 v2
  putStrLn $ printf "Test statistic: %f" tOrig

  let testType = optTestType opts

  let pTest = optSigP opts
  let pTail = case testType of
                OneTailed -> pTest
                TwoTailed -> pTest / 2

  -- Test information
  putStrLn $ "Test type: " ++ show testType
  putStrLn $ printf "Test significance: %f" pTest
  putStrLn $ printf "Tail significance: %f" pTail

  -- Approximate randomization testing.
  let test = runErrorT $ approxRandPairTest testType stat (optIterations opts) pTest v1 v2
  let result = evalRandom test prng
  case result of
    Left  err                -> putStrLn err
    Right (TestResult Significant    p) -> putStrLn $ printf "Significant: %f" p
    Right (TestResult NotSignificant p) -> putStrLn $ printf "Not significant: %f" p

printScores :: Options -> TestStatistic -> PureMT -> Sample ->
  Sample -> IO ()
printScores opts stat prng v1 v2 = do
  let test = runErrorT $ approxRandPairScores stat (optIterations opts) v1 v2
  case evalRandom test prng of
    Left err     -> putStrLn err
    Right scores -> mapM_ (putStrLn . printf "%f") scores

data Options = Options {
  optColumn        :: Int,
  optIterations    :: Int,
  optPRNGSeed      :: Maybe Word64,
  optPrintScores   :: Bool,
  optSigP          :: Double,
  optTestStatistic :: TestStatistic,
  optTestType      :: TestType
}

defaultOptions :: Options
defaultOptions = Options {
  optColumn        = 1,
  optIterations    = 10000,
  optPRNGSeed      = Nothing,
  optPrintScores   = False,
  optSigP          = 0.01,
  optTestStatistic = differenceMean,
  optTestType      = TwoTailed
}

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["column"]
      (ReqArg (\arg opt -> opt { optColumn = read arg }) "NUMBER")
      "column number (starting at 1)",
    Option ['i'] ["iterations"]
      (ReqArg (\arg opt -> opt { optIterations = read arg }) "NUMBER")
      "number of iterations",
    Option ['o'] ["one-tailed"]
      (NoArg (\opt -> opt { optTestType = OneTailed }))
      "perform a one-tailed test",
    Option ['p'] []
      (ReqArg (\arg opt -> opt {optSigP = read arg }) "NUMBER")
      "significant p-value",
    Option []    ["print-scores"]
      (NoArg (\opt -> opt { optPrintScores = True }))
      "output scores of permuted vectors",
    Option ['s'] ["seed"]
      (ReqArg (\arg opt -> opt { optPRNGSeed = Just $ read arg}) "NUMBER")
      "pseudorandom number generator seed",
    Option ['t'] ["test-statistic"]
      (ReqArg (\arg opt -> opt { optTestStatistic = parseStatistic arg}) "NAME")
      "test statistic (mean_diff, var_ratio)"
  ]

getOptions :: IO (Options, [String])
getOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (actions, nonOpts, [])   -> do
      when (length nonOpts /= 2) usageExit
      let opts = foldl (flip ($)) defaultOptions actions
      return (opts, nonOpts)
    (_,    _,          _)    ->
      usageExit
  where
  usageExit = do
    putStrLn $ usageInfo header options
    exitFailure
    where
      header = "Usage: approx-rand-test [OPTION...] scores scores2"

parseStatistic :: String -> TestStatistic
parseStatistic "mean_diff" = differenceMean
parseStatistic "var_ratio" = varianceRatio
parseStatistic _           = error "Unknown test statistic"
