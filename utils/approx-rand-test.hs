-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : Apache 2
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- Approximate randomization test (Noreen, 1989)

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}

import           Control.Exception.Base (Exception)
import           Control.Monad (liftM, when)
import           Control.Monad.Error (runErrorT)
import           Control.Monad.Mersenne.Random (evalRandom)
import           Control.Monad.Trans.Resource (ResourceThrow (..))
import           Data.Conduit (($$), ($=)) 
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Typeable (Typeable)
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

data ReadException =
  DoubleConversionException String
  deriving (Show, Typeable)

instance Exception ReadException

readFileCol :: C.ResourceIO m => String -> Int -> m [Double]
readFileCol fn col =
  liftM reverse $ C.runResourceT (
    CB.sourceFile fn $=
    CB.lines $=
    CT.decode CT.utf8 $=
    CL.map (T.split (== ' ')) $=
    CL.map (!! col) $=
    toDouble $$
    CL.consume )

toDouble :: ResourceThrow m => C.Conduit T.Text m Double
toDouble = CL.mapM $ \v ->
  case TR.double v of
    Left err     -> resourceThrow $ DoubleConversionException err
    Right (d, _) -> return $ d

main :: IO ()
main = do
  -- Read command-line options and arguments.
  (opts, args) <- getOptions

  -- Read score files
  let col = pred $ optColumn opts
  v1 <- liftM V.fromList $ readFileCol (args !! 0) col
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
                TwoTailed -> pTest / 2.0

  -- Test information
  putStrLn $ "Test type: " ++ show testType
  putStrLn $ printf "Test significance: %f" $ pTest
  putStrLn $ printf "Tail significance: %f" $ pTail

  -- Approximate randomization testing.
  let test = runErrorT $ approxRandPairTest testType stat (optIterations opts) pTest v1 v2
  let result = evalRandom test prng
  case result of
    Left  err                -> putStrLn err
    Right (Significant    p) -> putStrLn $ printf "Significant: %f" p
    Right (NotSignificant p) -> putStrLn $ printf "Not significant: %f" p

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
  optIterations    = 1000,
  optPRNGSeed      = Nothing,
  optPrintScores   = False,
  optSigP          = 0.01,
  optTestStatistic = meanDifference,
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
parseStatistic "mean_diff" = meanDifference
parseStatistic "var_ratio" = varianceRatio
parseStatistic _           = error "Unknown test statistic"