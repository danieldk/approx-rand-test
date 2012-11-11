module TextHistogram (
  printHistogram,
) where

import           Control.Monad (forM_, when)
import           Statistics.Test.ApproxRand
import           System.IO (hPutStrLn, stderr)
import           Text.Printf (printf)

import           Histogram

printHistogram :: Int -> TestResult -> IO ()
printHistogram bins result@(TestResult _ score _) =
  case histogram bins result of
    Left err   -> hPutStrLn stderr err
    Right hist ->
      let bucketSize = (fst $ hist !! 1) - (fst $ head hist)
          bucketHalf = bucketSize / 2
          charsPerDot = (maximum $ map snd hist) `div` 50 in
            forM_ hist $ \(label, freq) -> do
              let blocks = freq `div` charsPerDot
              when (blocks > 0) $ do
                let lower = label - bucketHalf
                let barChar = if score >= lower && score < lower + bucketSize then
                            '✣'
                          else
                            '█'
                putStr $ printf "%12.3e | " $ lower + bucketHalf
                putStrLn $ replicate blocks barChar

