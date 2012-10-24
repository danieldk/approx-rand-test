module CairoHistogram (
  hasCairoHistograms,
  writeHistogram
) where

import           System.IO (hPutStrLn, stderr)

hasCairoHistograms :: Bool
hasCairoHistograms = False

-- Stub function, for when we did not compile with Cairo support.
writeHistogram :: a -> Int -> b -> c -> IO ()
writeHistogram _ _ _ _ = do
  hPutStrLn stderr "This program was built without Cairo support, cannot make a histogram."

