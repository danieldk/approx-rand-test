module ChartBackend (
  backendFormats,
  writeWithBackend
) where

import           Control.Monad (void)
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as CairoChart
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

backendFormats :: [String]
backendFormats = ["pdf", "png", "ps", "svg"]

writeWithBackend :: Chart.Renderable () -> FP.FilePath -> IO ()
writeWithBackend renderable path =
  case snd $ FP.splitExtension path of
    ".pdf" -> CairoChart.renderableToPDFFile renderable 800 600 path
    ".png" -> void $ CairoChart.renderableToPNGFile renderable 800 600 path
    ".ps"  -> CairoChart.renderableToPSFile renderable 800 600 path
    ".svg" -> CairoChart.renderableToSVGFile renderable 800 600 path
    _      -> hPutStrLn stderr "Unknown output format!"

