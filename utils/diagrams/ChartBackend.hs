module ChartBackend (
  backendFormats,
  writeWithBackend
) where

import           Control.Monad (void)
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as DiagramsChart
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

backendFormats :: [String]
backendFormats = ["eps", "svg"]

type RenderFunction a = Chart.Renderable a -> Double -> Double -> FilePath ->
  IO (Chart.PickFn a)

renderFunctions :: [(String, RenderFunction a)]
renderFunctions =
  [ (".eps", DiagramsChart.renderableToEPSFile),
    (".svg", DiagramsChart.renderableToSVGFile) ]
  

writeWithBackend :: Chart.Renderable () -> FP.FilePath -> IO ()
writeWithBackend renderable path =
  case lookup (snd $ FP.splitExtension path) renderFunctions of
    Just f  -> void $ f renderable 800 600 path
    Nothing ->
      hPutStrLn stderr "Unknown output format!"

