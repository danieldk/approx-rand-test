module ChartBackend (
  backendFormats,
  writeWithBackend
) where

import           Control.Lens.Setter ((.~))
import           Control.Monad (void)
import qualified Data.Map.Lazy as Map
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as DiagramsChart
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

backendFormats :: [String]
backendFormats = ["eps", "svg"]

type RenderFunction a = Chart.Renderable a -> FilePath -> IO (Chart.PickFn a)

defaultFileOptions :: DiagramsChart.FileOptions
defaultFileOptions = DiagramsChart.FileOptions (800, 600) DiagramsChart.SVG Map.empty

renderFunctions :: [(String, RenderFunction a)]
renderFunctions =
  [ (".eps", DiagramsChart.renderableToFile $
      (DiagramsChart.fo_format .~ DiagramsChart.EPS) defaultFileOptions),
    (".svg", DiagramsChart.renderableToFile defaultFileOptions) ]
  

writeWithBackend :: Chart.Renderable () -> FP.FilePath -> IO ()
writeWithBackend renderable path =
  case lookup (snd $ FP.splitExtension path) renderFunctions of
    Just f  -> void $ f renderable path
    Nothing ->
      hPutStrLn stderr "Unknown output format!"

