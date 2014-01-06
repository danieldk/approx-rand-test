{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChartBackend (
  backendFormats,
  writeWithBackend
) where

import           Control.Lens.Setter ((.~))
import           Control.Monad (void)
import           Data.Default.Class
import           Data.Default.Instances.Containers ()
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as DiagramsChart
import qualified System.FilePath.Posix as FP
import           System.IO (hPutStrLn, stderr)

type RenderFunction a = Chart.Renderable a -> FilePath -> IO (Chart.PickFn a)

instance Default DiagramsChart.FileOptions where
  def = DiagramsChart.FileOptions (800, 600) DiagramsChart.SVG def

renderFunctions :: [(String, RenderFunction a)]
renderFunctions =
  [ (".eps", DiagramsChart.renderableToFile $
      DiagramsChart.fo_format .~ DiagramsChart.EPS $ def),
    (".svg", DiagramsChart.renderableToFile def) ]

backendFormats :: [String]
backendFormats = map fst renderFunctions

writeWithBackend :: Chart.Renderable () -> FP.FilePath -> IO ()
writeWithBackend renderable path =
  case lookup (snd $ FP.splitExtension path) renderFunctions of
    Just f  -> void $ f renderable path
    Nothing ->
      hPutStrLn stderr "Unknown output format!"

