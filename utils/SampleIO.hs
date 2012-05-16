{-# LANGUAGE DeriveDataTypeable #-}

module SampleIO 
(
  readFileCol
) where

import           Control.Exception.Base (Exception)
import           Control.Monad (liftM)
import           Data.Conduit (($$), ($=)) 
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Typeable (Typeable)

data ReadException =
  DoubleConversionException String
  deriving (Show, Typeable)

instance Exception ReadException

readFileCol :: String -> Int -> IO [Double]
readFileCol fn col =
  liftM reverse $ C.runResourceT (
    CB.sourceFile fn $=
    CB.lines $=
    CT.decode CT.utf8 $=
    CL.map (T.split (== ' ')) $=
    CL.map (!! col) $=
    toDouble $$
    CL.consume )

toDouble :: C.MonadThrow m => C.Conduit T.Text m Double
toDouble = CL.mapM $ \v ->
  case TR.double v of
    Left err     -> C.monadThrow $ DoubleConversionException err
    Right (d, _) -> return d
