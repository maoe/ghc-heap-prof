{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Foldable (foldMap)
import Data.Monoid
import Prelude hiding (unlines)
import System.Environment
import System.IO

import Data.List.Split
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Builder as L

main :: IO ()
main = do
  factor:path:_ <- getArgs
  text <- L.readFile path
  L.hPutBuilder stdout $ thinDownBy (read factor) text

thinDownBy :: Int -> L.ByteString -> L.Builder
thinDownBy factor = unlines . map head . chunksOf factor . splitSample
  where
    unlines = foldMap (\b -> b <> "\n")

splitSample :: L.ByteString -> [L.Builder]
splitSample = map unlines . split byBeginSample . L.lines
  where
    unlines = foldMap (\text -> L.lazyByteString text <> "\n")
    byBeginSample = keepDelimsL $ whenElt $ L.isPrefixOf "BEGIN_SAMPLE"
