{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Foldable (foldMap)
import Data.Monoid
import System.Environment

import Data.List.Split
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.Text.Lazy.IO as L

main :: IO ()
main = do
  factor:path:_ <- getArgs
  text <- L.readFile path
  L.putStr $ thinDownBy (read factor) text

thinDownBy :: Int -> L.Text -> L.Text
thinDownBy factor =
  L.toLazyText . unlines . map head . chunksOf factor . splitSample
  where
    unlines = foldMap (\b -> b <> "\n")

splitSample :: L.Text -> [L.Builder]
splitSample = map unlines . split byBeginSample . L.lines
  where
    unlines = foldMap (\text -> L.fromLazyText text <> "\n")
    byBeginSample = keepDelimsL $ whenElt $ L.isPrefixOf "BEGIN_SAMPLE"
