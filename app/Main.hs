{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy          as LB
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as LB8
import qualified Data.ByteString.Lex.Fractional
                                               as BF
import           Data.Geohash                   ( encode )
import qualified Data.HashMap.Strict           as HM
import           Data.HashMap.Strict            ( HashMap )
import           Data.Maybe
import           Lib                            ( prefixes
                                                , coordinates
                                                )

main :: IO ()
main = do
  file <- LB.readFile "test_points.txt"
  let lines = tail (LB8.lines file)
  -- parallel parsing and geohash computation
  let coords = withStrategy (parBuffer 100 rdeepseq) $ map processLine lines
  let words = prefixes coords
  LB.writeFile "resultat.txt" $ LB8.unlines $ LB8.pack <$> words
    where
      processLine = fromJust . (encode 12) . coordinates
