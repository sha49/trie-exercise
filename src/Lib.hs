module Lib
    ( Trie(..)
    , prefixes
    , empty
    , addWord
    , fromWords
    , toWords
    , coordinates
    ) where

import System.IO
import qualified Data.HashMap.Strict           as HM
import           Data.HashMap.Strict            ( HashMap )
import           Data.Geohash                   ( encode )
import           Data.Maybe
import qualified Data.ByteString.Lazy          as LB
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as LB8
import qualified Data.ByteString.Lex.Fractional as BF

-- -------------------------------------------------------------
data Trie = Node (HashMap Char Trie) | Leaf String deriving Show

empty :: Trie
empty = Node HM.empty
-- -------------------------------------------------------------
prefixes :: [String] -> [String]
prefixes = toWords . fromWords
-- -------------------------------------------------------------
addWord :: Trie -> String -> Trie
addWord t [] = t
addWord (Leaf rest) word = addToLeaf word rest
addWord (Node childs) word = addToNode word childs

addToLeaf :: String -> String -> Trie
addToLeaf s@(w:ws) [] = Leaf s
addToLeaf s@(w:ws) l@(r:rs)
  | w == r = addWord (addWord empty l) s 
  | otherwise = Node $ HM.fromList [(w, Leaf ws), (r, Leaf rs)]

addToNode :: String -> (HashMap Char Trie) -> Trie
addToNode (w:ws) childs
  | HM.null childs = Node $ HM.fromList [(w, Leaf ws)]
  | otherwise = case (HM.lookup w childs) of
      Nothing -> Node $ HM.insert w (Leaf ws) childs
      (Just child) -> Node $ HM.insert w (addWord child ws) childs
-- -------------------------------------------------------------
fromWords :: [String] -> Trie
fromWords = foldl addWord empty
-- -------------------------------------------------------------
toWords :: Trie -> [String]
toWords (Leaf rest) = [""]
toWords (Node childs) = concatMap collapse $ HM.toList childs
    where
  collapse (char, trie) = prependM char (toWords trie)

prependM :: Char -> [String] -> [String]
prependM c [] = [[c]]
prependM c s = map ((:) c) s
-- -------------------------------------------------------------
coordinates :: (Fractional a) => ByteString -> (a, a)
coordinates line = toTup $ fst <$> values
    where
  toTup (lat:long:[]) = (lat, long)
  toTup _ = error "Wrong file format"
  values = fromJust -- it is ok to explode if the file is not well formatted
      $ sequence
      $ BF.readSigned BF.readDecimal . LB8.toStrict
      <$> LB8.split ',' line
