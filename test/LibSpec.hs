module LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Set                       ( fromList
                                                , Set
                                                , findMax
                                                , member
                                                , size
                                                )
import           Data.List                      ( sort )
import           Lib

spec :: Spec
spec = describe "Prefix Set" $ do
  it "(toWords o fromWords)" $ do
    let words    = ["hola", "adeu", "holi", "hopa"]
    let expected = fromList ["hola", "holi", "a", "hop"]
    -- using sets to compare results since the result is not a sorted list
    (fromList $ prefixes words) `shouldBe` expected

  it "(toWords o fromWords) over the empty list" $ do
    prefixes [] `shouldBe` []

  it "prop_sorted" $ (forAll string12List prop_sorted)

  it "prop_sharedPrefs" $ (forAll string12List prop_sharedPrefs)
  
-- changing the order of the input should not affect the output
prop_sorted :: [String] -> Bool
prop_sorted words = (fromList . prefixes . sort) words == (fromList . prefixes) words

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

cartesian :: (a -> b -> c) -> [a] -> [b] -> [[c]]
cartesian f xs ys = map (flip map ys . f) xs

prefixLengthSets :: [String] -> [Set Int]
prefixLengthSets words = fromList . (map length) <$> cartesian commonPrefix words words

-- The set always contains the comparison of each word with himself.
-- 1) If it is a word not sharing any prefix the set must be {0, 1}.
-- 2) Otherwise it must contain a word sharing a prefix of size n-1.
-- 3) If there is a single word it must be of size 1 and the set must be {1}
-- condition (2) ==> condition (1) .So coding (2) and (3) is enough.

-- A valid prefix set => (2) and (3).
isValid :: Set Int -> Bool
-- isValid set = member (findMax set - 1) set || set == fromList [1]
isValid set = member (findMax set - 1) set || set == fromList [1]

prop_sharedPrefs :: [String] -> Bool
prop_sharedPrefs words = all id $ isValid <$> sets
  where
    sets = prefixLengthSets $ prefixes words

-- Use words with {a,b,c,d} to have more chances to create shared prefixes
abcd12 :: Gen String
abcd12 = sized $ \n -> do sequence [ choose('a','d') | _ <- [1..12]]

string12List :: Gen [String]
string12List =
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ abcd12 | _ <- [1..k] ]
