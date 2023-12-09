{-# LANGUAGE OverloadedStrings #-}
module Rucksack where
import           Data.Char (isUpper, ord)
import           Data.List (intersect, nub)
import qualified Data.Text as T

data Rucksack a = Rucksack {
  firstCompartment  :: [a],
  secondCompartment :: [a]
  }

contents :: Rucksack a -> [a]
contents r = firstCompartment r ++ secondCompartment r

parseRucksack :: T.Text -> Rucksack T.Text
parseRucksack r =  Rucksack { firstCompartment = T.chunksOf 1 first
                            , secondCompartment = T.chunksOf 1 second}
  where l = T.length r
        (first, second) = T.splitAt (l `div` 2) r

rucksackIntersection :: (Eq a) => Rucksack a  -> [a]
rucksackIntersection r = nub $ intersect (firstCompartment r) (secondCompartment r)

priority :: T.Text -> Int
priority = T.foldl' (\i c -> i + ord c -
                      if isUpper c then ord 'A' - 1 - 26 else ord 'a' - 1) 0

day3Example :: [T.Text]
day3Example = map T.pack $ lines "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k ls = let (a, b) = splitAt k ls in a : chunksOf k b

findBadges :: (Eq a) => [Rucksack a] -> [a]
findBadges [r1, r2, r3] = nub $ intersect i1 i2
  where i1 = nub $ intersect (contents r1) (contents r2)
        i2 = nub $ intersect (contents r2) (contents r3)
findBadges _ = undefined
