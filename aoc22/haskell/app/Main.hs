module Main (main) where

import Lib
import Data.List (sort)

main :: IO ()
main = day1

day1 :: IO ()
day1 = do
  putStrLn "Day 1"
  input <- fmap (groupBySeparator "") $ readLines "../input/day1-1.txt"
  let totalElfCalories = fmap (sum . (fmap (read :: String -> Int))) input
  let sortedCalories = reverse $ sort totalElfCalories
  putStrLn "Most calories caried:"
  print $ head sortedCalories
  putStrLn "Sum of three most calories:"
  print $ sum $ take 3 sortedCalories
