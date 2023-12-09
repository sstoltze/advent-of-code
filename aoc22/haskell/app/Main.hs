{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib
import           Rps
import           Rucksack
-- import Data.Text (Text)
import           Data.List (sort)

main :: IO ()
main = day1

day1 :: IO ()
day1 = do
  putStrLn "Day 1"
  input <- fmap (groupBySeparator "") $ readLines "../input/day1-1.txt2"
  let totalElfCalories = fmap (sum . (fmap textToInt)) input
  let sortedCalories = reverse $ sort totalElfCalories
  putStrLn "Most calories caried:"
  print $ head sortedCalories
  putStrLn "Sum of three most calories:"
  print $ sum $ take 3 sortedCalories

day2 :: IO ()
day2 = do
  putStrLn "Day 2"
  fileInput <- readLines "../input/day2-1.txt"
  let totalScore = sum $ fmap (toScore . parseGame) fileInput
  putStrLn "Total score:"
  print totalScore
  let updatedScore = sum $ fmap (toScore . updatedParseGame) fileInput
  putStrLn "Updated score:"
  print updatedScore

day3 :: IO ()
day3 = do
  putStrLn "Day 3"
  fileInput <- readLines "../input/day3-1.txt"
  let rucksacks = fmap parseRucksack fileInput
  let inBothPockets = concat $ fmap rucksackIntersection rucksacks
  let summedPriorities = sum $ fmap priority inBothPockets
  putStrLn "Summed priorities:"
  print summedPriorities
  let badges = fmap findBadges $ chunksOf 3 rucksacks
  let badgePriorities = sum $ fmap (sum . fmap priority) badges
  putStrLn "Summed badges:"
  print badgePriorities
