module Aoc where

import System.IO

day1Input :: IO [Int]
day1Input = do
  file <- readFile "../input/day1-1.txt"
  return $ fmap read (lines file)

fuel :: Int -> Int
fuel m = (m `div` 3) - 2

totalFuel :: Int -> Int
totalFuel m = if f <= 0
              then 0
              else f + totalFuel f
  where f = fuel m

day1 :: IO ()
day1 = do
  input <- day1Input
  let moduleFuel = foldl (+) 0 (map fuel      input)
  let total      = foldl (+) 0 (map totalFuel input)
  putStrLn ("Fuel for modules: " ++ (show moduleFuel))
  putStrLn ("Total fuel: " ++ (show total))
