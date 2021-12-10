{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Bits       as B
import qualified Data.Char       as C
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "Day 1"
  day1
  putStrLn "Day 2"
  day2

-- Day 1
day1Input :: IO [Int]
day1Input = fmap read . lines <$> readFile "../input/day1.txt"

countIncreases :: (a -> Int) -> [a] -> Int
countIncreases = countIncreases' 0
  where
    countIncreases' res key list =
      case list of
        [] -> res
        [_] -> res
        (x : y : xs) ->
          if key x < key y
            then countIncreases' (res + 1) key (y : xs)
            else countIncreases' res key (y : xs)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n l =
  let next = take n l
   in if length next < n
        then []
        else next : slidingWindow n (drop 1 l)

day1 :: IO ()
day1 = do
  input <- day1Input
  let depthIncreases = countIncreases id input
  putStrLn $ "The number of increases in depth is " ++ show depthIncreases
  let slidingIncreases = countIncreases sum $ slidingWindow 3 input
  putStrLn $ "The number of sliding increases in depth is " ++ show slidingIncreases

-- Day 2
data Submarine = Submarine {subDepth :: Int, subHorizontal :: Int, subAim :: Int} deriving (Show)

newSubmarine :: Submarine
newSubmarine = Submarine 0 0 0

data SubmarineCommand
  = Forward Int
  | Up Int
  | Down Int

parseCommand :: String -> SubmarineCommand
parseCommand c = case words c of
  ("forward" : n : _) -> Forward (read n)
  ("up" : n : _)      -> Up (read n)
  ("down" : n : _)    -> Down (read n)
  _                   -> Forward 0

-- Day 2.1
runCommand :: Submarine -> SubmarineCommand -> Submarine
runCommand s (Forward n) = s {subHorizontal = subHorizontal s + n}
runCommand s (Up n)      = s {subDepth = subDepth s - n}
runCommand s (Down n)    = s {subDepth = subDepth s + n}

-- Day 2.2
runCommand' :: Submarine -> SubmarineCommand -> Submarine
runCommand' s (Forward n) = s {subHorizontal = subHorizontal s + n, subDepth = subDepth s + n * subAim s}
runCommand' s (Up n) = s {subAim = subAim s - n}
runCommand' s (Down n) = s {subAim = subAim s + n}

day2Input :: IO [SubmarineCommand]
day2Input = fmap parseCommand . lines <$> readFile "../input/day2.txt"

day2 :: IO ()
day2 = do
  input <- day2Input
  let Submarine {subDepth = depth, subHorizontal = hori} = foldl runCommand newSubmarine input
  putStrLn $
    "The submarine is at depth "
      ++ show depth
      ++ " and horizontal "
      ++ show hori
      ++ ". The product is "
      ++ show (depth * hori)
  let Submarine {subDepth = part2Depth, subHorizontal = part2Hori} = foldl runCommand' newSubmarine input
  putStrLn $
    "With aim, the submarine is at depth "
      ++ show part2Depth
      ++ " and horizontal "
      ++ show part2Hori
      ++ ". The product is "
      ++ show (part2Depth * part2Hori)

-- Day 6
day6Input :: IO LanternfishSchool
day6Input = readFish . fmap read . Split.splitOn "," <$> readFile "../input/day6.txt"

-- Represents days-to-offspring -> number of fish
type LanternfishSchool = Map.Map Int Int

readFish :: [Int] -> LanternfishSchool
readFish = foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty

updateSchool :: Int -> LanternfishSchool -> LanternfishSchool
updateSchool 0 s = s
updateSchool n s = updateSchool (n -1) $ Map.fromListWith (+) $ foldl updateKey [] $ Map.toList s
  where
    -- A mature fish creates a new fish that takes 8 days to mature, and the original can produce new fish in 6 days
    updateKey res (0, v) = (8, v) : (6, v) : res
    -- All other fish get one day closer
    updateKey res (i, v) = (i - 1, v) : res

countFish :: LanternfishSchool -> Int
countFish = sum . Map.elems

day6 :: IO ()
day6 = do
  fish <- day6Input
  putStrLn $ "After 80 days, there are " ++ show (countFish $ updateSchool 80 fish) ++ " fish."
  putStrLn $ "After 256 days, there are " ++ show (countFish $ updateSchool 256 fish) ++ " fish."
