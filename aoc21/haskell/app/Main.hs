module Main where

import           Lib (someFunc)

main :: IO ()
main = someFunc

-- Day 2
data Submarine = Submarine {subDepth :: Int, subHorizontal :: Int, subAim :: Int} deriving (Show)

newSubmarine :: Submarine
newSubmarine = Submarine 0 0 0

data SubmarineCommand
  = Forward Int
  | Up Int
  | Down Int

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

parseCommand :: String -> SubmarineCommand
parseCommand c = case words c of
  ("forward" : n : _) -> Forward (read n)
  ("up" : n : _)      -> Up (read n)
  ("down" : n : _)    -> Down (read n)
  _                   -> Forward 0

day2Input :: IO [SubmarineCommand]
day2Input = fmap parseCommand . lines <$> readFile "../input/day2.txt"

day2 :: IO ()
day2 = do
  input <- day2Input
  let Submarine {subDepth = depth, subHorizontal = hori} = foldl runCommand newSubmarine input
  putStrLn $ "The submarine is at depth " ++ show depth ++ " and horizontal " ++ show hori ++ ". The product is " ++ show (depth * hori)
  let Submarine {subDepth = depth, subHorizontal = hori} = foldl runCommand' newSubmarine input
  putStrLn $ "With aim, the submarine is at depth " ++ show depth ++ " and horizontal " ++ show hori ++ ". The product is " ++ show (depth * hori)
