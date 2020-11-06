module Main where

import Data.List.Split
import Data.Char
import Data.List
import Data.Bits
import Text.Printf

main :: IO ()
main = do
  putStrLn "hello world"

day10Input :: IO [Int]
day10Input = do
  s <- readFile "../input/day10-1.txt"
  return $ read <$>  splitOn "," s

data Knot = Knot { skipSize :: Int
                 , position :: Int
                 , knot :: [Int]
                 , knotLength :: Int
                 }

knotSize :: Int
knotSize = 256

mkKnot :: [Int] -> Knot
mkKnot l = Knot { skipSize = 0
                , position = 0
                , knot = cycle l
                , knotLength = length l
                }

newKnot :: Knot
newKnot = mkKnot [0..knotSize-1]

updateSkipSize :: Int -> Knot -> Knot
updateSkipSize n k = k { skipSize = n + skipSize k}

updatePosition :: Int -> Knot -> Knot
updatePosition len k = k { position = (len + position k + skipSize k) `mod` knotLength k}

reverseKnot :: Int -> Knot -> Knot
reverseKnot len k = k { knot = updatedKnot}
  where
    (toReverse, rest) = splitAt len $ drop pos $ knot k
    updatedKnot = drop (knotLength k - pos) $ cycle $ (reverse toReverse) ++ (take restLen rest)
    restLen = knotLength k - len
    pos = position k

getKnot :: Knot -> [Int]
getKnot k = take len $ drop (len * q) $ knot k
  where
    len = knotLength k
    q = position k `div` len

knotPosition :: Knot -> Int
knotPosition k = position k `mod` knotLength k

knotHash :: Int -> Knot -> Knot
knotHash len = updateSkipSize 1 . updatePosition len . reverseKnot len

runHash :: [Int] -> Knot -> Knot
runHash l k = foldl (flip knotHash) k l

knotMult :: Int -> Knot -> Int
knotMult n = product . take n . getKnot

day10 :: IO ()
day10 = do
  input <- day10Input
  let hash = runHash input newKnot
  let output = knotMult 2 hash
  putStrLn $ "Day 10-1: Product of the first two: " ++ show output
  input2 <- day10TwoInput
  let output2 = day10Two input2
  putStrLn $ "Day 10-1: Hash: " ++ output2

day10TwoInput :: IO [Int]
day10TwoInput = do
  i <- readFile "../input/day10-1.txt"
  return $ day10TwoInputTransform $ delete '\n' i

day10TwoInputTransform :: String -> [Int]
day10TwoInputTransform s = (fmap ord s) ++ [17, 31, 73, 47, 23]

runHashRounds :: Int -> [Int] -> Knot -> Knot
runHashRounds 0 _ k = k
runHashRounds n l k = runHashRounds (n-1) l $ runHash l k

denseHash :: [Int] -> [Int]
denseHash [] = []
denseHash bits = (foldl xor 0 f) : denseHash r
  where
    (f, r) = splitAt 16 bits

showHash :: [Int] -> String
showHash hash = fmap toLower $ foldl (++) "" $ fmap (printf "%02X") hash

day10Two :: [Int] -> String
day10Two list = showHash . denseHash . getKnot $ runHashRounds 64 list newKnot
