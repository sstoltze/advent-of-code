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

data Direction = N | S | NE | NW | SE | SW deriving (Read, Show)

newtype Path = Path [Direction] deriving (Show)

data SplitPath = SplitPath { north :: Int
                           , south :: Int
                           , southwest :: Int
                           , southeast :: Int
                           , northwest :: Int
                           , northeast :: Int
                           } deriving (Show)

simplifySplitPath :: SplitPath -> SplitPath
simplifySplitPath sp
  | n  >= 1 && s  >= 1 = simplifySplitPath $ sp { north = n - min n s
                                                , south = s - min n s
                                                }
  | nw >= 1 && se >= 1 = simplifySplitPath $ sp { northwest = nw - min nw se
                                                , southeast = se - min nw se
                                                }
  | ne >= 1 && sw >= 1 = simplifySplitPath $ sp { northeast = ne - min ne sw
                                                , southwest = sw - min ne sw
                                                }
  | n  >= 1 && sw >= 1 = simplifySplitPath $ sp { north = n - min n sw
                                                , southwest = sw - min n sw
                                                , northwest = nw + min n sw
                                                }
  | n  >= 1 && se >= 1 = simplifySplitPath $ sp { north = n - min n se
                                                , southeast = se - min n se
                                                , northeast = ne + min n se
                                                }
  | s  >= 1 && nw >= 1 = simplifySplitPath $ sp { south = s - min s nw
                                                , northwest = nw - min s nw
                                                , southwest = sw + min s nw
                                                }
  | s  >= 1 && ne >= 1 = simplifySplitPath $ sp { south = s - min s ne
                                                , northeast = ne - min s ne
                                                , southeast = se + min s ne
                                                }
  | nw >= 1 && ne >= 1 = simplifySplitPath $ sp { northwest = nw - min nw ne
                                                , northeast = ne - min nw ne
                                                , north = n + min nw ne
                                                }
  | sw >= 1 && se >= 1 = simplifySplitPath $ sp { southwest = sw - min sw se
                                                , southeast = se - min sw se
                                                , south = s + min sw se
                                                }
  | otherwise = sp
  where
    n = north sp
    s = south sp
    nw = northwest sp
    ne = northeast sp
    sw = southwest sp
    se = southeast sp

splitPathLength :: SplitPath -> Int
splitPathLength sp = n + s + nw + ne + sw + se
  where
    n = north sp
    s = south sp
    nw = northwest sp
    ne = northeast sp
    sw = southwest sp
    se = southeast sp

splitPath :: Path -> SplitPath
splitPath (Path []) = SplitPath { north = 0
                                , south = 0
                                , southwest = 0
                                , southeast = 0
                                , northwest = 0
                                , northeast = 0
                                }
splitPath (Path (d:ds)) = case d of
                            N -> p { north = north p + 1 }
                            S -> p { south = south p + 1 }
                            NW -> p { northwest = northwest p + 1 }
                            NE -> p { northeast = northeast p + 1 }
                            SW -> p { southwest = southwest p + 1 }
                            SE -> p { southeast = southeast p + 1 }
  where
    p = splitPath (Path ds)

readPath :: String -> Path
readPath s = Path $ fmap read $ splitOn "," $ fmap toUpper s

day11Input :: IO Path
day11Input = do
  s <- readFile "../input/day11-1.txt"
  return $ readPath s

day11Length = splitPathLength . simplifySplitPath . splitPath

day11 :: IO ()
day11 = do
  inputPath <- day11Input
  let len = day11Length inputPath
  putStrLn $ "Day 11-1: Path length is " ++ show len
  let Path p = inputPath
  let maxDistance = maximum $ fmap (day11Length . Path) $ inits p
  putStrLn $ "Day 11-2: Max distance on the trip was " ++ show maxDistance
