{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Aoc where

import           Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import           Data.Text (pack)
import           Data.Char (digitToInt, intToDigit)
import           Data.List (elemIndex, minimumBy, find)
import qualified Data.Attoparsec.Text as P
import           Control.Applicative


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
  let moduleFuel = sum (map fuel      input)
  let total      = sum (map totalFuel input)
  putStrLn $ "Fuel for modules: " ++ show moduleFuel
  putStrLn $ "Total fuel: " ++ show total

newtype Vec3 a = Vec3 (a, a, a)
  deriving (Show, Eq, Functor)

instance Num a => Num (Vec3 a) where
  Vec3 (a1, a2, a3) + Vec3 (b1, b2, b3) = Vec3 (a1 + b1, a2 + b2, a3 + b3)
  Vec3 (a1, a2, a3) * Vec3 (b1, b2, b3) = Vec3 (a1 * b1, a2 * b2, a3 * b3)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger n = Vec3 (fromInteger n, fromInteger n, fromInteger n)

type Position = Vec3 Int
type Velocity = Vec3 Int

data Moon = Moon {position :: Position,
                  velocity :: Velocity}
  deriving (Show, Eq)

day12Input :: IO [Moon]
day12Input = do
  file <- readFile "../input/day12-1.txt"
  case P.parseOnly parseMoons (pack file) of
    Right moons -> return moons
    Left _ -> return []

day12TestInput :: [Moon]
day12TestInput = case P.parseOnly parseMoons "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>" of
    Right moons -> moons
    Left _ -> []

parseMoons :: P.Parser [Moon]
parseMoons = do
  moons <- P.many' parseMoon
  P.endOfInput
  return moons
  where
    parseMoon :: P.Parser Moon
    parseMoon = do
      P.char '<'
      x <- parseCoordinate
      P.string ", "
      y <- parseCoordinate
      P.string ", "
      z <- parseCoordinate
      P.char '>'
      P.many' P.endOfLine
      return $ Moon (Vec3 (x, y, z)) (Vec3 (0, 0, 0))
    parseCoordinate :: P.Parser Int
    parseCoordinate = do
      P.letter
      P.char '='
      P.signed P.decimal

applyVelocity :: Moon -> Moon
applyVelocity m = Moon { position = position m + velocity m
                       , velocity = velocity m}

applyGravity :: [Moon] -> [Moon]
applyGravity [] = []
applyGravity (m:ms) = m':ms' where
  (m', ms') = foldr moonGravity (m, []) (applyGravity ms)
  moonGravity newMoon (updatedMoon, updatedMoons) =
    ( Moon (position updatedMoon) (velocity updatedMoon - change)
    , Moon (position newMoon)     (velocity newMoon     + change) : updatedMoons) where
    change = signum $ position updatedMoon - position newMoon

simulateMoons :: [Moon] -> [Moon]
simulateMoons = fmap applyVelocity . applyGravity

sumVec :: (Num a) => Vec3 a -> a
sumVec (Vec3 (a, b, c)) = a + b + c

potentialEnergy :: Moon -> Int
potentialEnergy = sumVec . abs . position

kineticEnergy :: Moon -> Int
kineticEnergy = sumVec . abs . velocity

totalEnergy :: Moon -> Int
totalEnergy m = kineticEnergy m * potentialEnergy m

systemEnergy :: [Moon] -> Int
systemEnergy = sum . fmap totalEnergy

moonPeriod :: [Moon] -> Int
moonPeriod ms = p
  where
    simulation = drop 1 $ iterate simulateMoons ms
    p = maybe 0 (+1) $ elemIndex ms simulation

period :: [Moon] -> Int
period ms = foldr lcm 1 [lcmx ms, lcmy ms, lcmz ms]
  where
    lcmx = moonPeriod . map xMoon
    lcmy = moonPeriod . map yMoon
    lcmz = moonPeriod . map zMoon
    xMoon (Moon (Vec3 (x,_,_)) (Vec3 (vx,_,_))) = Moon (Vec3 (x,0,0)) (Vec3 (vx,0,0))
    yMoon (Moon (Vec3 (_,y,_)) (Vec3 (_,vy,_))) = Moon (Vec3 (0,y,0)) (Vec3 (0,vy,0))
    zMoon (Moon (Vec3 (_,_,z)) (Vec3 (_,_,vz))) = Moon (Vec3 (0,0,z)) (Vec3 (0,0,vz))

day12 :: IO ()
day12 = do
  moons <- day12Input
  let simulation = iterate simulateMoons moons
  let energy = systemEnergy $ simulation !! 1000
  putStrLn $ "Total energy after 1000 turns: " ++ show energy
  let p = period moons
  putStrLn $ "The system has period " ++ show p

data OrbitName = COM
               | OrbitName String
               deriving (Show, Eq)

data Orbit = Orbit { orbitName   :: OrbitName
                   , orbitAround :: OrbitName
                   } deriving (Show, Eq)

parseOrbits :: P.Parser [Orbit]
parseOrbits = do
  orbits <- P.many' parseSingleOrbit
  P.endOfInput
  return $ Orbit COM COM : orbits
  where
    parseSingleOrbit = do
      stationary <- P.many' (P.letter <|> P.digit)
      P.char ')'
      sattelite <- P.many' (P.letter <|> P.digit)
      P.many' P.endOfLine
      return Orbit { orbitName = toOrbitName sattelite, orbitAround = toOrbitName stationary }
    toOrbitName s = if s == "COM" then COM else OrbitName s

allOrbits :: Orbit -> [Orbit] -> [Orbit]
allOrbits o os = if orbitName o == COM
                 then []
                 else case findOrbitByName (orbitAround o) os of
                        Just o' -> o' : allOrbits o' os
                        Nothing -> []

findOrbitByName :: OrbitName -> [Orbit] -> Maybe Orbit
findOrbitByName _ [] = Nothing
findOrbitByName n (o:os) = if orbitName o == n then Just o else findOrbitByName n os

day6Input :: IO [Orbit]
day6Input = do
  file <- readFile "../input/day6-1.txt"
  case P.parseOnly parseOrbits (pack file) of
    Right orbits -> return orbits
    Left _ -> return []

day6TestInput :: IO [Orbit]
day6TestInput = case P.parseOnly parseOrbits (pack "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") of
                  Right orbits -> return orbits
                  Left _ -> return []

day6 :: IO ()
day6 = do
  orbits <- day6Input
  putStrLn $ "Total number of orbits is " ++ show (sum (map (length . flip allOrbits orbits) orbits))

data Layer = Layer { layerWidth :: Int
                   , layerHeight :: Int
                   , layerPixels :: [Int]
                   } deriving (Show)

data Image = Image { imageWidth :: Int
                   , imageHeight :: Int
                   , imageLayers :: [Layer]
                   } deriving (Show)

errorImage :: Image
errorImage = Image { imageWidth = 0
                   , imageHeight = 0
                   , imageLayers = []
                   }

listRows :: Int -> Int -> [a] -> [[a]]
listRows _ 0 _ = []
listRows width height pixels = row : listRows width (height-1) rest
  where
    (row, rest) = splitAt width pixels

parseLayer :: Int -> Int -> P.Parser Layer
parseLayer width height = pixelsToLayer <$> (fmap digitToInt <$> P.count (width * height) P.digit)
  where
    pixelsToLayer p = Layer { layerWidth = width
                            , layerHeight = height
                            , layerPixels = p
                            }

parseImage :: Int -> Int -> P.Parser Image
parseImage width height = layersToImage <$> P.many' (parseLayer width height)
  where
    layersToImage ls = Image { imageWidth = width
                             , imageHeight = height
                             , imageLayers = ls
                             }

drawImage :: Image -> String
drawImage (Image { imageWidth = width, imageHeight = height, imageLayers = layers}) = join '\n' $ listRows width height $ fmap drawImageCharacter [0..imageSize]
  where
    imageSize = height * width - 1
    drawImageCharacter = layerCharacter layers
    layerCharacter [] _ = 'O'
    layerCharacter (l:ls) i = case layerPixels l !! i of
                                  0 -> ' '
                                  1 -> 'X'
                                  2 -> layerCharacter ls i
                                  _ -> error ":("
    join _ [] = []
    join c (x:xs) = x ++ c : join c xs


day8TestInput :: Image
day8TestInput = case P.parseOnly (parseImage 3 2) "123456789012" of
                  Right image -> image
                  Left _ -> errorImage

day8Input :: IO Image
day8Input = do
  file <- readFile "../input/day8-1.txt"
  return $ either (const errorImage) id (P.parseOnly (parseImage 25 6) (pack file))

day8 :: IO ()
day8 = do
  input <- day8Input
  let minLayer = minimumBy compareLayerZeroes (imageLayers input)
  print $ layerCount 1 minLayer * layerCount 2 minLayer
  putStr $ drawImage input
  where
    compareLayerZeroes a b = compare (layerCount 0 a) (layerCount 0 b)
    layerCount n = length . filter (== n) . layerPixels

parseSignal :: String -> [Int]
parseSignal = map digitToInt . filter (/= '\n')

showSignal :: [Int] -> String
showSignal = map intToDigit

fft :: [Int] -> [Int]
fft signal = take (length signal) $ map (`fftPass` signal) [1..]
  where
    fftPass :: Int -> [Int] -> Int
    fftPass k pat = abs (sum $ zipWith (*) pat (passPattern k)) `mod` 10
    baseFftPattern :: [Int]
    baseFftPattern = [0, 1, 0, -1]
    passPattern :: Int -> [Int]
    passPattern k = drop 1 $ cycle $ concatMap (replicate k) baseFftPattern

fftSignals :: [Int] -> [[Int]]
fftSignals signal = let next = fft signal in signal : fftSignals next

day16Input :: IO [Int]
day16Input = parseSignal <$> readFile "../input/day16-1.txt"

day16 :: IO ()
day16 = do
  signal <- day16Input
  let signals = fftSignals signal
  let hundreth = signals !! 100
  putStrLn $ take 8 $ showSignal hundreth
  putStrLn $ take 8 $ showSignal (realSignal signal)

realSignal :: [Int] -> [Int]
realSignal signal = drop (read $ showSignal $ take 7 signal) $ fftSignals (concat $ replicate 10000 signal) !! 100

type Reagent = (Int, String)
type Reactions = Map.Map Reagent [Reagent]

parseReactions :: String -> Reactions
parseReactions s = case P.parseOnly parseReactions' (pack s) of
  Right reactions -> reactions
  Left _          -> Map.empty
  where
    parseReactions' :: P.Parser Reactions
    parseReactions' = do
      reactions <- P.many' parseSingleReaction
      P.endOfInput
      return $ foldr Map.union Map.empty reactions
    parseSingleReaction :: P.Parser Reactions
    parseSingleReaction = do
      reagents <- P.many' parseReagent
      P.string " => "
      result <- parseReagent
      P.many' P.endOfLine
      return $ Map.singleton result reagents
    parseReagent :: P.Parser Reagent
    parseReagent = do
      P.many' (P.char ' ' <|> P.char ',')
      number <- P.decimal
      P.char ' '
      reagent <- P.many' P.letter
      return (number, reagent)

lookupReagent :: Reactions -> String -> Maybe Reagent
lookupReagent reactions element = find ((== element) . reagentName) $ Map.keys reactions

reagentName :: Reagent -> String
reagentName = snd

neededElements :: Reactions -> Reagent -> [Reagent]
neededElements reactions (k, element) =
  if k <= 0
  then [(k, element)]
  else
    case lookupReagent reactions element of
      Just (n, _) -> foldr addReagentToList [] reagents
        where q' = k `div` n
              q = if q'*n < k then q' + 1 else q'
              reagents' = fromJust $ Map.lookup (n, element) reactions
              reagents = (k - q*n, element) : fmap (\(a,e) -> (a*q, e)) reagents'
      Nothing -> [(k, element)]

addReagentToList :: Reagent -> [Reagent] -> [Reagent]
addReagentToList (0, _)  [] = []
addReagentToList reagent [] = [reagent]
addReagentToList reagent@(k, element) (r@(n, rName):rs)
  | element == rName = if k+n == 0 then rs else (k+n, element) : rs
  | otherwise        = r : addReagentToList reagent rs

runBackwardsReactions :: Reactions -> [Reagent] -> [Reagent]
runBackwardsReactions reactions wantedList = foldr addReagentToList [] $ concatMap (neededElements reactions) wantedList

runBackwardsReactionsToCompletion :: Reactions -> [Reagent] -> [Reagent]
runBackwardsReactionsToCompletion reactions reagentList = fst $ head $ dropWhile (uncurry (/=)) $ zip produced (drop 2 produced)
  where produced = iterate (runBackwardsReactions reactions) reagentList

testReactions :: Reactions
testReactions = parseReactions $ unlines [ "10 ORE => 10 A"
                                         , "1 ORE => 1 B"
                                         , "7 A, 1 B => 1 C"
                                         , "7 A, 1 C => 1 D"
                                         , "7 A, 1 D => 1 E"
                                         , "7 A, 1 E => 1 FUEL"]

testReactions2 :: Reactions
testReactions2 = parseReactions $ unlines [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
                                          , "17 NVRVD, 3 JNWZP => 8 VPVL"
                                          , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
                                          , "22 VJHF, 37 MNCFX => 5 FWMGM"
                                          , "139 ORE => 4 NVRVD"
                                          , "144 ORE => 7 JNWZP"
                                          , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
                                          , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
                                          , "145 ORE => 6 MNCFX"
                                          , "1 NVRVD => 8 CXFTF"
                                          , "1 VJHF, 6 MNCFX => 4 RFSQX"
                                          , "176 ORE => 6 VJHF"]

day14Input :: IO Reactions
day14Input = parseReactions <$> readFile "../input/day14-1.txt"

oreProduced :: [Reagent] -> Int
oreProduced = maybe 0 fst . find ((== "ORE") . reagentName)

maxFuelForOre :: Reactions -> Int -> Int -> Int -> Int
maxFuelForOre reactions maxOre start end = maxFuelForOre' reactions maxOre (nextGuess start end) 0 start end
  where
    nextGuess a b = (b + a) `div` 2
    maxFuelForOre' rs n k best min max
      | min == max || max - min == 1  = if produced min <= n
                                        then min
                                        else best
      | produced k <= n = if produced k >= best
                          then maxFuelForOre' rs n (nextGuess min max) k    k   max
                          else maxFuelForOre' rs n (nextGuess min max) best min k
      | otherwise = maxFuelForOre' rs n (nextGuess min max) best min k
    produced fuel = oreProduced $ runBackwardsReactionsToCompletion reactions [(fuel, "FUEL")]


day14 :: IO ()
day14 = do
  reactions <- day14Input
  let completed = runBackwardsReactionsToCompletion reactions [(1, "FUEL")]
  let oreNeeded = oreProduced completed
  print oreNeeded
  let ore = 1000000000000
  let canBeProduced = maxFuelForOre reactions ore 1 ore
  print canBeProduced
