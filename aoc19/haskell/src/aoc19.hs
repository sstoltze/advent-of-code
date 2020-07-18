{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Aoc where

--import System.IO
import Data.Text (pack)
import Data.Char (digitToInt)
import Data.List (elemIndex, minimumBy)
import Data.Attoparsec.Text
--import Data.Attoparsec.Combinator
import Control.Applicative
--import Data.Scientific (Scientific)

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
  case parseOnly parseMoons (pack file) of
    Right moons -> return moons
    Left _ -> return []

day12TestInput :: [Moon]
day12TestInput = case parseOnly parseMoons "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>" of
    Right moons -> moons
    Left _ -> []

parseMoons :: Parser [Moon]
parseMoons = do
  moons <- many' parseMoon
  endOfInput
  return moons
  where
    parseMoon :: Parser Moon
    parseMoon = do
      char '<'
      x <- parseCoordinate
      string ", "
      y <- parseCoordinate
      string ", "
      z <- parseCoordinate
      char '>'
      many' endOfLine
      return $ Moon (Vec3 (x, y, z)) (Vec3 (0, 0, 0))
    parseCoordinate :: Parser Int
    parseCoordinate = do
      letter
      char '='
      signed decimal

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

parseOrbits :: Parser [Orbit]
parseOrbits = do
  orbits <- many' parseSingleOrbit
  endOfInput
  return $ Orbit COM COM : orbits
  where
    parseSingleOrbit = do
      stationary <- many' (letter <|> digit)
      char ')'
      sattelite <- many' (letter <|> digit)
      many' endOfLine
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
  case parseOnly parseOrbits (pack file) of
    Right orbits -> return orbits
    Left _ -> return []

day6TestInput :: IO [Orbit]
day6TestInput = case parseOnly parseOrbits (pack "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") of
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

parseLayer :: Int -> Int -> Parser Layer
parseLayer width height = pixelsToLayer <$> (fmap digitToInt <$> count (width * height) digit)
  where
    pixelsToLayer p = Layer { layerWidth = width
                            , layerHeight = height
                            , layerPixels = p
                            }

parseImage :: Int -> Int -> Parser Image
parseImage width height = layersToImage <$> many' (parseLayer width height)
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
day8TestInput = case parseOnly (parseImage 3 2) "123456789012" of
                  Right image -> image
                  Left _ -> errorImage

day8Input :: IO Image
day8Input = do
  file <- readFile "../input/day8-1.txt"
  return $ either (const errorImage) id (parseOnly (parseImage 25 6) (pack file))

day8 :: IO ()
day8 = do
  input <- day8Input
  let minLayer = minimumBy compareLayerZeroes (imageLayers input)
  print $ layerCount 1 minLayer * layerCount 2 minLayer
  putStr $ drawImage input
  where
    compareLayerZeroes a b = compare (layerCount 0 a) (layerCount 0 b)
    layerCount n = length . filter (== n) . layerPixels
