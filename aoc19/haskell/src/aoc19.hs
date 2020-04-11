{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Aoc where

--import System.IO
import Data.Text (pack)
import Data.List (elemIndex)
import Data.Attoparsec.Text
import Data.Scientific (Scientific)

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

-- <x=-1, y=0, z=2>
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

xMoon :: Moon -> Moon
xMoon (Moon (Vec3 (x,_,_)) (Vec3 (vx,_,_))) = Moon (Vec3 (x,0,0)) (Vec3 (vx,0,0))

yMoon :: Moon -> Moon
yMoon (Moon (Vec3 (_,y,_)) (Vec3 (_,vy,_))) = Moon (Vec3 (0,y,0)) (Vec3 (0,vy,0))

zMoon :: Moon -> Moon
zMoon (Moon (Vec3 (_,_,z)) (Vec3 (_,_,vz))) = Moon (Vec3 (0,0,z)) (Vec3 (0,0,vz))

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

day12 :: IO ()
day12 = do
  moons <- day12Input
  let simulation = iterate simulateMoons moons
  let energy = systemEnergy $ simulation !! 1000
  putStrLn $ "Total energy after 1000 turns: " ++ show energy
  let p = period moons
  putStrLn $ "The system has period " ++ show p
