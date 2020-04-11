{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Aoc where

--import System.IO
import Data.Text (pack)
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

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

data Moon = Moon {position :: Position,
                  velocity :: Velocity}
  deriving Show

day12Input :: IO [Moon]
day12Input = do
  file <- readFile "../input/day12-1.txt"
  putStrLn file
  case parseOnly inputMoons (pack file) of
    Right moons -> return moons
    Left _ -> return []

inputMoons :: Parser [Moon]
inputMoons = do
  moons <- many' moonLine
  endOfInput
  return moons

-- <x=-1, y=0, z=2>
moonLine :: Parser Moon
moonLine = do
  char '<'
  x <- parseCoordinate
  string ", "
  y <- parseCoordinate
  string ", "
  z <- parseCoordinate
  char '>'
  many' endOfLine
  return $ Moon (x, y, z) (0, 0, 0)

parseCoordinate :: Parser Int
parseCoordinate = do
  letter
  char '='
  signed decimal

applyVelocity :: Moon -> Moon
applyVelocity m = Moon { position = (x + vx, y + vy, z + vz)
                       , velocity = velocity m}
  where
    (x, y, z)    = position m
    (vx, vy, vz) = velocity m



applyGravity :: [Moon] -> [Moon]
applyGravity [] = []
applyGravity (m:ms) = m':ms' where
  ms' = applyGravity ms
  (m', ms'')

day12 :: IO ()
day12 = do
  moons <- day12Input
  putStrLn "Test"
