{-# LANGUAGE OverloadedStrings #-}
module Rps where

import Data.Text (words, Text)

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

data Game = Game { opponentMove :: RPS, myMove :: RPS } deriving (Show)

parseGame :: Text -> Game
parseGame g = case Data.Text.words g of
  [opponent, own] -> Game { opponentMove = fromOpponent opponent, myMove = fromSelf own}
  _ -> undefined

calculateMove :: Text -> RPS -> RPS
calculateMove status move =
  let moveList =
        case status of
          "X" -> dropWhile (\a -> move /= a) $ cycle (reverse [Rock, Paper, Scissors])
          "Y" -> cycle [move]
          "Z" -> dropWhile (\a -> move /= a) $ cycle [Rock, Paper, Scissors]
          _ -> undefined
       in moveList !! 1

updatedParseGame :: Text -> Game
updatedParseGame g = case Data.Text.words g of
  [opponent, status] ->
    let move = fromOpponent opponent in
      Game { opponentMove = move, myMove = calculateMove status move };
    _ -> undefined


fromOpponent :: Text -> RPS
fromOpponent "A" = Rock
fromOpponent "B" = Paper
fromOpponent "C" = Scissors
fromOpponent _ = undefined

fromSelf :: Text -> RPS
fromSelf "X" = Rock
fromSelf "Y" = Paper
fromSelf "Z" = Scissors
fromSelf _ = undefined

moveToScore :: RPS -> Int
moveToScore Rock = 1
moveToScore Paper = 2
moveToScore Scissors = 3

winnerScore :: RPS -> RPS -> Int
winnerScore opponent self =
  let opponentLosesTo = (dropWhile (\a -> opponent /= a) $ cycle [Rock, Paper, Scissors]) !! 1
  in
    if self == opponentLosesTo then 6 else if self == opponent then 3 else 0


toScore :: Game -> Int
toScore g = winnerScore (opponentMove g) (myMove g) + moveToScore (myMove g)
