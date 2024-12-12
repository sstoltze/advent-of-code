module Main where

import Test.Hspec (describe, hspec, it, shouldContain)

import Aoc24.Day9
import Data.Text

main :: IO ()
main = hspec $ do
  describe "run" $ do
    it "outputs day9" $ do
      (Data.Text.unpack Aoc24.Day9.run) `shouldContain` "Day 9"
