module Main where

import Test.Hspec (describe, hspec, it, shouldContain)

import qualified Day9

main :: IO ()
main = hspec $ do
  describe "run" $ do
    it "outputs day9" $ do
      Day9.run `shouldContain` "Day 9"
