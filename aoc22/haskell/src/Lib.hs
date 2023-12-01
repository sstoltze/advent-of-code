{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Data.Text (Text, lines, pack)
import Data.Text.Read (decimal)

readLines :: FilePath -> IO [Text]
readLines f = fmap (Data.Text.lines . pack) $ readFile f

textToInt :: Text -> Int
textToInt t = either (const (-1)) fst $ decimal t

groupBySeparator :: Eq a => a -> [a] -> [[a]]
groupBySeparator sep' xs' = groupBy' sep' xs' [] []
  where
    groupBy' _ [] res currentRes = reverse $ (reverse currentRes) : res
    groupBy' sep (x:xs) res currentRes =
      if sep == x then
        groupBy' sep xs ((reverse currentRes) : res) []
        else
        groupBy' sep xs res (x : currentRes)
