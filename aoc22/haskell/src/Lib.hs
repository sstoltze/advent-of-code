module Lib where

readLines :: FilePath -> IO [String]
readLines f = fmap lines $ readFile f

groupBySeparator :: Eq a => a -> [a] -> [[a]]
groupBySeparator sep' xs' = groupBy' sep' xs' [] []
  where
    groupBy' _ [] res currentRes = reverse $ (reverse currentRes) : res
    groupBy' sep (x:xs) res currentRes =
      if sep == x then
        groupBy' sep xs ((reverse currentRes) : res) []
        else
        groupBy' sep xs res (x : currentRes)
