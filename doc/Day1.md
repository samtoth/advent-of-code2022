[Day 1 official page](https://adventofcode.com/2022/day/1)


```haskell
module Day1 (part1, part2) where

import Data.Monoid (Sum(..))
import Text.Read (readMaybe)
import Data.Semigroup (Max(..))
import Data.List
```

```haskell
groupMap :: Monoid b => (a -> b) -> [Maybe a] -> [b]
groupMap f xs = groupHelper (fmap f <$> xs) mempty

groupHelper :: Monoid a => [Maybe a] -> a -> [a]
groupHelper [] curr = [curr]
groupHelper (Just x : rest) curr = groupHelper rest (x <> curr)
groupHelper (Nothing : rest) curr = curr : groupHelper rest mempty


getGroups :: String -> [Sum Int]
getGroups = groupMap Sum . fmap (readMaybe @Int) . lines

part1 :: String -> String
part1 = show . getSum . getMax . foldMap Max . getGroups

part2 :: String -> String
part2 = show . getSum . getSum . foldMap Sum . take 3 . reverse . sort . getGroups

```