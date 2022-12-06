```haskell
module Day6 where

import Data.List
import Control.Arrow
import Data.Monoid (Any(..))

windowed :: Int -> [a] -> [[a]]
windowed n = fmap (take n) . tails

unique :: Eq a => [a] -> Bool
unique = not . getAny . foldMap Any . fmap (uncurry elem <<< head &&& tail) . uncurry ($) . (take . pred . length &&& tails)
````

part1 :: String -> String
part1 = show . (+4). length . takeWhile not . fmap unique . windowed 4 

 ## Part 2


We need to now look for a 'packet' size of 14 instead of 4. This should be a fairly easy change, first extract out the bulk of the function from part 1:
```haskell
findPacketUnique :: Eq a => Int -> [a] -> Int
findPacketUnique l = (+ l) . length . takeWhile not . fmap unique . windowed l

part1 :: String -> String
part1 = show . findPacketUnique 4
```
part 2 writes itself :) 

```haskell
part2 :: String -> String
part2 = show . findPacketUnique 14
```