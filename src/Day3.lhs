```haskell
module Day3 where

import Data.Char (isAsciiUpper)
import Data.Char (isAsciiLower)
import Data.Char (isAscii)
import Data.Monoid (Sum(..))
import Control.Arrow ((&&&), first)
import Control.Category ((<<<))
import Data.List.Split (chunksOf)
import Data.List (intersect)
```
```haskell


part1 :: String -> String
part1 = show . getSum . foldMap Sum . fmap eval . lines
    where
        -- | Partial function head used because it is guaranteed that there is at least one item in common between each compartment
        eval :: String -> Int
        eval =  value <<< head <<< uncurry intersect <<< uncurry splitAt <<< (`div` 2) . length &&& id

value :: Char -> Int
value c | isAsciiUpper c = fromEnum c - 64 + 26
value c | isAsciiLower c = fromEnum c - 96
value c | otherwise = error "bad input - char not ascii"
```


```haskell
part2 :: String -> String
part2 = show . getSum . foldMap Sum . fmap (eval . tuplefy) . chunksOf 3 . lines
    where 
          -- | another partial function - fine because it is specified that there are 3n lines of input
          tuplefy :: [a] -> ((a, a), a) 
          tuplefy [a, b ,c] = ((a, b), c)
          tuplefy _ = error "bad input - non even groups"

          -- | Partial function head - used because spec guarantees that there is one item shared between 3 rucksacs
          eval :: ((String, String), String) -> Int
          eval = value <<< head <<< uncurry intersect <<< first (uncurry intersect)
```
