 # Advent of code 2022

Welcome to my 2022 advent of code solutions. This is my first year doing the challenge and I am going to be working through it all in haskell. 
With the aim of trying to write well structured and readable code. 

Note that the following language extensions are applied to the project globally:

 * TypeApplications
 * LambdaCase
 * DerivingStrategies
 * TypeOperators
 * DataKinds
 * DeriveGeneric
 * DeriveAnyClass
 * GeneralisedNewtypeDeriving
 * OverloadedStrings
 * FlexibleInstances
 * StandaloneDeriving

 ##### Using the following dependencies:

```haskell
module Main where

import Data.Word (Word16)
import GHC.Generics (Generic, D)
import Options.Generic
import Control.Monad (forM)
import System.Exit (exitSuccess, exitFailure)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
```

To begin with i'm going to write a simple harness for running the challenges on the input - as well as verifying the tests given in introductory text.


We want to define a list of days - each day with two parts. each part is simply a function that takes the inptut file as a string and returns the output, again as a string.

```haskell
type AOC = [AOCDay]
data AOCDay = AOCDay (String -> String) (String -> String)

challenges :: AOC
challenges = [
       AOCDay D1.part1 D1.part2
     , AOCDay D2.part1 D2.part2
     , AOCDay D3.part1 D3.part2
     , AOCDay D4.part1 D4.part2
     , AOCDay D5.part1 D5.part2
     , AOCDay D6.part1 D6.part2
     ]
```

We also wan't to parse command line options. Using optparse-generic - this is made pretty easy. First define the options you want as a data structure 
and the derive automatially the Parse record type class to run the otion parser on.

```haskell
data Part = Part1 | Part2
    deriving stock (Generic, Read, Show)
    deriving anyclass (ParseRecord, ParseField, ParseFields)

newtype Day = Day Word16
    deriving stock (Generic)
    deriving newtype (Read, Show, Num, Enum, Eq, Ord, Real, Integral)
    deriving anyclass (ParseRecord, ParseField, ParseFields)

data Options w = Run (w ::: Day <?> "Which day should I run?") (w ::: Part <!> "Part1" <?> "Either Part1 or Part2")
               | Test (w ::: Day <?> "Which day should I test?") (w ::: Part <!> "Part1" <?> "Either Part1 or Part2") (w ::: [String] <?> "list of test files to run")
    deriving stock (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)
```

```haskell
runHarness :: AOC -> IO ()
runHarness a = do
    ops <- unwrapRecord "Advent of Code 2022"
    case (ops :: Options Unwrapped) of
        Run day part -> do
                    let func = case ((a !! ((fromInteger . toInteger) day - 1)), part) of
                                (AOCDay p1 _, Part1) -> p1 
                                (AOCDay _ p2, Part2) -> p2
                    dat <- readFile $ "input/day" <> show day <> ".txt" 
                    putStrLn $ func dat
        
        Test day part tests -> reportErrors . foldMap All =<< forM tests (\test -> do 
                    let func = case (a !! ((fromInteger . toInteger) day - 1), part) of
                                (AOCDay p1 _, Part1) -> p1 
                                (AOCDay _ p2, Part2) -> p2
                    dat <- readFile (test <> "-in.txt")
                    expected <- readFile (test <> "-out.txt")
                    let res = func dat
                    case res == expected of
                        True -> putStrLn ("test " <> test <> " succeeded with output: " <> res) >> return True
                        False -> putStrLn ("test " <> test <> " failed\nExpected:\t" <> expected <> "\ngot:\t" <> res) >> return False
                    )

reportErrors :: All -> IO ()
reportErrors (All True) = putStr "Sucess! :)" >> exitSuccess
reportErrors (All False) = putStr "Some errors occurred - see above" >> exitFailure
```

A little quick and dirty but it will do for now!

```haskell
main :: IO ()
main = runHarness $ challenges
```

[On to day 1](doc/Day1.md)
[Day 2](doc/Day2.md)
[Day 3](doc/Day3.md)
[Day 4](doc/Day4.md)
[Day 5](doc/Day5.md)
[Day 6](doc/Day6.md)