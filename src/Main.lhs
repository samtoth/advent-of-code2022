> module Main where

 # Advent of code 2022

\begin{code}

import qualified Day1 as D1
import Data.Word (Word16)
import GHC.Generics (Generic)
import Options.Generic
import Control.Monad (forM)
import System.Exit (exitSuccess, exitFailure)
import qualified Day2 as D2

main :: IO ()
main = runHarness $ challenges

type AOC = [AOCDay]
data AOCDay = AOCDay (String -> String) (String -> String)

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


challenges :: AOC
challenges = [
       AOCDay D1.part1 D1.part2
     , AOCDay D2.part1 D2.part2
       ]

\end{code}