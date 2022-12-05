```haskell
module Day4 (part1, part2) where
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec.Char


type Parser = Parsec Void T.Text

newtype Range = Range (Int, Int)
    deriving (Show)

intP :: Parser Int
intP = read <$> some (digitChar) 


rangeP :: Parser Range
rangeP =  Range <$> ((,) <$> intP <* char '-' <*> intP)

pairP :: Parser (Range, Range)
pairP = (,) <$> rangeP <* char ',' <*> rangeP

pairsP :: Parser [(Range, Range)]
pairsP = (++) <$>
         many (try (pairP <* newline)) <*>
         (maybe [] (:[]) <$> optional (pairP <* optional newline))

isContained :: (Range, Range) -> Bool
isContained (Range (a, b), Range (c, d)) | a <= c && b >= d = True
isContained (Range (a, b), Range (c, d)) | c <= a && d >= b = True
isContained (_           , _           ) | otherwise      = False

overlap :: (Range, Range) -> Bool
overlap (Range (a, b), Range (c, d)) | b < c = False
overlap (Range (a, b), Range (c, d)) | d < a = False
overlap (_           , _           ) | otherwise = True

part1 :: String -> String
part1 = maybe "parse failed" show . fmap @Maybe (length . filter isContained) .  parseMaybe pairsP . T.pack

part2 :: String -> String
part2 = maybe "parse failed" show . fmap @Maybe (length . filter overlap) .  parseMaybe pairsP . T.pack

```