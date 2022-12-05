```haskell
module Day2  where
import Data.Monoid (Sum(..))
import Control.Arrow ((&&&))
import Control.Category ((<<<))
import Text.Megaparsec
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec.Char (char, space, hspace, newline)

type Game = (Draw, Draw)
data Draw = Rock | Paper | Scissors
    deriving (Eq, Show)
data Outcome = Win | Loss | Draw


type Points = Int

evaluateGame :: Game -> Outcome
evaluateGame (a, b) | a == b = Draw
evaluateGame (Rock, Paper) = Win
evaluateGame (Rock, Scissors) = Loss
evaluateGame (Paper, Rock) = Loss
evaluateGame (Paper, Scissors) = Win
evaluateGame (Scissors, Rock) = Win
evaluateGame (Scissors, Paper) = Loss

points :: Outcome -> Points
points Win = 6
points Draw = 3
points Loss = 0

extraPoints :: Draw -> Points
extraPoints Rock = 1
extraPoints Paper = 2
extraPoints Scissors = 3

type Parser = Parsec Void T.Text

drawP :: (Char, Char, Char) -> Parser Draw
drawP (r, p, s) = char r *> pure Rock     <|>
                  char p *> pure Paper    <|>
                  char s *> pure Scissors

gameP :: Parser Game
gameP = (,) <$> drawP ('A', 'B', 'C') <* hspace <*> drawP ('X', 'Y', 'Z')

gamesP :: Parser [Game]
gamesP = many (gameP <* optional newline)

parseGames :: T.Text -> Maybe [Game]
parseGames = parseMaybe gamesP 

eval :: Game -> Points
eval = uncurry (+) <<< (extraPoints . snd &&& points . evaluateGame)

score :: [Game] -> Points
score = getSum .  foldMap Sum . fmap eval

part1 :: String -> String
part1 = maybe "parse failed" show . fmap score . parseGames . T.pack

outcomeP :: Parser Outcome
outcomeP = char 'X' *> pure Loss <|> 
           char 'Y' *> pure Draw <|>
           char 'Z' *> pure Win

stratP :: Parser (Draw, Outcome)
stratP = (,) <$> drawP ('A', 'B', 'C') <* hspace <*> outcomeP

stratsP :: Parser [(Draw, Outcome)]
stratsP = many (stratP <* optional newline)

chooseDraw :: (Draw, Outcome) -> Draw
chooseDraw (d, Draw) = d
chooseDraw (Rock, Win) = Paper
chooseDraw (Rock, Loss) = Scissors
chooseDraw (Paper, Win) = Scissors
chooseDraw (Paper, Loss) = Rock
chooseDraw (Scissors, Win) = Rock
chooseDraw (Scissors, Loss) = Paper


part2 :: String -> String
part2 = maybe "parse failed" show . fmap (score . fmap (fst &&& chooseDraw)) . parseMaybe stratsP . T.pack

```