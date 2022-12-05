```haskell
module Day5 where

import Control.Monad.State (State, MonadState (..), modify, runState)
import Control.Monad (forM_, forM)
import Control.Arrow ((>>>), (***))
import Control.Category (Category)
import Data.List.Index (modifyAt)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (letterChar, char, string, digitChar, newline, hspace, space)
import Control.Monad (void)
import Control.Arrow ((<<<))


type Stack a = [a]

newtype StackM w a = StackM { unStack :: State [Stack w] a }
    deriving newtype (Functor, Applicative, Monad, MonadState [Stack w])

runStack :: [Stack w] -> StackM w a -> [Stack w]
runStack w = snd . (flip runState) w . unStack

getStack :: Int -> StackM w (Stack w)
getStack n = (!! n ) <$> get

pop :: Int -> StackM w w
pop n = getStack n >>= \case
        (top : rest) -> modify (modifyAt n tail) >> return top
        [] -> error "bad instruction set"

push :: Int -> w -> StackM w ()
push n x = modify $ modifyAt n (x:)


data Instruction = Instr {boxes :: Int, from :: Int, to :: Int}
        deriving (Show)


perform :: Instruction -> StackM w ()
perform (Instr n from to) = forM_ [1..n] . const $ pop (from - 1) >>= push (to - 1)
    
type Parser = Parsec Void String

boxOrNoBox :: Parser (Maybe Char)
boxOrNoBox =     Just <$> (char '[' *> letterChar <* char ']')
             <|> char ' ' *> string "  " *> pure Nothing

layerParser :: Parser [Maybe Char]
layerParser = sepBy boxOrNoBox (char ' ')

bottemParser :: Parser [[a]]
bottemParser = sepBy1 (char ' ' *> digitChar *> char ' ' *> pure []) (char ' ')


stackParser :: Parser [Stack Char]
stackParser = try bottemParser
          <|> do
                row <- layerParser
                void $ newline
                rest <- stackParser
                return . fmap (uncurry $ maybe id (:)) $ zip (row ++ repeat Nothing) rest



intP :: Parser Int
intP = read <$> some (digitChar) 


instructionP :: Parser Instruction
instructionP = Instr <$> (string "move " *> intP) <* string " from " <*> intP <* string " to " <*> intP

instructionsP :: Parser [Instruction]
instructionsP = sepEndBy instructionP newline

inputP :: Parser ([Stack Char], [Instruction])
inputP = (,) <$> stackParser <*  space <*> instructionsP

part1 :: String -> String
part1 = maybe "parse failed" ( fmap head <<< uncurry ($) <<< (runStack *** sequence_ . fmap perform) ) . parseMaybe inputP 

performMk2 :: Instruction -> StackM w ()
performMk2 (Instr n from to) = forM [1..n] (const $ pop (from - 1)) >>= mapM_ (push (to - 1)) . reverse
 
part2 :: String -> String
part2 = maybe "parse failed" ( fmap head <<< uncurry ($) <<< (runStack *** sequence_ . fmap performMk2) ) . parseMaybe inputP 


```