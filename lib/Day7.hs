{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

----------------------------------------------------------------
-- Parsers
parseInput :: Parser [Int]
parseInput = (:) <$> decimal <*> many parseOne

parseOne :: Parser Int
parseOne = char ',' *> decimal

----------------------------------------------------------------
-- Solution
calculateFuelForPart1 :: [Int] -> Int -> Int
calculateFuelForPart1 columns column = sum $ abs . (column -) <$> columns

calculateFuelForPart2 :: [Int] -> Int -> Int
calculateFuelForPart2 columns column = sum $ sum . toCountList . (abs . (column -)) <$> columns

toCountList :: Int -> [Int]
toCountList end = [0 .. end]

solution1 :: [Int] -> Int
solution1 columns = minimum $ calculateFuelForPart1 columns <$> columns

solution2 :: [Int] -> Int
solution2 columns = minimum $ calculateFuelForPart2 columns <$> columns

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day7-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right columns ->
      print $ solution2 columns
