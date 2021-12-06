{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

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

--------------------------------------

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Eq, Show)

data Line = Line
  { start :: Point,
    end :: Point
  }
  deriving (Show)

type Parser a = Parsec Void Text a

parseInput :: Parser [Line]
parseInput = many (parseLine <* space1)

parseLine :: Parser Line
parseLine = do
  x1 <- decimal
  char ','
  y1 <- decimal
  string " -> "
  x2 <- decimal
  char ','
  y2 <- decimal
  pure $ Line (Point x1 y1) (Point x2 y2)

--------------------------------------
main :: IO ()
main = do
  contents <- T.readFile "day5-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right lines -> print $ solution1 lines

--------------------------------------

pointsOnLine :: Line -> [Point]
pointsOnLine l@(Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = [Point x1 yn | yn <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [Point xn y1 | xn <- [min x1 x2 .. max x1 x2]]
  | x1 - x2 == y1 - y2 = [Point xn yn | xn <- [min x1 x2 .. max x1 x2], yn <- [min y1 y2 .. max y1 y2], x1 - xn == y1 - yn]
  | x1 + y1 == x2 + y2 = [Point xn yn | xn <- [min x1 x2 .. max x1 x2], yn <- [min y1 y2 .. max y1 y2], xn + yn == x1 + y1]
  | otherwise = D.traceShow l $ error "nope"

solution1 :: [Line] -> Int
solution1 lines = s1
  where
    points :: [Point]
    points = foldMap pointsOnLine lines

    overlaps :: [(Point, Int)]
    overlaps = foldl go [] points

    go :: [(Point, Int)] -> Point -> [(Point, Int)]
    go table p =
      case lookup p table of
        Nothing -> (p, 0) : table
        Just value -> (p, value + 1) : filter ((/= p) . fst) table

    s1 :: Int
    s1 = foldl go' 0 overlaps

    go' :: Int -> (Point, Int) -> Int
    go' current (_, next)
      | next > 0 = current + 1
      | otherwise = current

-- |
--  1,1 -> 3,3
--  1,1  2,2  3,3
--
--  3,3 -> 1,1
--  3,3  2,2  1,1
--
--  3,5 -> 7,9
--  3,5 4,6 5,7 6,8 7,9
--
--  0,5 -> 5,0
--  0,5 1,4 2,3 3,2 4,1 5,0
--
--  5,0 -> 0,5
--  5,0 4,1 3,2 2,3 1,4 0,5
--
--  3,5 -> 5,3
--  5,3 4,4 3,5
