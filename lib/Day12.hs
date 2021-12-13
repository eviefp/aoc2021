{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.Char (isUpper)
import Data.Foldable
import Data.Function (on)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

data Segment = Start | Small String | Big String | End
  deriving (Eq, Show, Generic)

instance Hashable Segment

data Connection = Connection Segment Segment
  deriving (Show)

type Map = HashMap Segment [Segment]

----------------------------------------------------------------
-- Parsers

parseInput :: Parser [Connection]
parseInput = many parseConnection

parseConnection :: Parser Connection
parseConnection = do
  left <- parseSegment
  char '-'
  right <- parseSegment
  space
  pure $ Connection left right

parseSegment :: Parser Segment
parseSegment = start <|> end <|> cave
  where
    start = string "start" $> Start
    end = string "end" $> End
    cave = do
      name <- many letterChar
      pure $
        if all isUpper name
          then Big name
          else Small name

----------------------------------------------------------------
-- Solution1

input2Map :: [Connection] -> Map
input2Map xs = HM.fromListWith (<>) (foldl getSegments [] xs)
  where
    getSegments :: [(Segment, [Segment])] -> Connection -> [(Segment, [Segment])]
    getSegments xs (Connection s1 s2) = (s1, [s2]) : (s2, [s1]) : xs

nextPaths :: Map -> [Segment] -> [Segment]
nextPaths _ [] = [Start]
nextPaths m path = filter isValidSegment . concat . HM.lookup (last path) $ m
  where
    isValidSegment :: Segment -> Bool
    isValidSegment
      | last path == End = const False
      | otherwise =
        \case
          Start -> False
          seg@(Small _) -> seg `notElem` path
          _ -> True

findAllPaths :: Map -> [Segment] -> [[Segment]]
findAllPaths m current = do
  next <- nextPaths m current
  let now = current ++ [next]
  now : findAllPaths m now

solution1 :: [Connection] -> Int
solution1 = length . filter reachedEnd . flip findAllPaths [] . input2Map

reachedEnd :: [Segment] -> Bool
reachedEnd =
  \case
    [] -> False
    [End] -> True
    (seg : segs) -> reachedEnd segs

----------------------------------------------------------------
-- Solution2
nextPaths2 :: Map -> [Segment] -> [Segment]
nextPaths2 _ [] = [Start]
nextPaths2 m path = filter isValidSegment . concat . HM.lookup (last path) $ m
  where
    isValidSegment :: Segment -> Bool
    isValidSegment
      | last path == End = const False
      | otherwise =
        \case
          Start -> False
          seg@(Small _) -> twiceRule path seg
          _ -> True

twiceRule :: [Segment] -> Segment -> Bool
twiceRule path seg =
  not (hasVisitedSmallTwice path) || (seg `notElem` path)

hasVisitedSmallTwice :: [Segment] -> Bool
hasVisitedSmallTwice path =
  not (null ([seg | seg <- path, length (filter (go seg) path) > 1]))
  where
    go :: Segment -> Segment -> Bool
    go seg =
      \case
        oth@(Small _) -> oth == seg
        _ -> False

findAllPaths2 :: Map -> [Segment] -> [[Segment]]
findAllPaths2 m current = do
  next <- nextPaths2 m current
  let now = current ++ [next]
  now : findAllPaths2 m now

solution2 :: [Connection] -> Int
solution2 = length . filter reachedEnd . flip findAllPaths2 [] . input2Map

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day12-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 lines)
