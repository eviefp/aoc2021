{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day6 where

import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List
import Data.Maybe (fromMaybe)
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
newtype LanternfishAge = LanternfishAge Int
  deriving newtype (Show, Hashable, Eq)

----------------------------------------------------------------
-- Parsers

parseInput :: Parser [LanternfishAge]
parseInput = (:) <$> parseLanternfishAge <*> many parseOne

parseOne :: Parser LanternfishAge
parseOne = char ',' *> parseLanternfishAge

parseLanternfishAge :: Parser LanternfishAge
parseLanternfishAge = LanternfishAge <$> decimal

----------------------------------------------------------------
-- Solution 1

next :: LanternfishAge -> [LanternfishAge]
next (LanternfishAge age)
  | age == 0 = [LanternfishAge 6, offspring]
  | otherwise = [LanternfishAge (age - 1)]

offspring :: LanternfishAge
offspring = LanternfishAge 8

step :: [LanternfishAge] -> [LanternfishAge]
step = foldMap next

solution1 :: [LanternfishAge] -> Int
solution1 input = length $ foldl go input [0 .. 255]
  where
    go :: [LanternfishAge] -> Int -> [LanternfishAge]
    go lfa x =
      let result = step lfa
          output = seq result $ show x <> " " <> show (length result) <> "\n"
       in D.trace output result

----------------------------------------------------------------
-- Solution 2

step2 :: HashMap LanternfishAge Int -> HashMap LanternfishAge Int
step2 hm = foldl go mempty allAges
  where
    go :: HashMap LanternfishAge Int -> LanternfishAge -> HashMap LanternfishAge Int
    go current age =
      let nextValue = next2 hm age
       in HM.unionWith (+) current nextValue

allAges :: [LanternfishAge]
allAges =
  [ LanternfishAge 8,
    LanternfishAge 7,
    LanternfishAge 6,
    LanternfishAge 5,
    LanternfishAge 4,
    LanternfishAge 3,
    LanternfishAge 2,
    LanternfishAge 1,
    LanternfishAge 0
  ]

next2 :: HashMap LanternfishAge Int -> LanternfishAge -> HashMap LanternfishAge Int
next2 hm idx =
  case next idx of
    [single] -> fst $ updateSingle idx single hm
    [parent, child] -> updateParent idx parent child hm
    _ -> error "unexpected"

updateParent :: LanternfishAge -> LanternfishAge -> LanternfishAge -> HashMap LanternfishAge Int -> HashMap LanternfishAge Int
updateParent old new child hm =
  let (updated, value) = updateSingle old new hm
   in HM.insert child value updated

updateSingle :: LanternfishAge -> LanternfishAge -> HashMap LanternfishAge Int -> (HashMap LanternfishAge Int, Int)
updateSingle old new hm =
  let value = fromMaybe 0 (HM.lookup old hm)
   in (HM.insert new value mempty, value)

seed :: [LanternfishAge] -> HashMap LanternfishAge Int
seed input = HM.fromListWith (+) $ (,1) <$> input

solution2 :: [LanternfishAge] -> Int
solution2 input = HM.foldl (+) 0 $ foldl (\hm _ -> step2 hm) (seed input) [0 .. 255]

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day6-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right input -> print $ solution2 input
