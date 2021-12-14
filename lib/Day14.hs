{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List
import Data.Maybe (maybeToList)
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

newtype Element = Element
  { getElement :: Char
  }
  deriving newtype (Show, Eq, Ord)
  deriving (Generic)

instance Hashable Element

data Pair = Pair
  { getLeft :: Element,
    getRight :: Element
  }
  deriving (Show, Eq, Generic)

instance Hashable Pair

type Rules = HashMap Pair Element

data Input = Input
  { getPolymer :: [Element],
    getRules :: Rules
  }
  deriving (Show)

data Polymer = Polymer
  { getPairs :: HashMap Pair Int,
    getTail :: Element
  }
  deriving (Show)

----------------------------------------------------------------
-- Parsers
parseInput :: Parser Input
parseInput = do
  polymer <- many parseElement
  space
  Input polymer . HM.fromList <$> many parseRule

parseRule :: Parser (Pair, Element)
parseRule = do
  left <- parseElement
  right <- parseElement
  string " -> "
  between <- parseElement
  space
  pure (Pair left right, between)

parseElement :: Parser Element
parseElement = Element <$> upperChar

----------------------------------------------------------------
-- Solution1

solution1 :: Input -> Int
solution1 = solution 10

solution :: Int -> Input -> Int
solution times (Input polymer rules) =
  process $ iterate (stepPolymer rules) polymer !! times
  where
    process :: [Element] -> Int
    process result =
      let counts = fmap length . group . sort $ result
       in maximum counts - minimum counts

stepPolymer :: Rules -> [Element] -> [Element]
stepPolymer rules elements =
  concatMap applyRuleFor $ zipWith Pair elements hackyElements
  where
    applyRuleFor :: Pair -> [Element]
    applyRuleFor pair@(Pair left _) =
      left : maybeToList (HM.lookup pair rules)

    hackyElements :: [Element]
    hackyElements = tail elements ++ [Element '!']

----------------------------------------------------------------
-- Solution2

toPolymer :: [Element] -> Polymer
toPolymer elements =
  Polymer
    (HM.fromListWith (+) $ zip pairs (repeat 1))
    (last elements)
  where
    pairs :: [Pair]
    pairs = zipWith Pair elements (tail elements)

solution2 :: Input -> Int
solution2 (Input polymer rules) =
  process $ iterate (stepPolymer2 rules) (toPolymer polymer) !! 40
  where
    process :: Polymer -> Int
    process result =
      let counts = toStatistics result
       in maximum counts - minimum counts

toStatistics :: Polymer -> [Int]
toStatistics (Polymer pairs last) =
  HM.elems $ HM.foldrWithKey go (HM.singleton last 1) pairs
  where
    go :: Pair -> Int -> HashMap Element Int -> HashMap Element Int
    go (Pair l _) count stats =
      HM.insertWith (+) l count stats

stepPolymer2 :: Rules -> Polymer -> Polymer
stepPolymer2 rules (Polymer pairs last) =
  Polymer (HM.fromListWith (+) $ HM.foldrWithKey go [] pairs) last
  where
    go :: Pair -> Int -> [(Pair, Int)] -> [(Pair, Int)]
    go pair@(Pair left right) count currentResult =
      case HM.lookup pair rules of
        Nothing -> currentResult
        Just result ->
          (Pair left result, count) : (Pair result right, count) : currentResult

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day14-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 lines)

-- |
-- ABCD
--
--   AB     BC      CD
--  / \    / \     /  \
-- AX XB  BY YC   CZ  ZD
