{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day15 where

import Algorithm.Search (dijkstra)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Day9 (Point (Point), neighbourPoints, toCoordinates)
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

type CostMap = Map Point Int

----------------------------------------------------------------
-- Parsers
parseInput :: Parser [[Int]]
parseInput = some parseLine

parseLine :: Parser [Int]
parseLine = (fmap (read . pure) <$> some digitChar) <* space

----------------------------------------------------------------
-- Solution1

buildCostMap :: [[Int]] -> CostMap
buildCostMap =
  Map.fromList
    . foldMap (zipWith go [0 ..])
    . transpose
    . fmap (zip [0 ..])
  where
    go :: Int -> (Int, Int) -> (Point, Int)
    go y (x, value) = (Point x y, value)

findNeighbourPoints :: CostMap -> Point -> [Point]
findNeighbourPoints hm p =
  filter (isJust . (`Map.lookup` hm)) $ neighbourPoints p

solution1 :: [[Int]] -> Int
solution1 input =
  maybe 0 fst $
    dijkstra
      (findNeighbourPoints cm)
      (\_ p -> fromMaybe 999 $ Map.lookup p cm)
      (== end)
      (Point 0 0)
  where
    cm :: CostMap
    cm = buildCostMap input

    -- bottom right is length of a line, minus one
    end :: Point
    end = Point (length $ tail $ head input) (length $ tail $ head input)

----------------------------------------------------------------
-- Solution2

extendMap :: CostMap -> CostMap
extendMap = Map.foldrWithKey' go Map.empty
  where
    go :: Point -> Int -> Map Point Int -> Map Point Int
    go p risk m = Map.union m (Map.fromList $ mkPoints p risk)

    mkPoints :: Point -> Int -> [(Point, Int)]
    mkPoints (Point x y) risk =
      [ (Point (x + x' * 100) (y + y' * 100), wrap (risk + x' + y'))
        | x' <- [0 .. 4],
          y' <- [0 .. 4]
      ]

    wrap :: Int -> Int
    wrap x
      | x > 9 = x - 9
      | otherwise = x

solution2 :: [[Int]] -> Int
solution2 input =
  maybe 0 fst $
    dijkstra
      (findNeighbourPoints cm)
      (\_ p -> fromMaybe 999 $ Map.lookup p cm)
      (== end)
      (Point 0 0)
  where
    cm :: CostMap
    cm = extendMap $ buildCostMap input

    -- bottom right is length of a line, minus one
    end :: Point
    end = fromJust $ fst <$> Map.lookupMax cm

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day15-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)
