{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day9 where

import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
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
data Point = Point
  { getX :: Int,
    getY :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Point

----------------------------------------------------------------
-- Parsers
parseInput :: Parser [[Int]]
parseInput = many parseLine

parseLine :: Parser [Int]
parseLine = do
  line <- many digitChar
  newline
  pure $ read . pure <$> line

----------------------------------------------------------------
-- Solution1

horizontalMinimum :: [[Int]] -> [[(Int, Bool)]]
horizontalMinimum = fmap (fmap hasMinimum . zipIt)
  where
    zipIt :: [Int] -> [(Int, Int, Int)]
    zipIt input = zip3 (9 : input) input (tail input ++ [9])

    hasMinimum :: (Int, Int, Int) -> (Int, Bool)
    hasMinimum (prev, current, next) = (current, current < prev && current < next)

globalMinimum :: [[(Int, Bool)]] -> [[(Int, Bool)]]
globalMinimum = fmap (fmap hasMinimum . zipIt) . transpose
  where
    zipIt :: [(Int, Bool)] -> [(Int, Int, Int, Bool)]
    zipIt input = zipWith3 go (9 : fmap fst input) input (fmap fst (tail input) ++ [9])

    go :: Int -> (Int, Bool) -> Int -> (Int, Int, Int, Bool)
    go prev (current, isHorizontalMinimum) next =
      (prev, current, next, isHorizontalMinimum)

    hasMinimum :: (Int, Int, Int, Bool) -> (Int, Bool)
    hasMinimum (prev, current, next, isHorizontalMinimum) =
      (current, current < prev && current < next && isHorizontalMinimum)

solution1 :: [[Int]] -> Int
solution1 =
  getSum
    . foldMap (Sum . (+ 1))
    . foldMap (fmap fst . filter snd)
    . globalMinimum
    . horizontalMinimum

----------------------------------------------------------------
-- Solution2

solution2 :: [[Int]] -> Int
solution2 input =
  let hm = toCoordinates input
      minimums = findMinimumPoints hm
      basins = computeBasin hm <$> Set.toList minimums
      topBasins = take 3 . reverse . sort . fmap Set.size $ basins
   in product topBasins

toCoordinates :: [[Int]] -> HashMap Point Int
toCoordinates = HM.fromList . foldMap (zipWith go [0 ..]) . transpose . fmap (zip [0 ..])
  where
    go :: Int -> (Int, Int) -> (Point, Int)
    go y (x, value) = (Point x y, value)

findMinimumPoints :: HashMap Point Int -> Set Point
findMinimumPoints hm = HM.foldrWithKey (isMinimum hm) Set.empty hm

computeBasin :: HashMap Point Int -> Point -> Set Point
computeBasin hm p =
  let value = fromMaybe undefined $ HM.lookup p hm
      neihgbours = neighbourPoints p
   in Set.union (Set.singleton p) . Set.unions . fmap (computeBasin hm) $ nextPoints value
  where
    nextPoints :: Int -> [Point]
    nextPoints value =
      fmap fst
        . filter (\(_, x) -> x > value && x < 9)
        . mapMaybe go
        $ neighbourPoints p

    go :: Point -> Maybe (Point, Int)
    go p = (p,) <$> HM.lookup p hm

isMinimum :: HashMap Point Int -> Point -> Int -> Set Point -> Set Point
isMinimum hm p v acc =
  if all (> v) $ findNeighbourValues hm p
    then Set.insert p acc
    else acc

findNeighbourValues :: HashMap Point Int -> Point -> [Int]
findNeighbourValues hm p = catMaybes $ (`HM.lookup` hm) <$> neighbourPoints p

neighbourPoints :: Point -> [Point]
neighbourPoints (Point x y) =
  [Point (x -1) y, Point (x + 1) y, Point x (y -1), Point x (y + 1)]

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day9-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 lines)

testData :: [[Int]]
testData =
  [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
    [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
    [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
    [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
    [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
  ]
