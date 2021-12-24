{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.Bool (bool)
import Data.Either.Extra (fromRight')
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.List.Extra (groupSortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Day3 (toNumber)
import Day9 (Point (Point, getX, getY), toCoordinates)
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

type EnhancementMap = [Bool]

type Image = Map Point Bool

data Input = Input
  { enhMap :: EnhancementMap,
    img :: Image
  }
  deriving (Show)

----------------------------------------------------------------
-- Parsers
parseInput :: Parser Input
parseInput =
  Input <$> parseEnhancementMap <*> fmap toCoordinates parseImage
  where
    toCoordinates :: [[a]] -> Map Point a
    toCoordinates =
      Map.fromList
        . foldMap (zipWith go [0 ..])
        . transpose
        . fmap (zip [0 ..])

    go :: Int -> (Int, a) -> (Point, a)
    go y (x, value) = (Point x y, value)

parseEnhancementMap :: Parser EnhancementMap
parseEnhancementMap = many parseMapItem

parseMapItem :: Parser Bool
parseMapItem = toBool <$> punctuationChar
  where
    toBool :: Char -> Bool
    toBool =
      \case
        '.' -> False
        '#' -> True
        _ -> error "bad bit found"

parseImage :: Parser [[Bool]]
parseImage = many parseLine

parseLine :: Parser [Bool]
parseLine = do
  space1
  many parseMapItem

----------------------------------------------------------------
-- Solution1
enhance :: EnhancementMap -> Bool -> Image -> Image
enhance emap fill img = Map.foldlWithKey go Map.empty $ padImage fill img
  where
    go :: Image -> Point -> Bool -> Image
    go current p v =
      Map.union current
        . Map.singleton p
        . lookupPixel emap
        . computeIndex fill img
        $ p

padImage :: Bool -> Image -> Image
padImage fill img =
  Map.union img $
    surroundSquareFromTopLeftToBottomRight topLeft bottomRight
  where
    surroundSquareFromTopLeftToBottomRight :: Point -> Point -> Map Point Bool
    surroundSquareFromTopLeftToBottomRight p1 p2 =
      Map.fromList
        . fmap (,fill)
        $ [Point x y | x <- [getX p1 - 1 .. getX p2 + 1], y <- [getY p1 - 1, getY p2 + 1]]
          ++ [Point x y | y <- [getY p1 - 1 .. getY p2 + 1], x <- [getX p1 - 1, getX p2 + 1]]

    topLeft :: Point
    topLeft = fst . fromJust . Map.lookupMin $ img

    bottomRight :: Point
    bottomRight = fst . fromJust . Map.lookupMax $ img

computeIndex :: Bool -> Image -> Point -> Int
computeIndex fill img = toNumber . fmap (fromMaybe fill . (`Map.lookup` img)) . nineNeighbours

nineNeighbours :: Point -> [Point]
nineNeighbours Point {getX = x, getY = y} =
  [ Point (x - 1) (y - 1),
    Point x (y - 1),
    Point (x + 1) (y - 1),
    Point (x - 1) y,
    Point x y,
    Point (x + 1) y,
    Point (x - 1) (y + 1),
    Point x (y + 1),
    Point (x + 1) (y + 1)
  ]

lookupPixel :: EnhancementMap -> Int -> Bool
lookupPixel = (!!)

countLights :: Image -> Int
countLights = length . filter id . Map.elems

solution1 :: Input -> Int
solution1 Input {enhMap, img} = countLights . enhance enhMap True . enhance enhMap False $ img

----------------------------------------------------------------
-- Solution2

solution2 :: Input -> Int
solution2 Input {enhMap, img} = countLights $ go 50 False img
  where
    go :: Int -> Bool -> Image -> Image
    go 0 fill = id
    go n fill = go (n - 1) (not fill) . enhance enhMap fill

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day20-1-input"
  -- contents <- T.readFile "d20"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)

drawMap :: Image -> [String]
drawMap =
  fmap (fmap (bool '.' '#' . snd) . sortOn (getX . fst))
    . groupSortOn (getY . fst)
    . Map.toList

-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
