{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Data.Foldable
import Data.Function (on)
import Data.List
import Data.List.Extra (maximumOn)
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

data Velocity = Velocity
  { speedX :: Int,
    speedY :: Int
  }
  deriving (Show)

data Point = Point
  { getX :: Int,
    getY :: Int
  }
  deriving (Show)

data Rectangle = Rectangle
  { topLeft :: Point,
    bottomRight :: Point
  }
  deriving (Show)

data Probe = Probe
  { position :: Point,
    velocity :: Velocity
  }
  deriving (Show)

----------------------------------------------------------------
-- Parsers
parseInput :: Parser Rectangle
parseInput = do
  string "target area: x="
  topLeftX <- signed space decimal
  string ".."
  bottomRightX <- signed space decimal
  string ", y="
  bottomRightY <- signed space decimal
  string ".."
  topLeftY <- signed space decimal
  space
  pure $ Rectangle (Point topLeftX topLeftY) (Point bottomRightX bottomRightY)

----------------------------------------------------------------
-- Solution1
step :: Probe -> Probe
step Probe {..} = Probe pos vel
  where
    pos =
      Point (getX position + speedX velocity) (getY position + speedY velocity)
    vel =
      Velocity (let x = speedX velocity in x + (-1 * signum x)) (speedY velocity - 1)

solution1 :: Rectangle -> Int
solution1 = maximum . concatMap (fmap (getY . position)) . intersectingPaths

intersectingPaths :: Rectangle -> [[Probe]]
intersectingPaths bounds = filter (any intersectsBounds) allPaths
  where
    plausibleVelocities :: [Velocity]
    plausibleVelocities =
      [ Velocity x y
        | x <- [0 .. 70],
          y <- [-225 .. 1000]
      ]

    startProbes :: [Probe]
    startProbes = Probe (Point 0 0) <$> plausibleVelocities

    allPaths :: [[Probe]]
    allPaths =
      let x = takeWhile (hasNotGoneOver bounds) . iterate step <$> startProbes
       in x
    intersectsBounds :: Probe -> Bool
    intersectsBounds Probe {position} = position `inBounds` bounds

inBounds :: Point -> Rectangle -> Bool
inBounds p1 Rectangle {..} =
  getX p1 >= getX topLeft
    && getX p1 <= getX bottomRight
    && getY p1 <= getY topLeft
    && getY p1 >= getY bottomRight

hasNotGoneOver :: Rectangle -> Probe -> Bool
hasNotGoneOver bounds Probe {position}
  | getX position > boundsMaxX = False
  | getY position < boundsMinY = False
  | otherwise = True
  where
    boundsMaxX, boundsMinY :: Int
    boundsMaxX = getX . bottomRight $ bounds
    boundsMinY = getY . bottomRight $ bounds

----------------------------------------------------------------
-- Solution2

solution2 :: Rectangle -> Int
solution2 = length . intersectingPaths

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day17-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)
