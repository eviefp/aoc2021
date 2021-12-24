{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day22 where

import Data.Bool (bool)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Set (Set)
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

data Range x = Range
  { rangeMin :: x,
    rangeMax :: x
  }
  deriving (Show)

data Cube = Cube
  { cubeX :: Range Int,
    cubeY :: Range Int,
    cubeZ :: Range Int
  }
  deriving (Show)

data Instruction = Instruction
  { instrCube :: Cube,
    instrWhat :: Bool
  }
  deriving (Show)

type Input = [Instruction]

----------------------------------------------------------------
-- Parsers
parseInput :: Parser Input
parseInput = many parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  instrWhat <- parseInstrBool
  string " x="
  cubeX <- parseRange
  string ",y="
  cubeY <- parseRange
  string ",z="
  cubeZ <- parseRange
  newline
  pure $ Instruction {instrWhat, instrCube = Cube {..}}

parseRange :: Parser (Range Int)
parseRange = do
  rangeMin <- signed space decimal
  string ".."
  rangeMax <- signed space decimal
  pure $ Range {..}

parseInstrBool :: Parser Bool
parseInstrBool = True <$ string "on" <|> False <$ string "off"

----------------------------------------------------------------
-- Solution1
solution1 :: Input -> Int
solution1 = countCubes . foldl go (\_ _ _ -> Sum 0)
  where
    countCubes :: (Int -> Int -> Int -> Sum Int) -> Int
    countCubes f =
      getSum $
        fold
          [ f x y z
            | x <- [-50 .. 50],
              y <- [-50 .. 50],
              z <- [-50 .. 50]
          ]

    go ::
      (Int -> Int -> Int -> Sum Int) ->
      Instruction ->
      Int ->
      Int ->
      Int ->
      Sum Int
    go f Instruction {instrCube, instrWhat} x y z
      | coordsInCube instrCube x y z = bool (Sum 0) (Sum 1) instrWhat
      | otherwise = f x y z

    coordsInCube :: Cube -> Int -> Int -> Int -> Bool
    coordsInCube Cube {cubeX, cubeY, cubeZ} x y z
      | x `inRange` cubeX && y `inRange` cubeY && z `inRange` cubeZ = True
      | otherwise = False

    inRange :: Int -> Range Int -> Bool
    inRange i Range {rangeMin, rangeMax}
      | i >= rangeMin && i <= rangeMax = True
      | otherwise = False

----------------------------------------------------------------
-- Solution2

solution2 :: Input -> Int
solution2 =
  getSum
    . foldMap toVolume
    . foldl' go []
  where
    go :: [Cube] -> Instruction -> [Cube]
    go cubes i = step i cubes

    toVolume :: Cube -> Sum Int
    toVolume Cube {cubeX, cubeY, cubeZ} =
      Sum $ product $ (\Range {..} -> 1 + rangeMax - rangeMin) <$> [cubeX, cubeY, cubeZ]

step :: Instruction -> [Cube] -> [Cube]
step Instruction {instrCube, instrWhat = False} [] = []
step Instruction {instrCube, instrWhat = True} [] = [instrCube]
step i@Instruction {instrCube, instrWhat} (c : cs)
  | disjoint instrCube c = c : step i cs
  | c `subset` instrCube = step i cs
  | instrCube `subset` c && instrWhat = c : cs
  | minX c < minX instrCube =
    let (c1, c2) = splitX (minX instrCube) c
     in c1 : step i (c2 : cs)
  | minY c < minY instrCube =
    let (c1, c2) = splitY (minY instrCube) c
     in c1 : step i (c2 : cs)
  | minZ c < minZ instrCube =
    let (c1, c2) = splitZ (minZ instrCube) c
     in c1 : step i (c2 : cs)
  | maxX c > maxX instrCube =
    let (c1, c2) = splitX (maxX instrCube + 1) c
     in c2 : step i (c1 : cs)
  | maxY c > maxY instrCube =
    let (c1, c2) = splitY (maxY instrCube + 1) c
     in c2 : step i (c1 : cs)
  | maxZ c > maxZ instrCube =
    let (c1, c2) = splitZ (maxZ instrCube + 1) c
     in c2 : step i (c1 : cs)
  | otherwise = error "unmatched case xx"

minX :: Cube -> Int
minX = rangeMin . cubeX

minY :: Cube -> Int
minY = rangeMin . cubeY

minZ :: Cube -> Int
minZ = rangeMin . cubeZ

maxX :: Cube -> Int
maxX = rangeMax . cubeX

maxY :: Cube -> Int
maxY = rangeMax . cubeY

maxZ :: Cube -> Int
maxZ = rangeMax . cubeZ

splitX :: Int -> Cube -> (Cube, Cube)
splitX x Cube {cubeX, cubeY, cubeZ}
  | rangeMin cubeX < x && x <= rangeMax cubeX =
    ( Cube {cubeX = Range {rangeMin = rangeMin cubeX, rangeMax = x - 1}, ..},
      Cube {cubeX = Range {rangeMin = x, rangeMax = rangeMax cubeX}, ..}
    )
  | otherwise = error $ "bad range x: " <> show x <> " .. " <> show cubeX

splitY :: Int -> Cube -> (Cube, Cube)
splitY y Cube {cubeX, cubeY, cubeZ}
  | rangeMin cubeY < y && y <= rangeMax cubeY =
    ( Cube {cubeY = Range {rangeMin = rangeMin cubeY, rangeMax = y - 1}, ..},
      Cube {cubeY = Range {rangeMin = y, rangeMax = rangeMax cubeY}, ..}
    )
  | otherwise = error "bad range y"

splitZ :: Int -> Cube -> (Cube, Cube)
splitZ z Cube {cubeX, cubeY, cubeZ}
  | rangeMin cubeZ < z && z <= rangeMax cubeZ =
    ( Cube {cubeZ = Range {rangeMin = rangeMin cubeZ, rangeMax = z - 1}, ..},
      Cube {cubeZ = Range {rangeMin = z, rangeMax = rangeMax cubeZ}, ..}
    )
  | otherwise = error "bad range z"

subset :: Cube -> Cube -> Bool
subset
  Cube {cubeX = x1, cubeY = y1, cubeZ = z1}
  Cube {cubeX = x2, cubeY = y2, cubeZ = z2} =
    ss x1 x2 && ss y1 y2 && ss z1 z2
    where
      ss :: Range Int -> Range Int -> Bool
      ss r1 r2 = ((<=) `on` rangeMin) r2 r1 && ((<=) `on` rangeMax) r1 r2

disjoint :: Cube -> Cube -> Bool
disjoint
  Cube {cubeX = x1, cubeY = y1, cubeZ = z1}
  Cube {cubeX = x2, cubeY = y2, cubeZ = z2} =
    dj x1 x2 || dj y1 y2 || dj z1 z2
    where
      dj :: Range Int -> Range Int -> Bool
      dj
        Range {rangeMin = a1, rangeMax = b1}
        Range {rangeMin = a2, rangeMax = b2} = (b1 < a2) || (b2 < a1)

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day22-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)

-- putStrLn $ "Solution 2: " <> show (solution2 input)
