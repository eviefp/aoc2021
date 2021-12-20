{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day18 where

import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.State.Strict (State, evalState)
import Data.Either (fromRight)
import Data.Either.Extra (fromRight')
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = ParsecT Void Text (State [Path]) a

----------------------------------------------------------------
-- Types
data Path = L | R
  deriving (Eq, Ord, Show)

data SDigit = SDigit
  { snPath :: [Path],
    snValue :: Int
  }
  deriving (Eq, Ord, Show)

type SNumber = [SDigit]

type Input = [SNumber]

----------------------------------------------------------------
-- Parsers
parseInput :: Parser Input
parseInput = many (parseSDigit <* space)

parseSDigit :: Parser SNumber
parseSDigit = parseNumber <|> parsePair

parseNumber :: Parser SNumber
parseNumber =
  fmap pure $ gets SDigit <*> decimal

parsePair :: Parser SNumber
parsePair = do
  char '['
  modify (L :)
  left <- parseSDigit
  char ','
  modify ((R :) . tail)
  right <- parseSDigit
  char ']'
  modify tail
  pure $ left <> right

----------------------------------------------------------------
-- Solution1

reduce :: SNumber -> SNumber
reduce n
  | any needsExplode n = reduce $ explode n
  | any needsSplit n = reduce $ split n
  | otherwise = n
  where
    needsSplit :: SDigit -> Bool
    needsSplit = (> 9) . snValue

    needsExplode :: SDigit -> Bool
    needsExplode = (> 4) . length . snPath

    explode :: SNumber -> SNumber
    explode (d1 : d2 : d3 : d4 : xs)
      | needsExplode d2 && arePair d2 d3 = explodeAdd d1 d2 : mkZero d2 : explodeAdd d4 d3 : xs
    explode (d2 : d3 : d4 : xs)
      | needsExplode d2 && arePair d2 d3 = mkZero d2 : explodeAdd d4 d3 : xs
    explode [d1, d2, d3]
      | needsExplode d2 && arePair d2 d3 = [explodeAdd d1 d2, mkZero d2]
    explode (x : xs) = x : explode xs
    explode [] = []

    mkZero :: SDigit -> SDigit
    mkZero SDigit {snPath, snValue} =
      SDigit {snPath = tail snPath, snValue = 0}

    arePair :: SDigit -> SDigit -> Bool
    arePair = (==) `on` (tail . snPath)

    explodeAdd :: SDigit -> SDigit -> SDigit
    explodeAdd (SDigit pas n) (SDigit _ n') = SDigit {snPath = pas, snValue = n + n'}

    split :: SNumber -> SNumber
    split [] = []
    split (x : xs)
      | needsSplit x = mkSplit x <> xs
      | otherwise = x : split xs
      where
        mkSplit :: SDigit -> [SDigit]
        mkSplit SDigit {snPath, snValue} =
          [ SDigit {snPath = L : snPath, snValue = snValue `div` 2},
            SDigit {snPath = R : snPath, snValue = uncurry (+) (snValue `divMod` 2)}
          ]

add :: SNumber -> SNumber -> SNumber
add left right = reduce $ (prependPath L <$> left) <> (prependPath R <$> right)
  where
    prependPath :: Path -> SDigit -> SDigit
    prependPath p SDigit {snPath, snValue} =
      SDigit {snPath = snPath <> [p], snValue = snValue}

magnitude :: SNumber -> Int
magnitude = sum . fmap digitMagnitude
  where
    digitMagnitude :: SDigit -> Int
    digitMagnitude SDigit {snPath, snValue} =
      product $ snValue : fmap pathMagnitude snPath

    pathMagnitude :: Path -> Int
    pathMagnitude =
      \case
        L -> 3
        R -> 2

solution1 :: Input -> Int
solution1 = magnitude . foldl1 add

----------------------------------------------------------------
-- Solution2

solution2 :: Input -> Int
solution2 = maximum . fmap (magnitude . uncurry add) . zipNums
  where
    zipNums :: Input -> [(SNumber, SNumber)]
    zipNums input =
      [ (x, y)
        | x <- input,
          y <- input,
          x /= y
      ]

--------- ------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day18-1-input"
  let input = (`evalState` []) $ runParserT parseInput "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)

testData :: SNumber
testData =
  head $
    fromRight' $
      (`evalState` []) $
        runParserT parseInput "" $
          "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

small :: SNumber
small = [SDigit [L] 1, SDigit [R] 1]
