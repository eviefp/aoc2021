{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Control.Newtype.Generics (Newtype, over2)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import GHC.Generics (Generic)
import Text.Megaparsec hiding (mkPos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

----------------------------------------------------------------
-- Parsers

parseInput :: Parser [([String], [String])]
parseInput = many parseLine

parseLine :: Parser ([String], [String])
parseLine = do
  inputs <- many (parseSegment <* hspace1)
  char '|'
  outputs <- many (hspace1 *> parseSegment)
  newline
  pure (inputs, outputs)

parseSegment :: Parser String
parseSegment = many letterChar

----------------------------------------------------------------
-- Solution1

is1478 :: String -> Bool
is1478 s =
  let len = length s
   in len `elem` [2, 4, 3, 7]

solution1 :: [([String], [String])] -> Int
solution1 = length . filter id . fmap is1478 . foldMap snd

----------------------------------------------------------------
-- Solution2

data Signal = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show)

top :: PossibleSignals
top = PossibleSignals $ Set.fromList [A, B, C, D, E, F, G]

newtype Digit = Digit
  { getSignal :: Set Signal
  }
  deriving newtype (Show)

charToDigit :: Char -> Signal
charToDigit c
  | c == 'a' = A
  | c == 'b' = B
  | c == 'c' = C
  | c == 'd' = D
  | c == 'e' = E
  | c == 'f' = F
  | c == 'g' = G
  | otherwise = error "charToDigit"

newtype WonkyDigit = WonkyDigit
  { getWonkyDigit :: Set Signal
  }

toWonkyDigit :: String -> WonkyDigit
toWonkyDigit = WonkyDigit . Set.fromList . fmap charToDigit

newtype PossibleSignals = PossibleSignals
  { getPossibleSignals :: Set Signal
  }
  deriving (Eq, Ord, Generic)

instance Newtype PossibleSignals

instance Semigroup PossibleSignals where
  (<>) = over2 PossibleSignals (Set.intersection @Signal)

instance Monoid PossibleSignals where
  mempty = top

newtype ConjunctionMap = ConjunctionMap
  { getCMap :: Signal -> PossibleSignals
  }

instance Semigroup ConjunctionMap where
  (ConjunctionMap f) <> (ConjunctionMap g) = ConjunctionMap $ \x -> f x <> g x

instance Monoid ConjunctionMap where
  mempty = ConjunctionMap mempty

newtype DigitMapping = DigitMapping
  { getDigitMapping :: Signal -> Signal
  }

solution2 :: [([String], [String])] -> [Char]
solution2 input = fmap digitToSum $ head $ thing <$> typed'
  where
    typed :: [([WonkyDigit], [WonkyDigit])]
    typed = bimap (fmap toWonkyDigit) (fmap toWonkyDigit) <$> input

    typed' :: [(DigitMapping, [WonkyDigit])]
    typed' = first solveWonkyDigits <$> typed

    thing :: (DigitMapping, [WonkyDigit]) -> [Digit]
    thing (dm, wds) = translate dm <$> wds

    solveWonkyDigits :: [WonkyDigit] -> DigitMapping
    solveWonkyDigits =
      DigitMapping
        . fmap resolveSignal
        . getCMap
        . foldMap (toMapping . fmap splitBySignals)
        . groupByNumberOfDigits

    translate :: DigitMapping -> WonkyDigit -> Digit
    translate (DigitMapping f) = Digit . Set.fromList . fmap f . Set.toList . getWonkyDigit

    groupByNumberOfDigits :: [WonkyDigit] -> [(Int, [WonkyDigit])]
    groupByNumberOfDigits =
      fmap go
        . groupBy ((==) `on` fst)
        . sortOn fst
        . fmap (\wd -> (wonkyLength wd, wd))

    splitBySignals :: [WonkyDigit] -> Signal -> Int
    splitBySignals wds signal = length $ filter (hasSignal signal) wds

    toMapping :: (Int, Signal -> Int) -> ConjunctionMap
    toMapping (digit, f) =
      ConjunctionMap $
        \s -> case (digit, f s) of
          (2, 0) -> mkNotPos [C, F]
          (2, 1) -> mkPos [C, F]
          (3, 0) -> mkNotPos [A, C, F]
          (3, 1) -> mkPos [A, C, F]
          (4, 0) -> mkNotPos [B, C, D, F]
          (4, 1) -> mkPos [B, C, D, F]
          (5, 1) -> mkPos [B, E]
          (5, 2) -> mkPos [C, F]
          (5, 3) -> mkPos [A, D, G]
          (6, 2) -> mkPos [C, D, E]
          (6, 3) -> mkPos [A, B, F, G]
          (7, 1) -> mkPos [A, B, C, D, E, F, G]
          (x, y) -> error $ "tomapping: " <> show x <> " " <> show y

    resolveSignal :: PossibleSignals -> Signal
    resolveSignal (PossibleSignals ps)
      | Set.size ps == 1 = Set.findMin ps
      | otherwise = error "could not solve"

mkPos :: [Signal] -> PossibleSignals
mkPos = PossibleSignals . Set.fromList

mkNotPos :: [Signal] -> PossibleSignals
mkNotPos =
  PossibleSignals . Set.difference (Set.fromList [A, B, C, D, E, F, G]) . Set.fromList

hasSignal :: Signal -> WonkyDigit -> Bool
hasSignal s (WonkyDigit wd) = Set.member s wd

go :: [(Int, WonkyDigit)] -> (Int, [WonkyDigit])
go xs = (fst $ head xs, fmap snd xs)

wonkyLength :: WonkyDigit -> Int
wonkyLength = Set.size . getWonkyDigit

digitToSum :: Digit -> Char
digitToSum (Digit d)
  | d == Set.fromList [A, B, C, E, F, G] = '0'
  | d == Set.fromList [C, F] = '1'
  | d == Set.fromList [A, C, D, E, G] = '2'
  | d == Set.fromList [A, C, D, F, G] = '3'
  | d == Set.fromList [B, C, D, F] = '4'
  | d == Set.fromList [A, B, D, F, G] = '5'
  | d == Set.fromList [A, B, D, E, F, G] = '6'
  | d == Set.fromList [A, C, F] = '7'
  | d == Set.fromList [A, B, C, D, E, F, G] = '8'
  | d == Set.fromList [A, B, C, D, F, G] = '9'
  | otherwise = error (show d)

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day8-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 test)

test :: [([String], [String])]
test = pure (["gea", "cbadfeg", "ae", "ecgdbf", "egdcf", "gfbac", "bafdeg", "facdeg", "eacfg", "acde"], ["edfgc", "facgb", "fgace"])
