{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Bool (bool)
import Data.List (transpose)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main :: IO ()
main = do
  contents <- T.readFile "day3-1-input"
  let input = parseMaybe parseInput contents
  case input of
    Nothing -> error "bad parse"
    Just parseResult -> do
      let transposed = transpose parseResult
          -- oxygen = it (maybe True id) transposed 0
          co2 = it (maybe False not) transposed 0
      -- print $ toNumber' $ transpose oxygen
      print $ transpose co2

-- print $ toNumber' $ transpose co2

-- let result = toNumber' (transpose oxygen) * toNumber' (transpose co2)
-- print result

type Parser a = Parsec Void Text a

parseInput :: Parser [[Bool]]
parseInput = many parseLine

parseLine :: Parser [Bool]
parseLine = do
  number <- many parseBinDigit
  space1
  pure number

parseBinDigit :: Parser Bool
parseBinDigit = (False <$ char '0') <|> (True <$ char '1')

toSum :: Bool -> Sum Int
toSum = bool (-1) 1

fromSum :: Sum Int -> Maybe Bool
fromSum x
  | x == 0 = Nothing
  | otherwise = Just $ x > 0

toNumber :: [Bool] -> Int
toNumber = foldl (\x digit -> 2 * x + bool 1 0 digit) 0

toNumber' :: [[Bool]] -> Int
toNumber' [x] = toNumber x
toNumber' _ = error "expected single result"

it :: (Maybe Bool -> Bool) -> [[Bool]] -> Int -> [[Bool]]
it _ [x] _ = [x]
it keep xs idx
  | idx < length xs =
    let result = transpose $ filter ((== keep commonDigit) . (!! idx)) $ transpose xs
     in D.trace (stuff result) $
          if length (head result) /= length (head xs)
            then it keep result 0
            else it keep result (idx + 1)
  | otherwise = xs
  where
    commonDigit = findMostCommonDigit $ xs !! idx
    stuff :: [[Bool]] -> String
    stuff result =
      unlines
        [ printLine $ xs !! idx,
          show idx,
          printLine $ result !! idx,
          show commonDigit,
          printLine $ head $ transpose result,
          "--------------------------------"
        ]

printLine :: [Bool] -> String
printLine = foldMap (bool "0" "1")

findMostCommonDigit :: [Bool] -> Maybe Bool
findMostCommonDigit = fromSum . foldMap toSum

-- |
--  - findMostCommonDigit should be [Bool] -> Maybe Bool (where Nothing means "just as many")
--  - we don't need to pass the f & g functions to 'it'
--  - we need to pass a way to figure out if we keep most or least common
--  - we need to pass a way to figure out what we do in case of "just as many"
