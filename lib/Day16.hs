{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day16 where

import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types
data Packet = Packet
  { pkVersion :: Version,
    pkContent :: Content
  }
  deriving (Show)

newtype Version = Version
  { getVersion :: Int
  }
  deriving (Show)

data Content
  = Literal Int
  | Sum [Packet]
  | Product [Packet]
  | Minimum [Packet]
  | Maximum [Packet]
  | Greater [Packet]
  | Less [Packet]
  | Equals [Packet]
  deriving (Show)

----------------------------------------------------------------
-- Parsers
parsePacket :: Parser Packet
parsePacket = do
  version <- Version <$> parse3DigitNumber
  typeId <- parse3DigitNumber
  contents <- case typeId of
    4 -> parseLiteral
    0 -> parseOperator Sum
    1 -> parseOperator Product
    2 -> parseOperator Minimum
    3 -> parseOperator Maximum
    5 -> parseOperator Greater
    6 -> parseOperator Less
    7 -> parseOperator Equals
  pure $ Packet version contents

parseOperator :: ([Packet] -> Content) -> Parser Content
parseOperator ctor =
  binDigitChar >>= \case
    '0' -> parseType0
    '1' -> parseType1
    c -> error $ "[PO] bad type: " <> show c
  where
    parseBits :: Int -> Parser String
    parseBits 0 = pure ""
    parseBits n = (:) <$> binDigitChar <*> parseBits (n -1)

    parseType0 :: Parser Content
    parseType0 = do
      length <- stringToInt <$> parseBits 15
      contents <- T.pack <$> parseBits length
      let x = concat $ parseMaybe (many parsePacket) contents
      pure $ ctor x

    parseType1 :: Parser Content
    parseType1 = do
      count <- stringToInt <$> parseBits 11
      ctor <$> replicateM count parsePacket

parse3DigitNumber :: Parser Int
parse3DigitNumber = do
  d3 <- toInt <$> binDigitChar
  d2 <- toInt <$> binDigitChar
  d1 <- toInt <$> binDigitChar
  pure $ d3 * 4 + d2 * 2 + d1

toInt :: Char -> Int
toInt =
  \case
    '0' -> 0
    '1' -> 1
    c -> error $ "Invalid hex char: " <> show c

stringToInt :: String -> Int
stringToInt bits =
  foldl (\x (digit, num) -> x + 2 ^ num * toInt digit) 0 $
    zip bits (reverse [0 .. length bits - 1])

parseLiteral :: Parser Content
parseLiteral = do
  Literal . stringToInt <$> parseBlocks
  where
    parseBlocks :: Parser String
    parseBlocks = do
      isEnd <- binDigitChar
      d4 <- binDigitChar
      d3 <- binDigitChar
      d2 <- binDigitChar
      d1 <- binDigitChar
      if isEnd == '0'
        then pure [d4, d3, d2, d1]
        else ([d4, d3, d2, d1] ++) <$> parseBlocks

----------------------------------------------------------------
-- Solution1

solution1 :: Packet -> Int
solution1 = \case
  (Packet ver con) -> case con of
    (Literal n) -> getVersion ver
    (Sum pas) -> getVersion ver + go pas
    (Product pas) -> getVersion ver + go pas
    (Minimum pas) -> getVersion ver + go pas
    (Maximum pas) -> getVersion ver + go pas
    (Greater pas) -> getVersion ver + go pas
    (Less pas) -> getVersion ver + go pas
    (Equals pas) -> getVersion ver + go pas
  where
    go :: [Packet] -> Int
    go = sum . fmap solution1

----------------------------------------------------------------
-- Solution2
solution2 :: Packet -> Int
solution2 = \case
  (Packet ver con) -> case con of
    (Literal n) -> n
    (Sum pas) -> sum (solution2 <$> pas)
    (Product pas) -> product (solution2 <$> pas)
    (Minimum pas) -> minimum (solution2 <$> pas)
    (Maximum pas) -> maximum (solution2 <$> pas)
    (Greater [p1, p2]) -> bool 0 1 $ solution2 p1 > solution2 p2
    (Less [p1, p2]) -> bool 0 1 $ solution2 p1 < solution2 p2
    (Equals [p1, p2]) -> bool 0 1 $ solution2 p1 == solution2 p2
    _ -> error "unexpected nr of args"

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.concatMap toBinary <$> T.readFile "day16-1-input"
  let input = parse parsePacket "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)
  where
    toBinary :: Char -> Text
    toBinary =
      \case
        '0' -> "0000"
        '1' -> "0001"
        '2' -> "0010"
        '3' -> "0011"
        '4' -> "0100"
        '5' -> "0101"
        '6' -> "0110"
        '7' -> "0111"
        '8' -> "1000"
        '9' -> "1001"
        'A' -> "1010"
        'B' -> "1011"
        'C' -> "1100"
        'D' -> "1101"
        'E' -> "1110"
        'F' -> "1111"
        c -> error $ "Invalid hex char: " <> show c

-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
