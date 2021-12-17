{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Day16 where

import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.Foldable
import Data.Function (on)
import Data.Functor.Foldable (Base, Recursive (cata, project))
import Data.List
import Data.Monoid (Sum (getSum))
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
data Packet a = Packet
  { pkVersion :: Version,
    pkContent :: Content a
  }
  deriving (Show, Functor, Foldable)

newtype Version = Version
  { getVersion :: Int
  }
  deriving (Show)

data Content a
  = Literal a
  | Sum [Packet a]
  | Product [Packet a]
  | Minimum [Packet a]
  | Maximum [Packet a]
  | Greater [Packet a]
  | Less [Packet a]
  | Equals [Packet a]
  deriving (Show, Functor, Foldable)

data PacketV b = PacketV
  { pVer :: Version,
    pRec :: [b]
  }
  deriving (Functor)

type instance Base (Packet a) = PacketV

instance Recursive (Packet a) where
  project Packet {..} = PacketV pkVersion (getPackets pkContent)

data PacketLit a b
  = PLLit a
  | PLSum [b]
  | PLProd [b]
  | PLMin [b]
  | PLMax [b]
  | PLGT b b
  | PLLT b b
  | PLEQ b b
  deriving (Functor)

newtype PacketCalc a = PacketCalc (Packet a)
  deriving (Functor)

type instance Base (PacketCalc a) = PacketLit a

instance Recursive (PacketCalc a) where
  project (PacketCalc Packet {..}) = mkPacketLit pkContent

mkPacketLit :: Content a -> PacketLit a (PacketCalc a)
mkPacketLit =
  \case
    (Literal a) -> PLLit a
    (Sum pas) -> PLSum $ PacketCalc <$> pas
    (Product pas) -> PLProd $ PacketCalc <$> pas
    (Minimum pas) -> PLMin $ PacketCalc <$> pas
    (Maximum pas) -> PLMax $ PacketCalc <$> pas
    (Greater [p1, p2]) -> PLGT (PacketCalc p1) (PacketCalc p2)
    (Less [p1, p2]) -> PLLT (PacketCalc p1) (PacketCalc p2)
    (Equals [p1, p2]) -> PLEQ (PacketCalc p1) (PacketCalc p2)
    _ -> error "unexpected"

getPackets :: Content a -> [Packet a]
getPackets =
  \case
    (Literal a) -> []
    (Sum pas) -> pas
    (Product pas) -> pas
    (Minimum pas) -> pas
    (Maximum pas) -> pas
    (Greater pas) -> pas
    (Less pas) -> pas
    (Equals pas) -> pas

----------------------------------------------------------------
-- Parsers
parsePacket :: Parser (Packet Int)
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

parseOperator :: ([Packet Int] -> Content Int) -> Parser (Content Int)
parseOperator ctor =
  binDigitChar >>= \case
    '0' -> parseType0
    '1' -> parseType1
    c -> error $ "[PO] bad type: " <> show c
  where
    parseBits :: Int -> Parser String
    parseBits 0 = pure ""
    parseBits n = (:) <$> binDigitChar <*> parseBits (n -1)

    parseType0 :: Parser (Content Int)
    parseType0 = do
      length <- stringToInt <$> parseBits 15
      contents <- T.pack <$> parseBits length
      let x = concat $ parseMaybe (many parsePacket) contents
      pure $ ctor x

    parseType1 :: Parser (Content Int)
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

parseLiteral :: Parser (Content Int)
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

solution1 :: Packet Int -> Int
solution1 = cata go
  where
    go :: PacketV Int -> Int
    go (PacketV ver ns) = getVersion ver + sum ns

----------------------------------------------------------------
-- Solution2
solution2 :: Packet Int -> Int
solution2 = cata go . PacketCalc
  where
    go :: PacketLit Int Int -> Int
    go =
      \case
        (PLLit n) -> n
        (PLSum ns) -> sum ns
        (PLProd ns) -> product ns
        (PLMin ns) -> minimum ns
        (PLMax ns) -> maximum ns
        (PLGT n i) -> bool 0 1 $ n > i
        (PLLT n i) -> bool 0 1 $ n < i
        (PLEQ n i) -> bool 0 1 $ n == i

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
