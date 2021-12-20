{-# LANGUAGE OverloadedStrings #-}

module Day161 where

import Control.Monad.Extra (replicateM)
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
data Packet a = Packet
  { pVersion :: Int,
    pContent :: Content a
  }

data Content a
  = Literal a
  | Sum [Packet a]
  | Product [Packet a]
  | Min [Packet a]
  | Max [Packet a]
  | Greater (Packet a) (Packet a)
  | Less (Packet a) (Packet a)
  | Equal (Packet a) (Packet a)

----------------------------------------------------------------
-- Parsers
parsePacket :: Parser (Packet Int)
parsePacket = do
  version <- parse3DigitNumber
  typeId <- parse3DigitNumber
  contents <- case typeId of
    4 -> parseLiteral
    0 -> parseOperator Sum
    1 -> parseOperator Product
    2 -> parseOperator Min
    3 -> parseOperator Max
    5 -> parseOperator (go Greater)
    6 -> parseOperator (go Less)
    7 -> parseOperator (go Equal)
    _ -> error ""
  pure $ Packet version contents
  where
    go :: (Packet Int -> Packet Int -> Content Int) -> [Packet Int] -> Content Int
    go ctor [p1, p2] = ctor p1 p2
    go _ _ = error ""

parse3DigitNumber :: Parser Int
parse3DigitNumber = do
  d3 <- binDigitChar
  d2 <- binDigitChar
  d1 <- binDigitChar
  pure $ stringToInt [d3, d2, d1]

stringToInt :: String -> Int
stringToInt bits =
  foldl (\x (digit, num) -> x + 2 ^ num * toInt digit) 0 $
    zip bits (reverse [0 .. length bits - 1])

toInt :: Char -> Int
toInt '0' = 0
toInt '1' = 1
toInt _ = error "unexpected char"

parseLiteral :: Parser (Content Int)
parseLiteral =
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

parseOperator :: ([Packet Int] -> Content Int) -> Parser (Content Int)
parseOperator ctor = do
  kind <- binDigitChar
  case kind of
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

----------------------------------------------------------------
-- Solution1
solution1 :: Packet Int -> Int
solution1 (Packet v sub) = v + go sub
  where
    go :: Content Int -> Int
    go (Literal _) = 0
    go (Sum xs) = sum . fmap solution1 $ xs
    go (Product xs) = sum . fmap solution1 $ xs
    go (Min xs) = sum . fmap solution1 $ xs
    go (Max xs) = sum . fmap solution1 $ xs
    go (Greater l r) = solution1 l + solution1 r
    go (Less l r) = solution1 l + solution1 r
    go (Equal l r) = solution1 l + solution1 r

----------------------------------------------------------------
-- Solution2
solution2 :: Packet Int -> Int
solution2 (Packet _ sub) = go sub
  where
    go :: Content Int -> Int
    go (Literal n) = n
    go (Sum pas) = sum . fmap solution2 $ pas
    go (Product pas) = product . fmap solution2 $ pas
    go (Min pas) = minimum . fmap solution2 $ pas
    go (Max pas) = maximum . fmap solution2 $ pas
    go (Greater pa pa') = boolToInt $ solution2 pa > solution2 pa'
    go (Less pa pa') = boolToInt $ solution2 pa < solution2 pa'
    go (Equal pa pa') = boolToInt $ solution2 pa == solution2 pa'

    boolToInt :: Bool -> Int
    boolToInt False = 0
    boolToInt True = 1

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.concatMap hexToBinary <$> T.readFile "day16-1-input"
  let input = parse parsePacket "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)
      putStrLn $ "Solution 2: " <> show (solution2 input)

hexToBinary :: Char -> Text
hexToBinary c =
  case c of
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
    _ -> error "unexpected hex char"

-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
