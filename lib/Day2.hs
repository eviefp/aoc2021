{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day2 where

import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

solution1 :: IO ()
solution1 = do
  contents <- T.readFile "day2-1-input"
  let directions = parseMaybe parseDirections contents
  case directions of
    Nothing -> putStrLn "Parse error"
    Just result -> do
      let endPosition = foldl go mempty result
          answer = getSum (forward endPosition) * getSum (depth endPosition)
      print answer
  where
    go :: Position -> Direction -> Position
    go current@Position {..} d =
      let delta = directionToPosition (getSum aim) d
       in current <> delta

data Position = Position
  { forward :: Sum Int,
    depth :: Sum Int,
    aim :: Sum Int
  }
  deriving (Show)

-- TODO: ask
data Position' a = Position' a a a
  deriving (Show)
  deriving (Semigroup, Monoid) via (P3 a)

-- deriving newtype (Semigroup, Monoid)

newtype P3 a = P3 (Position' a)

instance Semigroup a => Semigroup (P3 a) where
  (P3 (Position' a b c)) <> (P3 (Position' d e f)) = P3 $ Position' (a <> d) (b <> e) (c <> f)

instance Monoid a => Monoid (P3 a) where
  mempty = P3 $ Position' mempty mempty mempty

instance Semigroup Position where
  p1 <> p2 =
    Position (forward p1 <> forward p2) (depth p1 <> depth p2) (aim p1 <> aim p2)

instance Monoid Position where
  mempty = Position mempty mempty mempty

data Direction = Forward Int | Up Int | Down Int
  deriving (Show)

type Parser a = Parsec Void Text a

parseDirections :: Parser [Direction]
parseDirections = many parseDirection

parseDirection :: Parser Direction
parseDirection = go "forward" Forward <|> go "up" Up <|> go "down" Down
  where
    go :: Text -> (Int -> Direction) -> Parser Direction
    go text ctor = do
      string text
      space1
      value <- decimal
      space1
      pure $ ctor value

directionToPosition :: Int -> Direction -> Position
directionToPosition aim =
  \case
    Forward n -> Position (Sum n) (Sum (aim * n)) 0
    Up n -> Position 0 0 (Sum (- n))
    Down n -> Position 0 0 (Sum n)
