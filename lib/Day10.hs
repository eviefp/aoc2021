{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day10 where

import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Maybe (mapMaybe)
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

data Chunk = Parens | Bracket | Curly | Angled
  deriving (Eq, Show)

newtype ChunkStatus = ChunkStatus
  { openChunks :: [Chunk]
  }

data ChunkError = BadTerminator Chunk | UnexpectedChar Char | Whatever
  deriving (Show)

----------------------------------------------------------------
-- Parsers

parseInput :: Parser [String]
parseInput = many parseLine

parseLine :: Parser String
parseLine = many (satisfy isParens) <* newline
  where
    isParens :: Char -> Bool
    isParens = (`elem` ['(', ')', '<', '>', '[', ']', '{', '}'])

----------------------------------------------------------------
-- Solution1
solution1 :: [String] -> Int
solution1 = getSum . foldMap score . mapMaybe checkLine

score :: ChunkError -> Sum Int
score =
  \case
    BadTerminator ch ->
      case ch of
        Parens -> 3
        Bracket -> 57
        Curly -> 1197
        Angled -> 25137
    _ -> 0

checkLine :: String -> Maybe ChunkError
checkLine = go . foldlM checkChunk (ChunkStatus [])
  where
    go :: Either ChunkError ChunkStatus -> Maybe ChunkError
    go =
      \case
        Left x -> Just x
        _ -> Nothing

checkChunk :: ChunkStatus -> Char -> Either ChunkError ChunkStatus
checkChunk cs =
  \case
    '(' -> Right $ pushStatus Parens cs
    '[' -> Right $ pushStatus Bracket cs
    '{' -> Right $ pushStatus Curly cs
    '<' -> Right $ pushStatus Angled cs
    ')' -> popStatus Parens cs
    ']' -> popStatus Bracket cs
    '}' -> popStatus Curly cs
    '>' -> popStatus Angled cs
    c -> Left $ UnexpectedChar c

popStatus :: Chunk -> ChunkStatus -> Either ChunkError ChunkStatus
popStatus c (ChunkStatus s) =
  case s of
    (top : rest)
      | c == top -> Right $ ChunkStatus rest
      | otherwise -> Left $ BadTerminator c
    _ -> Left Whatever

pushStatus :: Chunk -> ChunkStatus -> ChunkStatus
pushStatus c = ChunkStatus . (c :) . openChunks

----------------------------------------------------------------
-- Solution2

solution2 :: [String] -> Int
solution2 = findValue . sort . fmap autoCompleteScore . mapMaybe getLineState

findValue :: [Int] -> Int
findValue xs =
  let len = length xs
      mid = toRational len / 2
   in xs !! floor mid

getLineState :: String -> Maybe ChunkStatus
getLineState = go . foldlM checkChunk (ChunkStatus [])
  where
    go :: Either ChunkError ChunkStatus -> Maybe ChunkStatus
    go (Left _) = Nothing
    go (Right cs) = Just cs

autoCompleteScore :: ChunkStatus -> Int
autoCompleteScore = foldl go 0 . openChunks
  where
    go :: Int -> Chunk -> Int
    go acc chunk = acc * 5 + acScore chunk

    acScore :: Chunk -> Int
    acScore =
      \case
        Parens -> 1
        Bracket -> 2
        Curly -> 3
        Angled -> 4

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day10-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 lines)

-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
testData :: [String]
testData =
  [ "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]
