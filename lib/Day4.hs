{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.Foldable
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

------------------------------

newtype Board = Board [[Int]]
  deriving (Show, Eq)

didItWin :: [Int] -> Board -> Bool
didItWin numbers (Board board) = anyRowIsComplete || anyColumnIsComplete
  where
    isRowComplete :: [Int] -> Bool
    isRowComplete row = all (`elem` numbers) row

    anyRowIsComplete :: Bool
    anyRowIsComplete = getAny $ foldMap (Any . isRowComplete) board

    anyColumnIsComplete :: Bool
    anyColumnIsComplete = getAny . foldMap (Any . isRowComplete) $ transpose board

type Parser a = Parsec Void Text a

parseInput :: Parser ([Board], [Int])
parseInput = do
  numbers <- many parseNumbers
  newline
  boards <- many parseBoard
  pure (boards, numbers)

parseNumbers :: Parser Int
parseNumbers = do
  number <- decimal
  _ <- optional $ char ','
  pure number

parseBoard :: Parser Board
parseBoard = do
  newline
  lines <- many parseLine
  pure $ Board lines

parseLine :: Parser [Int]
parseLine = do
  hspace
  first <- decimal
  rest <- many (hspace *> decimal)
  newline
  pure $ first : rest

------------------------------

main :: IO ()
main = do
  contents <- T.readFile "day4-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> print err
    Right (boards, numbers) -> do
      let (result, _) = foldl (go boards) ([], []) (inits numbers)
      print $ calculatePart1 $ last result

-- print $ calculatePart1 result

calculatePart1 :: (Board, [Int]) -> Int
calculatePart1 (Board board, numbers) = sum' * last numbers
  where
    sum' = sum . filter (not . (`elem` numbers)) $ fold board

go :: [Board] -> ([(Board, [Int])], [Board]) -> [Int] -> ([(Board, [Int])], [Board])
go boards (results, lookupBoard) numbers =
  (results ++ ((,numbers) <$> boardsThatWonNow), lookupBoard ++ boardsThatWonNow)
  where
    winningBoardsThisRound = filter (didItWin numbers) boards
    boardsThatWonNow = filter (not . (`elem` lookupBoard)) winningBoardsThisRound
