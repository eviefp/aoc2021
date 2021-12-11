{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Monad.Extra (whileM)
import Control.Monad.Trans.State.Strict (State, execState, get, modify, put, runState)
import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Day9 (Point (..))
import qualified Debug.Trace as D
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types

----------------------------------------------------------------
-- Parsers
parseInput :: Parser (HashMap Point Int)
parseInput = go <$> many parseLine
  where
    go :: [[Int]] -> HashMap Point Int
    go = HM.fromList . foldMap (zipWith zipper [0 ..]) . transpose . fmap (zip [0 ..])

    zipper :: Int -> (Int, Int) -> (Point, Int)
    zipper y (x, value) = (Point x y, value)

parseLine :: Parser [Int]
parseLine = do
  line <- many digitChar
  newline
  pure $ read . pure <$> line

----------------------------------------------------------------
-- Solution1

solution1 :: HashMap Point Int -> Int
solution1 hm = execState (go steps hm) 0
  where
    go :: Int -> HashMap Point Int -> State Int (HashMap Point Int)
    go 0 h = pure h
    go n h = do
      r <- step h
      get >>= \x -> D.traceM (show (steps - n) <> ": " <> show x)
      go (n -1) r
    steps = 100

step :: HashMap Point Int -> State Int (HashMap Point Int)
step hm = do
  let hm' = (+ 1) <$> hm
  let (flashes, hm'') = execState (whileM go) (0, hm')
  modify (+ flashes)
  pure $ (\x -> if x < 0 then 0 else x) <$> hm''
  where
    go :: State (Int, HashMap Point Int) Bool
    go = do
      (current, hm') <- get
      case HM.foldMapWithKey (\k v -> [k | v > 9]) hm' of
        [] -> pure False
        xs -> do
          let primaryFlash = foldr (\k m -> HM.insert k (-999999999) m) hm' xs
              neighbours = foldMap findNeighbours xs
              flashEcho = foldr (HM.update (Just . (+ 1))) primaryFlash neighbours
          put (current + length xs, flashEcho)
          pure True

    findNeighbours :: Point -> [Point]
    findNeighbours (Point y x) =
      [ Point (y -1) (x -1),
        Point (y -1) x,
        Point (y -1) (x + 1),
        Point y (x -1),
        Point y (x + 1),
        Point (y + 1) (x -1),
        Point (y + 1) x,
        Point (y + 1) (x + 1)
      ]

printMap :: HashMap Point Int -> String
printMap hm =
  let lst = HM.toList hm
      line1 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 0]
      line2 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 1]
      line3 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 2]
      line4 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 3]
      line5 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 4]
      line6 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 5]
      line7 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 6]
      line8 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 7]
      line9 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 8]
      line10 = foldMap (go . fst) . sortOn snd $ [(v, x) | (Point x y, v) <- lst, y == 9]
   in unlines [line1, line2, line3, line4, line5, line6, line7, line8, line9, line10]
  where
    go :: Int -> String
    go x
      | x < 0 = "?"
      | x > 9 = "@"
      | otherwise = show x

----------------------------------------------------------------
-- Solution2
solution2 :: HashMap Point Int -> Int
solution2 hm = execState (go 1 hm) 0
  where
    go :: Int -> HashMap Point Int -> State Int (HashMap Point Int)
    go n h = do
      prev <- get
      r <- step h
      now <- get
      if now - prev == 100
        then do
          put n
          pure r
        else go (n + 1) r

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day11-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right lines -> do
      putStrLn $ "Solution 1: " <> show (solution1 lines)
      putStrLn $ "Solution 2: " <> show (solution2 lines)
