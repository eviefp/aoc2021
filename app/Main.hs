module Main where

import Data.Foldable
import Data.Monoid

main :: IO ()
main = solution2'

{-
1
2
1
4
5

1 < 2  ? True
2 < 1  ? False
1 < 4  ? True
4 < 5  ? True
5
-}
solution1 :: IO ()
solution1 = do
  numberList <- getListOfDepths
  let increases = zipWith (<) numberList (tail numberList)
      numberOfIncreases = length $ filter (== True) increases

  print numberOfIncreases

solution1' :: IO ()
solution1' = do
  numberList <- getListOfDepths
  let increases = fold $ zipWith go numberList (tail numberList)

  print increases
  where
    go a b
      | a < b = Sum 1
      | otherwise = Sum 0

{-
1
2
1
4
5

1 + 2 + 1   =  ????
2 + 1 + 4
1 + 4 + 5
4 + 5
5

Alternate idea:
------------

A = a + b + c
B = b + c + d

'b' and 'c' are irrelevant so we can just compare 'a' and 'd'
-}
solution2 :: IO ()
solution2 = do
  numberList <- getListOfDepths
  let slidingWindowList =
        zipWith3
          add3
          numberList
          (tail numberList)
          (drop 2 numberList)
      increases = zipWith (<) slidingWindowList (tail slidingWindowList)
      numberOfIncreases = length $ filter (== True) increases

  print numberOfIncreases
  where
    add3 :: Int -> Int -> Int -> Int
    add3 a b c = a + b + c

getListOfDepths :: IO [Int]
getListOfDepths = do
  contents <- readFile "day1-1-input"
  pure $ read <$> lines contents

solution2' :: IO ()
solution2' = do
  numberList <- getListOfDepths
  let increases = fold $ zipWith go numberList (drop 3 numberList)

  print increases
  where
    go a b
      | a < b = Sum 1
      | otherwise = Sum 0
