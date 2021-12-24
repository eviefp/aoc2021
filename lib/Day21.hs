{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day21 where

import Control.Monad.Extra (whileM)
import Control.Monad.State.Strict (State, evalState, execState, get, gets, modify, put)
import Data.Foldable
import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D

----------------------------------------------------------------
-- Types
data Player = Player
  { pPosition :: Int,
    pScore :: Sum Int
  }
  deriving (Show)

data Die = Die
  { dieRolls :: Int,
    dieLastRoll :: Int
  }
  deriving (Show)

data GameState = GameState
  { player1 :: Player,
    player2 :: Player,
    die :: Die
  }
  deriving (Show)

----------------------------------------------------------------
-- Parsers

----------------------------------------------------------------
-- Solution1

startGame1 :: GameState
startGame1 =
  GameState
    { player1 = Player {pPosition = 6, pScore = 0},
      player2 = Player {pPosition = 10, pScore = 0},
      die = Die {dieRolls = 0, dieLastRoll = 100}
    }

stepGame :: State GameState Bool
stepGame = do
  player1Turn
  didPlayer1Win >>= \case
    True -> pure True
    False -> do
      player2Turn
      didPlayer2Win

player1Turn :: State GameState ()
player1Turn = do
  result <- rollDie
  movePlayer1 result
  where
    movePlayer1 :: Int -> State GameState ()
    movePlayer1 amount = do
      state <- get
      let newP1 = moveP1 amount (player1 state)
      put $ state {player1 = newP1}

    moveP1 :: Int -> Player -> Player
    moveP1 amt Player {pPosition, pScore} =
      let s = (amt + pPosition) `mod` 10
          newPos = if s == 0 then 10 else s
       in Player {pPosition = newPos, pScore = pScore + Sum newPos}

rollDie :: State GameState Int
rollDie = do
  d1 <- roll
  d2 <- roll
  d3 <- roll
  pure $ d1 + d2 + d3
  where
    roll :: State GameState Int
    roll = do
      state <- get
      let newDie = rollPure (die state)
      put $ state {die = newDie}
      pure $ dieLastRoll newDie

    rollPure :: Die -> Die
    rollPure Die {dieRolls, dieLastRoll}
      | dieLastRoll == 100 = Die {dieRolls = dieRolls + 1, dieLastRoll = 1}
      | otherwise = Die {dieRolls = dieRolls + 1, dieLastRoll = dieLastRoll + 1}

didPlayer1Win :: State GameState Bool
didPlayer1Win = do
  gets ((>= 1000) . pScore . player1)

player2Turn :: State GameState ()
player2Turn = do
  result <- rollDie
  movePlayer2 result
  where
    movePlayer2 :: Int -> State GameState ()
    movePlayer2 amount = do
      state <- get
      let newP2 = moveP2 amount (player2 state)
      put $ state {player2 = newP2}

    moveP2 :: Int -> Player -> Player
    moveP2 amt Player {pPosition, pScore} =
      let s = (amt + pPosition) `mod` 10
          newPos = if s == 0 then 10 else s
       in Player {pPosition = newPos, pScore = pScore + Sum newPos}

didPlayer2Win :: State GameState Bool
didPlayer2Win = gets ((>= 1000) . pScore . player2)

solution1 :: Int
solution1 = calc $ (`execState` startGame1) (whileM (not <$> stepGame))
  where
    calc :: GameState -> Int
    calc GameState {player1, player2, die} =
      losingPlayerScore player1 player2 * dieRolls die

    losingPlayerScore :: Player -> Player -> Int
    losingPlayerScore = (getSum .) . min `on` pScore

----------------------------------------------------------------
-- Solution2

type MemoKey = ((Integer, Integer), (Integer, Integer), Bool)

type Memo = M.Map MemoKey (Integer, Integer)

solution2 :: Integer
solution2 = uncurry max $ evalState (go ((6, 0), (10, 0), True)) M.empty
  where
    go :: MemoKey -> State Memo (Integer, Integer)
    go key =
      gets (M.lookup key)
        >>= (`maybe` return) (eval key >>= \res -> modify (M.insert key res) >> return res)
    eval :: MemoKey -> State Memo (Integer, Integer)
    eval ((space, score), opponent, turn)
      | snd opponent >= 21 = return $ if turn then (0, 1) else (1, 0)
      | otherwise =
        foldl1 (\(s1, s2) (w1, w2) -> (s1 + w1, s2 + w2))
          <$> sequence
            [ let step = (space + dice - 1) `mod` 10 + 1
               in do
                    (w1, w2) <- go (opponent, (step, score + step), not turn)
                    return (n * w1, n * w2)
              | (dice, n) <- [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
            ]

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  putStrLn $ "Solution 1: " <> show solution1
  putStrLn $ "Solution 2: " <> show solution2
