{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day23 where

import Algorithm.Search (dijkstra)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Day9 (Point (..))
import qualified Debug.Trace as D
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types
data Pod = A | B | C | D
  deriving (Show, Eq, Ord)

data Cell = Occupied Pod | Empty
  deriving (Show, Ord, Eq)

data CellPoint = CellPoint
  { cpCell :: Cell,
    cpPoint :: Point
  }
  deriving (Show, Ord, Eq)

data Room = Room
  { getRoom :: [CellPoint],
    roomFor :: Pod
  }
  deriving (Show, Ord, Eq)

data Map = Map
  { getRooms :: [Room],
    getHallway :: [CellPoint]
  }
  deriving (Show, Eq, Ord)

----------------------------------------------------------------
-- Parsers

----------------------------------------------------------------
-- Solution1
findCheapest :: Map -> Int
findCheapest = maybe 0 fst . dijkstra nextStates cost arePodsSorted

nextStates :: Map -> [Map]
nextStates Map {getRooms = rooms, getHallway = hallway} = moveHallway <> moveOutside
  where
    moveHallway :: [Map]
    moveHallway =
      concatMap mkToRoomFrom
        . filter ((/= Empty) . cpCell)
        $ hallway

    moveOutside :: [Map]
    moveOutside = error "not implemented"

    hasPathToRoom :: [Room] -> Cell -> Bool
    hasPathToRoom = error "not implemented"

    mkToRoomFrom :: CellPoint -> [Map]
    mkToRoomFrom cp@(CellPoint (Occupied p) _) =
      let r = findRoomFor p
       in [ mkMap cp r
            | isValid r && pathIsEmpty r cp
          ]

    findRoomFor :: Pod -> Room
    findRoomFor p = fromJust . find ((== p) . roomFor) $ rooms

    isValid :: Room -> Bool
    isValid Room {getRoom = [CellPoint Empty _, CellPoint Empty _]} = True
    isValid Room {getRoom = [CellPoint Empty _, CellPoint (Occupied p) _], roomFor} = roomFor == p
    isValid _ = False

    mkMap :: CellPoint -> Room -> Map
    mkMap = error "not implemented"

    pathIsEmpty :: Room -> CellPoint -> Bool
    pathIsEmpty r CellPoint {cpCell, cpPoint = cp}
      | any (nonEmpty (cpPoint . head . getRoom $ r) cp) hallway = False
      | otherwise = True

    nonEmpty :: Point -> Point -> CellPoint -> Bool
    nonEmpty Point {getX = x1} Point {getX = x2} CellPoint {cpPoint = Point {getX = x}, cpCell}
      | x1 > x2 = x <= x2 || x > x1 || cpCell == Empty
      | otherwise = x < x1 || x >= x2 || cpCell == Empty

cost :: Map -> Map -> Int
cost
  Map {getRooms = currRooms, getHallway = currHallway}
  Map {getRooms = nextRooms, getHallway = nextHallway}
    | currHallway /= nextHallway = (findHalwayCost `on` fmap cpCell) currHallway nextHallway
    | otherwise = findRoomCost currRooms nextRooms
    where
      findHalwayCost :: [Cell] -> [Cell] -> Int
      findHalwayCost left =
        getSum
          . foldMap (Sum . podCost)
          . catMaybes
          . zipWith go left

      findRoomCost :: [Room] -> [Room] -> Int
      findRoomCost left =
        getSum
          . foldMap (Sum . podCost)
          . catMaybes
          . zipWith go (fmap cpCell . foldMap getRoom $ left)
          . fmap cpCell
          . foldMap getRoom

      podCost :: Pod -> Int
      podCost =
        \case
          A -> 1
          B -> 10
          C -> 100
          D -> 1000

      go :: Cell -> Cell -> Maybe Pod
      go x y =
        case (x, y) of
          (Occupied p, Empty) -> pure p
          (Empty, Occupied p) -> pure p
          _ -> Nothing

arePodsSorted :: Map -> Bool
arePodsSorted Map {getRooms, getHallway}
  | any (/= Empty) (cpCell <$> getHallway) = False
  | otherwise =
    getAll
      . foldMap (uncurry allAre)
      . zip [A, B, C, D]
      $ getRooms
  where
    allAre :: Pod -> Room -> All
    allAre p = All . all ((== Occupied p) . cpCell) . getRoom

----------------------------------------------------------------
-- Solution2

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day23-1-input"
  let input = parse undefined "" contents
  case input of
    Left err -> mempty
    Right input -> mempty

-- putStrLn $ "Solution 1: " <> show (solution1 input)
-- putStrLn $ "Solution 2: " <> show (solution2 input)

input :: Map
input =
  Map
    { getRooms = [room1, room2, room3, room4],
      getHallway = CellPoint <$> replicate 11 Empty <*> mkHallwayPoints
    }
  where
    room1 = Room [mkRoolCell B 2 1, mkRoolCell A 2 2] A
    room2 = Room [mkRoolCell C 4 1, mkRoolCell D 4 2] B
    room3 = Room [mkRoolCell B 6 1, mkRoolCell C 6 1] C
    room4 = Room [mkRoolCell D 8 1, mkRoolCell A 8 1] D

    mkHallwayPoints :: [Point]
    mkHallwayPoints = Point <$> [0 .. 11] <*> repeat 0

    mkRoolCell :: Pod -> Int -> Int -> CellPoint
    mkRoolCell p x y = CellPoint (Occupied p) (Point x y)
