{-# LANGUAGE BlockArguments #-}
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

module Day19 where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Foldable
import Data.Function (on)
import Data.List
import Data.List.Extra (groupOn)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import qualified Debug.Trace as D
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void Text a

----------------------------------------------------------------
-- Types
data Point3 a = Point3
  { getX :: a,
    getY :: a,
    getZ :: a
  }
  deriving (Eq, Ord, Show)

data ReferenceRotation
  = XYZ
  | XZY
  | YXZ
  | YZX
  | ZXY
  | ZYX
  deriving (Eq, Ord, Show, Enum)

data Rotation = Rotation
  { rotX :: Bool,
    rotY :: Bool,
    rotZ :: Bool,
    rotRR :: ReferenceRotation
  }
  deriving (Eq, Ord, Show)

allRotations :: [Rotation]
allRotations =
  [ Rotation {rotX = x, rotY = y, rotZ = z, rotRR = rr}
    | x <- [False, True],
      y <- [False, True],
      z <- [False, True],
      rr <- [XYZ .. ZYX]
  ]

data WithOriginalPoint a = WithOriginalPoint
  { wopOriginal :: Point3 Int,
    wopValue :: a
  }
  deriving (Show, Functor)

instance Eq a => Eq (WithOriginalPoint a) where
  (==) = (==) `on` wopValue

instance Ord a => Ord (WithOriginalPoint a) where
  (<=) = (<=) `on` wopValue

type WOP = WithOriginalPoint (Point3 Int)

data Scanner a = Scanner
  { sIndex :: Int,
    sBeacons :: Set a
  }
  deriving (Eq, Show)

mapScanner :: Ord b => (a -> b) -> Scanner a -> Scanner b
mapScanner f s = s {sBeacons = Set.map f (sBeacons s)}

----------------------------------------------------------------
-- Parsers
parseInput :: Parser [Scanner (Point3 Int)]
parseInput = many parseScanner

parseScanner :: Parser (Scanner (Point3 Int))
parseScanner = do
  string "--- scanner "
  index <- decimal
  string " ---"
  newline
  beacons <- many parsePoint
  newline
  pure $ Scanner {sIndex = index, sBeacons = Set.fromList beacons}

parsePoint :: Parser (Point3 Int)
parsePoint = do
  x <- signed space decimal
  char ','
  y <- signed space decimal
  char ','
  z <- signed space decimal
  newline
  pure $ Point3 {getX = x, getY = y, getZ = z}

----------------------------------------------------------------
-- Solution1
makeRelativeTo :: Point3 Int -> Point3 Int -> WithOriginalPoint (Point3 Int)
makeRelativeTo reference point =
  WithOriginalPoint
    { wopOriginal = point,
      wopValue =
        Point3
          { getX = getX point - getX reference,
            getY = getY point - getY reference,
            getZ = getZ point - getZ reference
          }
    }

hasCommonSet :: Set WOP -> Set WOP -> Set WOP
hasCommonSet left = headOrEmpty . filter ((>= 12) . Set.size) . fmap (`Set.intersection` left) . rotations
  where
    rotations :: Set WOP -> [Set WOP]
    rotations s = [Set.map (fmap (rotatePoint rot)) s | rot <- allRotations]

    assertSingle :: [Set WOP] -> Set WOP
    assertSingle [x] = x
    assertSingle xs = error $ "single: " <> show (length xs)

    rotatePoint :: Rotation -> Point3 Int -> Point3 Int
    rotatePoint Rotation {rotX, rotY, rotZ, rotRR} Point3 {getX, getY, getZ} =
      let rx = bool (-1) 1 rotX
          ry = bool (-1) 1 rotY
          rz = bool (-1) 1 rotZ
       in case rotRR of
            XYZ -> invert rx ry rz getX getY getZ
            XZY -> invert rx rz ry getX getZ getY
            YXZ -> invert ry rx rz getY getX getZ
            YZX -> invert ry rz rx getY getZ getX
            ZXY -> invert rz rx ry getZ getX getY
            ZYX -> invert rz ry rx getZ getY getX

    invert :: Int -> Int -> Int -> Int -> Int -> Int -> Point3 Int
    invert rx ry rz x y z = Point3 {getX = x * rx, getY = y * ry, getZ = z * rz}

headOrEmpty :: [Set WOP] -> Set WOP
headOrEmpty = foldr const Set.empty

solution1 :: [Scanner (Point3 Int)] -> Int
solution1 =
  sum
    . adsf
    . fmap toCountBeacons
    . silly
    . fmap go
    . zipWithSelf
    . fmap (mapScanner =<< makeRelativeSets)
  where
    makeRelativeSets :: Scanner (Point3 Int) -> Point3 Int -> Set WOP
    makeRelativeSets Scanner {sIndex, sBeacons} p =
      Set.map (makeRelativeTo p) sBeacons

    go :: (Scanner (Set WOP), Scanner (Set WOP)) -> (Scanner WOP, Scanner WOP)
    go
      ( Scanner {sIndex = cIdx, sBeacons = cBeacons},
        Scanner {sIndex = idx, sBeacons = beacons}
        ) =
        ( Scanner
            { sIndex = cIdx,
              sBeacons = head $ Set.toList cBeacons
            },
          Scanner
            { sIndex = idx,
              sBeacons = hasCommonSet' cBeacons beacons
            }
        )

    hasCommonSet' :: Set (Set WOP) -> Set (Set WOP) -> Set WOP
    hasCommonSet' s1 s2 =
      headOrEmpty
        . filter (not . Set.null)
        $ [ hasCommonSet x y
            | x <- Set.toList s1,
              y <- Set.toList s2
          ]

toCountBeacons :: Scanner WOP -> Int
toCountBeacons = Set.size . sBeacons

adsf :: Show a => Traversable t => t a -> t a
adsf xs = unsafePerformIO do
  traverse_ print xs
  pure xs

silly :: [(Scanner WOP, Scanner WOP)] -> [Scanner WOP]
silly = fmap combine . groupOn sIndex . foldl go []
  where
    go :: [Scanner WOP] -> (Scanner WOP, Scanner WOP) -> [Scanner WOP]
    go curr (s1@Scanner {sIndex = i1, sBeacons = b1}, Scanner {sIndex = i2, sBeacons = b2})
      | i1 > i2 = Scanner {sIndex = i1, sBeacons = b1 `except` b2} : curr
      | otherwise = s1 : curr

    except :: Set WOP -> Set WOP -> Set WOP
    except = (Set.\\)

    combine :: [Scanner WOP] -> Scanner WOP
    combine [] = error "not implemented"
    combine (Scanner {sIndex = i, sBeacons = b} : xs) =
      Scanner {sIndex = i, sBeacons = foldl Set.intersection b $ fmap sBeacons xs}

zipWithSelf :: forall a. Eq a => [a] -> [(a, a)]
zipWithSelf xs =
  [ (a, b)
    | a <- xs,
      b <- xs,
      a /= b
  ]

----------------------------------------------------------------
-- Solution2

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day19-1-input"
  let input = parse parseInput "" contents
  case input of
    Left err -> mempty
    Right input -> do
      putStrLn $ "Solution 1: " <> show (solution1 input)

-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
