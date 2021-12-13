{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.Foldable
import Data.Function (on)
import Data.List
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

----------------------------------------------------------------
-- Parsers

----------------------------------------------------------------
-- Solution1

----------------------------------------------------------------
-- Solution2

----------------------------------------------------------------
-- Main
main :: IO ()
main = do
  contents <- T.readFile "day13-1-input"
  let input = parse undefined "" contents
  case input of
    Left err -> mempty
    Right lines -> mempty

-- putStrLn $ "Solution 1: " <> show (solution1 undefined)
-- putStrLn $ "Solution 2: " <> show (solution2 undefined)
