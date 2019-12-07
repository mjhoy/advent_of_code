{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, digit, char, newline)
import Text.Parsec (parse)
import Text.Parsec.Combinator
import Text.Read (readMaybe)
import Data.List (foldl', sort)

import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Ix

usage :: IO ()
usage = putStrLn $ "usage: runhaskell day_3.hs <input_file>"

newtype Key = Key { getKey :: Int }
  deriving (Eq, Ix, Ord, Num, Enum)

instance Show Key where
  show (Key i) = "K" ++ show i

data Direction = U | D | L | R
  deriving (Show, Eq)
data Line = Line Key Direction Int
  deriving (Show, Eq)
type Path = [Line]

direction :: Parser Direction
direction = do
  c <- anyChar
  case c of
    'U' -> pure U
    'D' -> pure D
    'L' -> pure L
    'R' -> pure R
    _   -> fail "not valid direction letter"

line :: Parser Line
line = do
  d <- direction
  num <- many1 digit
  case (readMaybe num) of
    Nothing -> fail "couldn't parse number"
    Just i -> pure $ Line 0 d i -- ummm we'll set the keys later

paths :: Parser [Path]
paths = do
  ps <- filter (not . null) <$> (line `sepBy` (char ',')) `sepBy` newline
  pure $ map (\(k, ls) -> map (\(Line _ d i) -> Line k d i) ls) $ zip [0..] ps

applyDirection :: Direction -> Coord -> Coord
applyDirection U = \(x,y) -> (x,y+1)
applyDirection D = \(x,y) -> (x,y-1)
applyDirection L = \(x,y) -> (x-1,y)
applyDirection R = \(x,y) -> (x+1,y)

newtype Index = Index { getIndex :: Int }
  deriving (Eq, Ix, Ord, Num, Enum)

instance Show Index where
  show (Index i) = show i

type Coord = (Index, Index)

type CoordSteps = (Int, Coord)

type Plane = Map Coord [(Key, Int)]

addKey :: Key -> Int -> Maybe [(Key, Int)] -> Maybe [(Key, Int)]
addKey k steps Nothing = Just [(k, steps)]
addKey k steps (Just ks) = case lookup k ks of
  (Just _) -> Just ks
  Nothing -> Just $ (k, steps) : ks

insertLine :: CoordSteps -> Line -> Plane -> (CoordSteps, Plane)
insertLine origin (Line k dir len) plane = foldl' fn (origin, plane) coords
  where
    step = \(s, c) -> (s + 1, applyDirection dir c)
    coords :: [CoordSteps] = take len $ iterate step (step origin) -- starts with a step
    fn (_, p) cS'@(s, c') = (cS', M.alter (addKey k s) c' p)

insertPath :: Coord -> Path -> Plane -> Plane
insertPath origin path plane = snd $ foldl' fn ((0, origin), plane) path
  where
    fn (c, p) l = insertLine c l p

centralPort :: Coord
centralPort = (0,0)

hammingDistance :: Coord -> Coord -> Int
hammingDistance ((Index x1), (Index y1)) ((Index x2), (Index y2)) =
  abs (x2 - x1) + abs (y2 - y1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (file:_) -> do
      str <- readFile file
      case (parse paths file str) of
        Right ps -> do
          let wiresInPlane = foldl' (\plane path -> insertPath centralPort path plane) M.empty ps
              didCross (_, v) = length v > 1
              crossCoords = filter didCross $ M.toList wiresInPlane
              coordsWithDistance = map (\(c, ks) -> sum (map snd ks)) crossCoords
              sorted = sort coordsWithDistance
          putStrLn $ show $ take 1 sorted
          -- putStrLn $ show $ sort $ M.toList wiresInPlane -- debug
        Left e -> putStrLn (show e)
