module Main where

import           System.Environment (getArgs)

import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)

import           Text.Parsec.String (Parser)
import           Text.Parsec.Char (char, newline, digit, letter)
import           Text.Parsec.Combinator
import           Text.Parsec (parse)
import           Control.Applicative ((<|>))

usage :: IO ()
usage = putStrLn "usage: runhaskell day_6.hs <input_file>"

type Orbits = Map String [String]

orbitThing :: Parser String
orbitThing = many1 (letter <|> digit)

orbitedBy :: Parser (String, String)
orbitedBy = do
  x <- orbitThing
  _ <- char ')'
  y <- orbitThing
  pure (x,y)

orbitedByAll :: Parser [(String, String)]
orbitedByAll = orbitedBy `endBy` newline

insertOrbit :: (String, String) -> Orbits -> Orbits
insertOrbit (orbited, orbits) = M.alter fn orbited
  where
    fn Nothing = Just [orbits]
    fn (Just others)  = Just (orbits:others)

countOrbits :: Orbits -> Int
countOrbits os = sum $ map countSingle allOrbited
  where
    allOrbited = M.keys os
    countSingle k =
      let kOrbits = maybe [] id (M.lookup k os)
      in length kOrbits + (sum $ map countSingle kOrbits)

minimumDef :: Ord a => a -> [a] -> a
minimumDef d [] = d
minimumDef _ xs = minimum xs

countOrbitalTransfer :: Orbits -> Orbits -> String -> String -> Int
countOrbitalTransfer orbitedByInfo orbitsAroundInfo from to = walk (-1) from (allPaths from)
  where
    allPaths k = osBy ++ osAround
      where
        osBy     = fromMaybe [] $ M.lookup k orbitedByInfo
        osAround = fromMaybe [] $ M.lookup k orbitsAroundInfo
    walk :: Int -> String -> [String] -> Int
    walk steps current nextPaths
      | to `elem` nextPaths = steps
      | otherwise           = minimumDef maxBound $ map nextWalk nextPaths
      where
        nextWalk :: String -> Int
        nextWalk k = walk (steps + 1) k (filter (/= current) $ allPaths k)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (file:_) -> do
      str <- readFile file
      case (parse orbitedByAll file str) of
        Right os -> do
          let
            osBy     = foldl' (\acc (a,b) -> insertOrbit (a,b) acc) M.empty os
            osAround = foldl' (\acc (a,b) -> insertOrbit (b,a) acc) M.empty os
          putStrLn $ "all orbits: " ++ (show $ countOrbits osAround)
          putStrLn $ "orbital transfers: " ++ (show $ countOrbitalTransfer osBy osAround "SAN" "YOU")
        Left e -> putStrLn $ show e
