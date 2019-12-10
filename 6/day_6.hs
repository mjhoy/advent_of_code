module Main where

import           System.Environment (getArgs)

import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.List (foldl')

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (file:_) -> do
      str <- readFile file
      case (parse orbitedByAll file str) of
        Right os -> do
          let orbits = foldl' (\acc o -> insertOrbit o acc) M.empty os
          putStrLn $ show $ countOrbits orbits
        Left e -> putStrLn $ show e
