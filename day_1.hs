{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Arrow ((>>>))
import Data.Functor ((<&>))

import Debug.Trace

newtype Mass = Mass Integer
  deriving (Eq, Show, Num, Real, Integral, Enum, Ord)

newtype Fuel = Fuel Integer
  deriving (Eq, Show, Num, Real, Integral, Enum, Ord)

fuelRequired :: Mass -> Fuel
fuelRequired = fromIntegral >>> (/ 3) >>> floor >>> (\x -> x - 2) >>> Fuel

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: runhaskell day_1.hs -- input"
    (inputFile:_) -> do
      masses :: [Mass] <- (fmap Mass) <$> catMaybes <$> (fmap readMaybe) <$> lines <$> readFile inputFile
      let answer = sum $ masses <&> fuelRequired
      putStrLn $ "Answer: " ++ (show answer)
