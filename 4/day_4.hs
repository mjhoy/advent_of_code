{-# LANGUAGE ScopedTypeVariables #-}

module Day4 where

import Data.List
import Data.Char

calculate :: Int -> Int -> [Int]
calculate from to = filter isValid [from..to]

isValid :: Int -> Bool
isValid n =
  and [
        -- Going from left to right, the digits never decrease
        all (uncurry (<=)) $ zip digits (drop 1 digits)

        -- Two adjacent digits are the same (like 22 in 122345).
      , adjacentSame digits
      ]

  where
    digits :: [Int] = map digitToInt (show n)


allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

adjacentSameWithoutMore :: Eq a => a -> [a] -> Bool
adjacentSameWithoutMore x (y:ys) | x == y = adjacentSameWithoutMore x ys
                                 | otherwise = adjacentSame (y:ys)
adjacentSameWithoutMore _ _ = False

adjacentSame :: Eq a => [a] -> Bool
adjacentSame (x:y:ys) | x == y =
                        case ys of
                          (z:zs) | y == z -> adjacentSameWithoutMore x zs
                                 | otherwise -> True
                          _ -> True
                      | otherwise = adjacentSame (y:ys)
adjacentSame _ = False
