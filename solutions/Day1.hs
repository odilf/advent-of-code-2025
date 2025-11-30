{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import Lib
import qualified Text.Parsec as Parsec
import Text.Parsec (digit, many, many1)
import Data.List (sort)

main :: IO ()
main = solution (Day 1) parser part1 part2

parser = do
  xys <- many line
  return $ transpose xys

  where
    line = do
      x <- integer
      _ <- Parsec.string "   "
      y <- integer
      _ <- Parsec.newline
      return (x, y)
    integer = read <$> many1 digit
    transpose [] = ([], [])
    transpose ((x, y) : rest) = let (xs, ys) = transpose rest in (x : xs, y : ys)

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum $ map (\x -> x * length (filter (x ==) ys)) xs
