module Main (main) where

import Lib
import Text.Parsec
import GHC.Num (integerLogBase)

main :: IO ()
main = solution (Day 2) parser part1 part2

parser :: Parser [(Int, Int)]
parser = sepBy range (char ',')
  where
    range = do
      l <- integer
      _ <- char '-'
      r <- integer
      return (l, r)
    integer = read <$> many1 digit

part1 xs = sum $ do
  (start, end) <- xs
  filter isValid [start..end]
    where
      isValid x = let
        width = fromEnum $ integerLogBase 10 (fromIntegral x) + 1
        (l, r) = x `divMod` (10^(width `div` 2))
        in l == r

part2 xs = sum $ do
  (start, end) <- xs
  filter isValid [start..end]
    where
      isValid x = matches (show x) ""

      matches [] [] = True
      matches [_] [] = False
      matches (s0 : sRest) [] = matches sRest [s0]
      matches s@(s0 : sRest) m = isRepeat s m || matches sRest (m ++ [s0])
      matches [] _ = False

      isRepeat [] _ = True
      isRepeat s m = isDivisible && take (length m) s == m && isRepeat (drop (length m) s) m
        where isDivisible = length s `mod` length m == 0
