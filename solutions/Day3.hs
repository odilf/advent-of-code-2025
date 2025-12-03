module Main (main) where

import Lib
import Text.Parsec

main :: IO ()
main = solution (Day 3) parser part1 part2

parser :: Parser [[Int]]
parser = many (battery <* newline)
  where
    battery = many integer
    integer = read <$> count 1 digit

iMax :: [Int] -> (Int, Int)
iMax xs = foldr (\(i, x) (mi, mx) -> if x >= mx then (i, x) else (mi, mx)) (undefined, -1) (zip [0..] xs)

part1 xss = sum $ map f xss
  where
    f :: [Int] -> Int
    f xs = let
        (i1, m1) = iMax (dropTail xs)
        (_, m2) = iMax (drop (i1 + 1) xs)
      in m1 * 10 + m2

    dropTail [] = error "Empty list"
    dropTail [_] = []
    dropTail (a : as) = a : dropTail as

part2 xss = sum $ do
  xs <- xss
  let f i (i_prev, m_prev) = (i_prev + 1 + i_next, m_prev + m_next * 10^i)
        where (i_next, m_next) = iMax (drop (i_prev + 1) . dropTail i $ xs)

  let maxes = foldr f (-1, 0) [0..11]
  return (snd maxes)

  where
    dropTail n xs | length xs == n = []
    dropTail n (x : xs) = x : dropTail n xs
    dropTail _ _ = error "List not large enough"
