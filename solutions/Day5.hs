module Main (main) where

import Lib
import Text.Parsec
import Data.List (sort)

main :: IO ()
main = solution (Day 5) parser part1 part2

parser :: Parser ([(Int, Int)], [Int])
parser = do
  r <- ranges
  _ <- newline
  i <- ids
  return (r, i)
  where
    ranges = many $ do
      start <- integer
      _ <- char '-'
      end <- integer
      _ <- newline
      return (start, end)
    integer :: Parser Int
    integer = read <$> many digit
    ids = many (integer <* newline)

part1 (ranges, ids) = length $ filter (\i -> any (\(start, end) -> (start <= i) && (i <= end)) ranges) ids

part2 (rs, _) = sum $ map (\(s, e) -> e - s + 1) (foldr joinRanges [] (sort rs))
  where
    joinRanges :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    joinRanges range [] = [range]
    joinRanges (start, end) ((r_start, r_end) : ranges)
      -- Merge head with rest of ranges
      | end >= r_start && end <= r_end = (start, r_end) : ranges
      -- Range is disjoint
      | end < r_start = (r_start, r_end) : joinRanges (start, end) ranges
      -- Merge, but continue looking in case we consume more than one range
      | otherwise = joinRanges (start, end) ranges
    
