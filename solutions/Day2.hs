module Main (main) where

import Lib
import Text.Parsec hiding (count)

main :: IO ()
main = solution (Day 2) parser part1 part2

parser :: Parser [Int]
parser = many line
  where
    line = integer <* newline
    integer = read <$> many1 digit

part1 _ = 69420

part2 _ = 69420
