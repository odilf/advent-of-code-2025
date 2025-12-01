module Main (main) where

import Lib
import Text.Parsec hiding (count)

main :: IO ()
main = solution (Day 1) parser part1 part2

parser :: Parser [Int]
parser = many line
  where
    line = do
      sign <- l <|> r
      x <- integer
      _ <- newline
      return (sign * x)
    l = -1 <$ char 'L'
    r = 1 <$ char 'R'
    integer = read <$> many1 digit

part1 xs = snd $ foldl (\(acc, count) x -> let m = (acc + x) `mod` 100 in (m, count + fromEnum (m == 0))) (50, 0) xs

part2 xs = snd $ foldl next (50, 0) xs where
  next (acc, count) delta = let
      m = (acc + delta) `mod` 100
      fullRots = abs (acc + delta) `div` 100
      switches = signum acc == -signum (acc + delta)
      cancels = (acc + delta) == 0
      rots = fullRots + fromEnum switches + fromEnum cancels
    in
      (m, count + rots)
