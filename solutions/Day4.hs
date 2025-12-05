{-# LANGUAGE TupleSections #-}
module Main (main) where

import Lib
import Text.Parsec
import qualified Data.HashSet as HashSet
import Data.Maybe (catMaybes)

main :: IO ()
main = solution (Day 4) parser part1 part2

parser :: Parser (HashSet.HashSet (Int, Int))
parser = HashSet.fromList . catMaybes <$> many paperRoll
  where
    paperRoll :: Parser (Maybe (Int, Int))
    paperRoll = do
      pos <- getPosition
      let x = sourceLine pos
      let y = sourceColumn pos
      c <- anyChar
      return $ if c == '@' then Just (x, y) else Nothing

numNeighbors rs (x, y) = length $ filter (\(dx, dy) -> HashSet.member (x + dx, y + dy) rs) deltas
  where deltas = [p | p <- concatMap (\dx -> map (dx,) [(-1)..1]) [(-1)..1], p /= (0, 0)]

part1 :: HashSet.HashSet (Int, Int) -> Int
part1 rs = length [r | r <- HashSet.toList rs, numNeighbors rs r < 4]

part2 :: HashSet.HashSet (Int, Int) -> Int
part2 rs = if numAccessible == 0 then 0 else numAccessible + part2 inaccessible
  where
    inaccessible = HashSet.fromList [r | r <- HashSet.toList rs, numNeighbors rs r >= 4]
    numAccessible = length rs - length inaccessible
