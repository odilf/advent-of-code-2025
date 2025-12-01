{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( solution
    , Day(Day)
    , Parser
    ) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Text.Encoding (decodeASCII)
import Data.Text (Text)
import Configuration.Dotenv as Dotenv
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Text.IO (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Char8 (pack)
import System.Environment (getEnv)
import Text.Parsec (Parsec, parse)

year :: Integer
year = 2025

newtype Day = Day Integer

instance Show Day where
  show (Day n) = "day-" ++ show n

type Parser i = Parsec Text () i

solution :: (Show a, Show b) => Day -> Parser i -> (i ->  a) -> (i -> b) -> IO ()
solution day parser part1 part2 = do
    _ <- solve day parser part1 part2
    return ()

solve :: (Show a, Show b) => Day -> Parser i -> (i ->  a) -> (i -> b) -> IO (a, b)
solve day parser part1 part2 = do
    input <- getInput day

    let parsedInput = case parse parser "" input of
         Right p -> p
         Left err -> error (show err)

    let !sol1 = part1 parsedInput
    putStrLn $ "Part 1 solution is: \n" ++ show sol1
    let !sol2 = part2 parsedInput
    putStrLn $ "Part 2 solution is: \n" ++ show sol2

    return (sol1, sol2)

getInput :: Day -> IO Text
getInput day = do
    let file = ".inputs/" ++ show day ++ ".txt"
    cached <- doesFileExist file
    if cached then
        readFile file
    else do
        input <- fetchInput day
        putStrLn ("Writting input at " ++ file)
        createDirectoryIfMissing True ".inputs"
        writeFile file input
        return undefined

fetchInput :: Day -> IO Text
fetchInput (Day n) = runReq defaultHttpConfig $ do
    liftIO $ putStrLn ("Fetcing input for day " ++ show n)
    sessionToken <- liftIO getSessionToken
    let url = https "adventofcode.com" /~ year /: "day" /~ n /: "input"
    let options =  header "Cookie" (pack $ "session=" ++ sessionToken)
                <> header "User-Agent" "odilf's haskell solution (at https://github.com/odilf/advent-of-code-2025)"

    response <- req GET url NoReqBody bsResponse options
    return $ decodeASCII (responseBody response)

getSessionToken :: IO String
getSessionToken = do
    Dotenv.loadFile Dotenv.defaultConfig
    getEnv "SESSION_TOKEN"
