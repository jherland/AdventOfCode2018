#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Utils

occurences :: String -> Map Char Int
occurences = go Map.empty where
    go m [] = m
    go m (char : chars') = go (Map.alter inc char m) chars' where
        inc :: Maybe Int -> Maybe Int
        inc Nothing = Just 1
        inc (Just v) = Just $ v + 1

hasNOfAny :: Int -> Map a Int -> Bool
hasNOfAny n m = not $ Map.null $ Map.filterWithKey (\_ v -> v == n) m

removeNthChar :: Int -> String -> String
removeNthChar n s = take n s ++ drop (n + 1) s

findAlmostDup :: [String] -> String
findAlmostDup = go 0 where
    go n ss = fromMaybe (go (n + 1) ss) (firstDup (map (removeNthChar n) ss))

main :: IO ()
main = do
    input <- readFile "02.input"
    let ids = lines input
    -- part 1
    let occs = map occurences ids
    let idsWDoubles = length $ filter (hasNOfAny 2) occs
    let idsWTriples = length $ filter (hasNOfAny 3) occs
    print $ idsWDoubles * idsWTriples
    -- part 2
    putStrLn $ findAlmostDup ids
