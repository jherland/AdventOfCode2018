#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

occurences :: [Char] -> Map Char Int
occurences chars = go Map.empty chars where
    go m [] = m
    go m (char : chars') = go (Map.alter inc char m) chars' where
        inc :: Maybe Int -> Maybe Int
        inc Nothing = Just 1
        inc (Just v) = Just $ v + 1

hasNOfAny :: Int -> Map a Int -> Bool
hasNOfAny n m = not $ Map.null $ Map.filterWithKey (\_ v -> v == n) m

removeNthChar :: Int -> [Char] -> [Char]
removeNthChar n s = (take n s) ++ (drop (n + 1) s)

findDup :: [String] -> Maybe String
findDup strings = go Set.empty strings where
    go _ [] = Nothing
    go set (s : ss) = if Set.member s set then Just s else go (Set.insert s set) ss

findAlmostDup :: [String] -> String
findAlmostDup strings = go 0 strings where
    go n ss = case findDup (map (removeNthChar n) ss) of
        Just s -> s
        Nothing -> go (n + 1) ss

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
