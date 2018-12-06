#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Char (toUpper)

reactive :: Char -> Char -> Bool
reactive a b = (a /= b) && (toUpper a == toUpper b)

react :: [Char] -> [Char]
react = go [] where
    go rwd [] = rwd
    go [] (f : fwd) = go [f] fwd
    go (r : rwd) (f : fwd)
        | reactive r f = go rwd fwd
        | otherwise = go (f : r : rwd) fwd

remove :: Char -> [Char] -> [Char]
remove x = filter (\c -> toUpper c /= toUpper x)

main :: IO ()
main = do
    input <- readFile "05.input"
    let polymer = head $ lines input
    -- part 1
    let reacted = react polymer
    print $ length reacted
    -- part 2
    print $ minimum [length $ react $ remove x polymer | x <- ['a'..'z']]
