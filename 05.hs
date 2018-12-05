#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Char (toUpper)

react :: Char -> Char -> Bool
react a b = (a /= b) && (toUpper a == toUpper b)

process :: [Char] -> [Char]
process = go [] where
    go rwd [] = reverse rwd
    go [] (f : fwd) = go [f] fwd
    go (r : rwd) (f : fwd)
        | react r f = go rwd fwd
        | otherwise = go (f : r : rwd) fwd

remove :: Char -> [Char] -> [Char]
remove x = filter (\c -> toUpper c /= toUpper x)

main :: IO ()
main = do
    input <- readFile "05.input"
    let polymer = head $ lines input
    -- part 1
    let reacted = process polymer
    print $ length reacted
    -- part 2
    print $ minimum [length $ process $ remove x polymer | x <- ['a'..'z']]
