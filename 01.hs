#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Text.Read (readMaybe)
import qualified Data.IntSet as IntSet

parse :: String -> Int
parse ('+' : val) = parse val
parse val = case readMaybe val :: Maybe Int of
    Just num -> num
    Nothing -> 0

firstDup :: [Int] -> Int
firstDup ints = go IntSet.empty ints where
    go s (n : ns) = if IntSet.member n s then n else go (IntSet.insert n s) ns
    go _ _ = undefined

main :: IO ()
main = do
    input <- readFile "01.input"
    let shifts = map parse $ lines input
    -- part 1
    print $ sum shifts
    -- part 2
    print $ firstDup $ scanl (+) 0 $ cycle shifts
