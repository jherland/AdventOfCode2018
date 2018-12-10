#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Utils

main :: IO ()
main = do
    input <- readFile "01.input"
    let shifts = parseMany (int <* eof) $ lines input
    -- part 1
    print $ sum shifts
    -- part 2
    print $ (fromJust . firstDup . scanl (+) 0 . cycle) shifts
