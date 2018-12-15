#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Elves = [Int]
type Scores = IntMap Int
type Scoreboard = (Scores, Int, Elves)

digits :: Int -> [Int]
digits 0 = [0]
digits s = go [] s where
    go ds 0 = ds
    go ds n = let (x, y) = divMod n 10 in go (y : ds) x

advance :: Scores -> Int -> Elves -> Elves
advance scores len = map (adv scores len) where
    adv scores' len' i = (i + 1 + (scores' IntMap.! i)) `mod` len'

oneExperiment :: Scoreboard -> Scoreboard
oneExperiment (scores, len, elves) = (scores', len', elves') where
    current = map (scores IntMap.!) elves
    digits' = digits $ sum current
    scores' = foldr insert scores $ zip [len ..] digits'
    insert (i, s) = IntMap.insert i s
    len' = len + length digits'
    elves' = advance scores' len' elves

roundsUntilLength :: Scoreboard -> Int -> Scoreboard
roundsUntilLength (scores, len, elves) n
    | len >= n = (scores, len, elves)
    | otherwise = roundsUntilLength (oneExperiment (scores, len, elves)) n

nRecipesAfterM :: Scoreboard -> Int -> Int -> [Int]
nRecipesAfterM board n m = map (scores IntMap.!) [m .. m + n - 1] where
    (scores, _, _) = roundsUntilLength board (n + m)

windows :: Scoreboard -> Int -> [(Int, [Int])]
windows board n = go board 0 where
    go board_ i = extract : go board' (i + 1) where
        board' = roundsUntilLength board_ (n + i)
        (scores', _ , _) = board'
        extract = (i, map (scores' IntMap.!) [i .. i + n - 1])

findSequence :: Scoreboard -> [Int] -> Int
findSequence board needle = go $ windows board (length needle) where
    go ((i, win) : rest) = if needle == win then i else go rest
    go _ = undefined

main :: IO ()
main = do
    input <- readFile "14.input"
    let num = read @Int $ head $ lines input
    let initial = (IntMap.fromList [(0, 3), (1, 7)], 2, [0, 1])
    -- part 1
    putStrLn $ concatMap show $ nRecipesAfterM initial 10 num
    -- part 2
    let needle = digits num
    print $ findSequence initial needle
