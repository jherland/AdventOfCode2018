#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (drop, length, take)
import Data.Sequence

type Index = Int
type Score = Int
type Elves = [Index]
type Scoreboard = (Seq Score, Elves)

digits :: Int -> Seq Int
digits = go empty where
    go ds 0 = ds
    go ds n = go (n `mod` 10 <| ds) (n `div` 10)

combine :: Seq Score -> Seq Score
combine = digits . sum

ix :: Seq Score -> Int -> Index
ix scores i = i `mod` length scores

at :: Seq Score -> Index -> Score
at scores i = scores `index` ix scores i

addScores :: Seq Score -> Elves -> Seq Score
addScores scores elves = scores >< combine recipes where
    recipes = fromList $ map (at scores) elves

advance :: Seq Score -> Elves -> Elves
advance scores = map (adv scores) where
    adv scores i = ix scores $ i + 1 + scores `index` i

oneRound :: Scoreboard -> Scoreboard
oneRound (scores, elves) = (scores', elves') where
    scores' = addScores scores elves
    elves' = advance scores' elves

-- Hmm, how do I turn this into an infinite series of recipe scores?
-- I want my main to simply do: take 10 $ drop 999 recipes...
roundsUntilLength :: Scoreboard -> Int -> Scoreboard
roundsUntilLength board n
    | length (fst board) >= n = board
    | otherwise = roundsUntilLength (oneRound board) n

nRecipesAfterM :: Scoreboard -> Int -> Int -> Seq Score
nRecipesAfterM scores n m = take n $ drop m $ fst $ roundsUntilLength scores (n + m)

join :: Seq Score -> String
join = concatMap show

main :: IO ()
main = do
    input <- readFile "14.input"
    let scores' = map (\c -> read @Int [c]) $ head $ lines input
    let scores = fromList [3, 7]
    let initial = (scores, [0, 1])
    putStrLn $ join $ nRecipesAfterM initial 10 9
    putStrLn $ join $ nRecipesAfterM initial 10 5
    putStrLn $ join $ nRecipesAfterM initial 10 18
    putStrLn $ join $ nRecipesAfterM initial 10 2018
    -- part 1
    putStrLn $ join $ nRecipesAfterM initial 10 637061
    -- 1513321432 is too low!
    -- part 2
