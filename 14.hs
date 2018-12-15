#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

type Index = Int
type Score = Int
type Elves = [Index]
type Scoreboard = ([Score], Elves)

digits :: Int -> [Int]
digits = go [] where
    go ds 0 = ds
    go ds n = go (n `mod` 10 : ds) (n `div` 10)

combine :: [Score] -> [Score]
combine = digits . sum

index :: [Score] -> Int -> Index
index scores i = i `mod` length scores

at :: [Score] -> Index -> Score
at scores i = scores !! index scores i

addScores :: [Score] -> Elves -> [Score]
addScores scores elves = scores ++ combine recipes where
    recipes = map (at scores) elves

advance :: [Score] -> Elves -> Elves
advance scores = map (adv scores) where
    adv scores i = index scores $ i + 1 + scores !! i

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

nRecipesAfterM :: Scoreboard -> Int -> Int -> [Score]
nRecipesAfterM scores n m = take n $ drop m $ fst $ roundsUntilLength scores (n + m)

join :: [Score] -> String
join = concatMap show

main :: IO ()
main = do
    input <- readFile "14.input"
    let scores' = map (\c -> read @Int [c]) $ head $ lines input
    let scores = [3, 7]
    let initial = (scores, [0, 1])
    putStrLn $ join $ nRecipesAfterM initial 10 9
    putStrLn $ join $ nRecipesAfterM initial 10 5
    putStrLn $ join $ nRecipesAfterM initial 10 18
    putStrLn $ join $ nRecipesAfterM initial 10 2018
    -- part 1
    putStrLn $ join $ nRecipesAfterM initial 10 637061

    -- part 2
