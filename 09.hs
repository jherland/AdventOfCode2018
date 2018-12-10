#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import Utils

settings :: ReadP (Int, Int)
settings = do
    p <- int <* string " players; last marble is worth "
    m <- int <* string " points\n" <* eof
    return (p, m)

type Marblez = ([Int], [Int]) -- (rwd, fwd)
type Scores = IntMap Int -- player -> score
type Game = (Marblez, Scores) -- (marbles, players)

initGame :: Int -> Game
initGame n = (([], [0]), IntMap.fromDistinctAscList [(i, 0) | i <- [1 .. n]])

rot :: Int -> Marblez -> Marblez
rot n (rwd, fwd)
    | n == 0    = (rwd, fwd)
    | n < 0     = swap $ rot (abs n) (fwd, rwd)
    | null fwd  = rot n ([], reverse rwd)
    | otherwise = rot (n - 1) (head fwd : rwd, drop 1 fwd)

place :: Int -> Marblez -> Marblez
place m (rwd, fwd) = (rwd', m:fwd') where
    (rwd', fwd') = rot 2 (rwd, fwd)

remove :: Marblez -> (Marblez, Int)
remove (rwd, fwd) = ((rwd', drop 1 fwd'), head fwd') where
    (rwd', fwd') = rot (-7) (rwd, fwd)

turn :: Int -> Int -> Game -> Game
turn player marble (mz, scores) = if marble `mod` 23 /= 0
    then (place marble mz, scores)
    else (mz', scores') where
        (mz', score) = remove mz
        scores' = IntMap.adjust (+(marble + score)) player scores

play :: Int -> Int -> Game
play numPlayers numMarbles =
    foldl (\g (p, m) -> turn p m g) game (zip players marbles) where
        players = cycle [1 .. numPlayers]
        marbles = [1 .. numMarbles]
        game = initGame numPlayers

highScore :: Game -> Int
highScore (_, scores) = IntMap.foldr max 0 scores

main :: IO ()
main = do
    input <- readFile "09.input"
    let (nplayers, hmarble) = parseOne settings input
    -- part 1
    print $ highScore $ play nplayers hmarble
    -- part 2
    print $ highScore $ play nplayers (hmarble * 100)
