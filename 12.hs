#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
import Utils

type Plant = Char -- '.' - dead, '#' - alive
type Near = String -- 5 chars
type Pots = (String, Int) -- (plants, first index)
type Spread = Map Near Plant

plant :: ReadP Plant
plant = satisfy (== '.') <|> satisfy (== '#')

parseState :: ReadP [Plant]
parseState = string "initial state: " >> many plant <* eof

parseSpread :: ReadP (Near, Plant)
parseSpread = do
    from <- count 5 plant
    to <- string " => " >> plant <* eof
    return (from, to)

at :: Pots -> Int -> Plant
at (plants, first) i
    | null plants = '.' -- too far right
    | i < first   = '.' -- too far left
    | i == first  = head plants
    | i > first   = at (tail plants, first + 1) i
    | otherwise   = undefined

near :: Pots -> Int -> Near
near pots i = map (at pots) [i - 2 .. i + 2]

next :: Spread -> Pots -> Int -> Plant
next spread pots i = spread Map.! near pots i

trim :: Pots -> Pots
trim ('.' : plants, first) = trim (plants, first + 1)
trim (plants, first) = (rtrim plants, first) where
    rtrim = reverse . dropWhile (== '.') . reverse

tng :: Spread -> Pots -> Pots
tng spread (cur, first) = trim (map f [first' .. last'], first') where
    first' = first - 2
    last' = first + length cur + 2
    f = next spread (cur, first)

sumPots :: Pots -> Int
sumPots (plants, first) = sum $ zipWith f [first .. ] plants where
    f i p = if p == '#' then i else 0

stabilize :: Spread -> Pots -> Int -> (Pots, Int, Int)
stabilize spread cur gen =
    if stable then (cur, gen, df) else stabilize spread nxt (gen + 1) where
        nxt = tng spread cur
        stable = fst nxt == fst cur -- same string
        df = snd nxt - snd cur -- different index/offset

ffwd :: Pots -> Int -> Int -> Pots
ffwd (plants, first) df gens = (plants, first + gens * df)

main :: IO ()
main = do
    input <- readFile "12.input"
    let (stateLine : "" : spreadLines) = lines input
    let pots = trim (parseOne parseState stateLine, 0)
    let spread = Map.fromList $ parseMany parseSpread spreadLines
    -- part 1
    print $ sumPots $ iterate (tng spread) pots !! 20
    -- part 2 (iterate until our state has stabilized)
    let (stable, gen, df) = stabilize spread pots 0
    print $ sumPots $ ffwd stable df (50 * 1000 * 1000 * 1000 - gen)
