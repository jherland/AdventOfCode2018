#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Utils

type Clay = Set Coord -- (y, x)
type Flow = Set Coord

clayVeins :: ReadP [Coord]
clayVeins = do
    a1 <- satisfy (`elem` "xy") <* char '='
    i1 <- int <* string ", "
    a2 <- satisfy (`elem` "xy") <* char '='
    i2 <- int <* string ".."
    i3 <- int <* eof
    case (a1, a2) of
        ('y', 'x') -> return [(i1, r) | r <- [i2 .. i3]]
        ('x', 'y') -> return [(r, i1) | r <- [i2 .. i3]]
        _ -> pfail

up :: Coord -> Coord
up (y, x) = (y - 1, x)

down :: Coord -> Coord
down (y, x) = (y + 1, x)

left :: Coord -> Coord
left (y, x) = (y, x - 1)

right :: Coord -> Coord
right (y, x) = (y, x + 1)

findPool :: Coord -> Clay -> Flow -> [Coord]
findPool cur clay wflow
    | cur `Set.notMember` wflow = []
    | left cur `Set.notMember` clay = findPool (left cur) clay wflow
    | otherwise = go cur [] where
        go c found
            | c `Set.notMember` wflow = []
            | right c `Set.notMember` clay = go (right c) (c : found)
            | otherwise = c : found

findAbove :: [Coord] -> Flow -> [Coord]
findAbove pool wflow = filter (`Set.member` wflow) $ map up pool

flow :: Clay -> [(Coord, Bool)] -> (Flow, Flow)
flow clay queue = go queue (Set.empty, Set.empty) where
    (_, (ymax, _)) = boundingBox $ Set.toList clay
    go [] results = results
    go ((cur, rwd) : q) (wflow, wpool) =
        let finished  = (fst cur > ymax) || cur `Set.member` wpool
            s = down cur
            w = left cur
            e = right cur
            wflow' = Set.insert cur wflow
         in if finished then go q (wflow, wpool) else
            if not rwd then -- forward flow
                if s `Set.notMember` Set.union clay wpool then
                    if s `Set.notMember` wflow then
                        go ((s, False) : q) (wflow', wpool)
                    else go q (wflow', wpool)
                else -- clay or pool below, spread sideways
                    let lq
                            | w `Set.member` clay = ((cur, True) : q)
                            | w `Set.notMember` wflow = ((w, False) : q)
                            | otherwise = q
                        rq
                            | e `Set.member` clay = ((cur, True) : lq)
                            | e `Set.notMember` wflow = ((e, False) : lq)
                            | otherwise = lq
                     in go rq (wflow', wpool)
            else -- reverse flow
                let pool = findPool cur clay wflow
                    above = findAbove pool wflow
                    wpool' = Set.union wpool $ Set.fromList pool
                    q' = map (, False) above ++ q
                 in go q' (wflow, wpool')

main :: IO ()
main = do
    input <- readFile "17.input"
    let clay = Set.fromList $ concat $ parseMany clayVeins $ lines input
    let spring = (0, 500)
    let start = (fst (fst (boundingBox (Set.toList clay))), snd spring)
    let (wflow, wpool) = flow clay [(start, False)]
    putStr $ renderMap (Map.insert spring '+' $
                        Map.union (Map.union (Map.fromSet (const '~') wpool)
                                             (Map.fromSet (const '|') wflow))
                                  (Map.fromSet (const '#') clay))
                       ' '
    -- part 1
    print $ Set.size wflow
    -- part 2
    print $ Set.size wpool
