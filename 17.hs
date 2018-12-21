#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE TypeApplications #-}

-- import Data.Bits ((.&.), (.|.))
-- import Data.Bool (bool)
-- import qualified Data.IntMap.Strict as IM
-- import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
-- import Text.Read (readMaybe)
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
right (y, x) = (y, x - 1)

spreadLeft :: Clay -> Coord -> Flow -> (Flow, Bool)
spreadLeft clay cur flow =
    let flow' = Set.insert cur flow
        startFalling = down cur `Set.notMember` clay
        keepSpreading = left cur `Set.notMember` clay
     in if startFalling then fall clay (down cur) flow' else
        if keepSpreading then spreadLeft clay (left cur) flow' else (flow', True)

spreadRight :: Clay -> Coord -> Flow -> (Flow, Bool)
spreadRight clay cur flow =
    let flow' = Set.insert cur flow
        startFalling = down cur `Set.notMember` clay
        keepSpreading = right cur `Set.notMember` clay
     in if startFalling then fall clay (down cur) flow' else
        if keepSpreading then spreadRight clay (right cur) flow' else (flow', True)

fall :: Clay -> Coord -> Flow -> (Flow, Bool)
fall clay cur flow =
    let ((_, _), (ymax, _)) = boundingBox $ Set.toList clay
        finished = fst cur >= ymax
        flow' = Set.insert cur flow
        keepFalling = down cur `Set.notMember` clay
     in if finished then (flow', False) else
        if keepFalling
           then fall clay (down cur) flow'
                -- TODO: Check if we get backingUp == True from below and
                -- keep spreading from here, in that case.
            else
                let (lhs, lbu) = spreadLeft clay (left cur) flow'
                    (rhs, rbu) = spreadRight clay (right cur) lhs
                    flow'' = rhs
                    backingUp = lbu && rbu
                 in (flow'', backingUp)

main :: IO ()
main = do
    input <- readFile "17.small.input"
    let clay = Set.fromList $ concat $ parseMany clayVeins $ lines input
    let spring = (0, 500)
    let (flowWater, _) = fall clay spring Set.empty
    putStr $ renderMap (
        Map.insert spring '+' $
        Map.union
            (Map.fromSet (const '|') flowWater)
            (Map.fromSet (const '#') clay)
        ) ' '
