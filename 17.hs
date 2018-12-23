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
import Debug.Trace

type Clay = Set Coord -- (y, x)
type Flow = Set Coord

debug :: String -> a -> a
-- debug = trace
debug _ a = a

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

goLeft :: Clay -> Coord -> Flow -> (Flow, Bool)
goLeft clay cur right_ =
    let stopped = cur `Set.member` debug (show cur ++ "> <<<") (Set.union clay right_)
        here = Set.insert cur right_
        (below, dbu) = fall clay (down cur) $ debug (show cur ++ "| <<< -> vvv start falling") here
        (left_, lbu) = goLeft clay (left cur) $ debug (show cur ++ "| <<< -> <<< keep spreading") here
     in if stopped then debug (show cur ++ "< *<* nope!") (right_, True) else
        if not dbu then debug (show cur ++ "< vvv whee!") (below, False) else
        debug (show cur ++ "< ||| keep flowing") (left_, lbu)

goRight :: Clay -> Coord -> Flow -> (Flow, Bool)
goRight clay cur left_ =
    let stopped = cur `Set.member` debug (show cur ++ "> >>>") (Set.union clay left_)
        here = Set.insert cur left_
        (below, dbu) = fall clay (down cur) $ debug (show cur ++ "| >>> -> vvv start falling") here
        (right_, rbu) = goRight clay (right cur) $ debug (show cur ++ "| >>> -> >>> keep spreading") here
     in if stopped then debug (show cur ++ "< *>* nope!") (left_, True) else
        if not dbu then debug (show cur ++ "< vvv whee!") (below, False) else
        debug (show cur ++ "< ||| keep flowing") (right_, rbu)

fall :: Clay -> Coord -> Flow -> (Flow, Bool)
fall clay cur above =
    let (_, (ymax, _)) = boundingBox $ Set.toList clay
        finished = fst cur > ymax
        stopped = cur `Set.member` debug (show cur ++ "> vvv") (Set.union clay above)
        here = Set.insert cur above
        (below, dbu) = fall clay (down cur) $ debug (show cur ++ "| vvv -> vvv keep falling") here
        (left_, lbu) = goLeft clay (left cur) $ debug (show cur ++ "| vvv -> <<< spread left") below
        (right_, rbu) = goRight clay (right cur) $ debug (show cur ++ "| vvv -> >>> spread right") left_
     in if finished then debug (show cur ++ "< *** finished!") (above, False) else
        if stopped then debug (show cur ++ "< *v* nope!") (above, True) else
        if not dbu then debug (show cur ++ "< ||| no splashback") (below, False) else
        if lbu && rbu then debug (show cur ++ "< >|< backing up") (right_, True) else
        debug (show cur ++ "< ||| flowing left+right") (right_, False)

flow :: Clay -> [Coord] -> Flow
flow clay queue = go queue Set.empty where
    go [] seen  = seen
    go (cur : q) seen =
        let (_, (ymax, _)) = boundingBox $ Set.toList clay
            finished  = (fst cur > ymax) || cur `Set.member` seen
            free c = c `Set.notMember` Set.union clay seen
            d = down cur
            l = left cur
            r = right cur
            seen' = Set.insert cur seen
            fall = go (d : q) seen'
            q'
                | free l && free r = (l : r : q)
                | free l = (l : q)
                | free r = (r : q)
                | otherwise = q
         in if finished then debug (show cur ++ ": *** finished") $ go q seen else
            if free (down cur) then debug (show cur ++ ": vvv fall") fall else
            debug (show cur ++ ": <|> spread") $ go q' seen'

main :: IO ()
main = do
    input <- readFile "17.small.input"
    let clay = Set.fromList $ concat $ parseMany clayVeins $ lines input
    let spring = (0, 500)
    putStr $ renderMap (Map.insert spring '+' $ Map.fromSet (const '#') clay) ' '
    let water = flow clay [spring]
    putStr $ renderMap (
        Map.insert spring '+' $
        Map.union
            (Map.fromSet (const '|') water)
            (Map.fromSet (const '#') clay)
        ) ' '

    -- part 1
    print $ Set.size water
