#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE TupleSections #-}

import Control.Exception (assert)
import Data.List (intercalate, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Utils
import Debug.Trace

type Space = Set Coord -- (y, x) for all non-wall coords
data Kind = Elf | Goblin deriving (Eq, Show)
data Unit = Unit { pos_ :: Coord
                 , kind_ :: Kind
                 , hp_ :: Int
                 , ap_ :: Int
                 } deriving Eq
instance Show Unit where
    show (Unit pos Elf hp _) = "E/" ++ show hp ++ "@" ++ show pos
    show (Unit pos Goblin hp _) = "G/" ++ show hp ++ "@" ++ show pos
type Units = Map Coord Unit
data Game = Game { space_ :: Space
                 , units_ :: Units
                 }
type DistanceMap = Map Coord Int

parse :: Int -> String -> Game
parse elfAttack= buildGame . concat . zipWith parseLine [0 ..] . lines where
    parseLine y = zipWith (\x c -> ((y, x), c)) [0 ..]
    buildGame :: [(Coord, Char)] -> Game
    buildGame = go (Set.empty, Map.empty) where
        go (space, units) [] = Game space units
        go (space, units) ((pos, c) : rest) = go (space', units') rest where
            space' = if c /= '#' then Set.insert pos space else space
            units' = case c of
                'E' -> Map.insert pos (Unit pos Elf 200 elfAttack) units
                'G' -> Map.insert pos (Unit pos Goblin 200 3) units
                _ -> units

render :: Game -> String
render (Game space units) =
    unlines $ map renderLine [ymin - 1 .. ymax + 1] where
        ((ymin, xmin), (ymax, xmax)) = boundingBox $ Set.toList space
        renderLine y = map (renderPos y) [xmin - 1 .. xmax + 1] ++ suffix y
        renderPos y x
          | (y, x) `Map.member` units = case units Map.! (y, x) of
                (Unit _ Elf _ _) -> 'E'
                (Unit _ Goblin _ _) -> 'G'
          | (y, x) `Set.member` space = '.'
          | otherwise= '#'
        suffix y = "  " ++ intercalate ", " (
            Map.elems (Map.map show (Map.filter ((== y) . fst . pos_) units)))

around :: Coord -> Space -- coords around the given point, in reading order
around (y, x) = Set.fromDistinctAscList [
                        (pred y, x), (y, pred x), (y, succ x), (succ y, x)]

adjacent :: Space -> Coord -> Space -- adjacent coords that exist in space
adjacent space = Set.intersection space . around

isAdjacent :: Space -> Coord -> Coord -> Bool
isAdjacent space pos1 pos2 = pos2 `Set.member` adjacent space pos1

enemies :: Units -> Unit -> Units
enemies units unit = case kind_ unit of
    Elf -> Map.filter ((== Goblin) . kind_) units
    Goblin -> Map.filter ((== Elf) . kind_) units

hasEnemies :: Units -> Unit -> Bool
hasEnemies units = not. Map.null . enemies units

occupied :: Units -> Coord -> Bool -- Location c is occupied by one of units
occupied units c = c `Set.member` Map.keysSet units

available :: Game -> Space
available (Game space units) = foldr Set.delete space $ Map.keys units

inRange :: Game -> Unit -> Space -- Open squares adjacent to enemies
inRange game@(Game space units) unit = enemyAdjs where
    avail = available game
    enemyPos = Map.keys $ enemies units unit
    enemyAdjs = Set.unions $ map (adjacent avail) enemyPos

bfs :: Space -> DistanceMap -> [(Coord, Int)] -> Space -> DistanceMap
bfs _ graph [] _ = graph
bfs space graph queue seen = bfs space graph' queue'' seen' where
    ((pos, dist) : queue') = queue
    neighbours = adjacent space pos
    unseen = Set.difference neighbours seen
    enqueue = [(c, dist + 1) | c <- Set.toList unseen]
    graph' = Map.union graph $ Map.fromList enqueue
    queue'' = queue' ++ enqueue
    seen' = Set.union seen unseen

distanceFrom :: Space -> Coord -> DistanceMap -- Map space to distance from pos
distanceFrom space pos =
    bfs space (Map.singleton pos 0) [(pos, 0)] (Set.singleton pos)

chooseTarget :: Space -> Space -> Coord -> (Maybe Coord, DistanceMap)
chooseTarget space inrange src =
    let allDistances = Map.fromSet (distanceFrom space) inrange
        reachable = Map.filter (Map.member src) allDistances
        -- find target nearest to src
        (target, distmap) = head $ sortOn nearest (Map.toList reachable)
        nearest (tgt, dmap) = (dmap Map.! src, tgt)
     in if Map.null reachable 
           then trace "  No target chosen" (Nothing, Map.empty)
           else trace ("  Chose target " ++ show target) (Just target, distmap)

findPath :: Space -> Space -> Coord -> Maybe Coord -- Find where to move
findPath space inrange src =
    let (target, distmap) = chooseTarget space inrange src
        -- find adjancent square nearest target
        adjDist = traceShowId $ Map.fromSet (distmap Map.!?) (adjacent space src)
        moveTo = sortOn swap (Map.toList (Map.filter isJust adjDist))
     in if isNothing target || null moveTo
           then Nothing
           else Just (fst $ head moveTo)

enemyInRange :: Game -> Unit -> Maybe Unit
enemyInRange (Game space units) unit = chooseFirst adjacentEnemies where
        enemies' = enemies units unit
        adjacent' = adjacent space $ pos_ unit
        adjacentEnemies = Map.restrictKeys enemies' adjacent'
        chooseFirst = listToMaybe . Map.elems

turn :: Game -> Unit -> Units
turn = phase1

phase1 :: Game -> Unit -> Units -- Are there any enemies?
phase1 (Game space units) unit
    | not $ hasEnemies units unit = units -- no enemies, combat has ended
    | otherwise = units''' where -- lift unit from board while doing its turn
        units' = Map.delete (pos_ unit) units
        (units'', unit') = phase2 (Game space units') unit
        units''' = Map.insert (pos_ unit') unit' units''

phase2 :: Game -> Unit -> (Units, Unit) -- Are there any target squares?
phase2 game@(Game _ units) unit =
    let inrange = inRange game unit
    in if Set.null inrange
        then (units, unit) -- no available target squares, nothing to do
        else phase3 game unit inrange

phase3 :: Game -> Unit -> Space -> (Units, Unit) -- Do we need to move?
phase3 game@(Game space units) unit inrange
    | pos_ unit `Set.member` inrange = phase9 game unit -- already in range
    | otherwise = phase4 game unit inrange -- need to move

phase4 :: Game -> Unit -> Space -> (Units, Unit) -- Figure out where to move
phase4 game@(Game space units) unit inrange =
    let avail = trace ("4. Finding path from " ++ show unit ++ " to " ++ show inrange) $ available game
        moveTo' = findPath avail inrange (pos_ unit)
        moveTo = trace ("  Found: " ++ show moveTo') moveTo'
    in maybe (units, unit) (phase5 game unit) moveTo

phase5 :: Game -> Unit -> Coord -> (Units, Unit) -- move one step
phase5 game@(Game space units) unit pos = trace ("5. Moving " ++  show unit ++ " to " ++ show pos) $ phase6 game unit' where
    avail = available game
    unit' = assert (pos `Set.member` adjacent avail (pos_ unit)) unit { pos_ = pos }

phase6 :: Game -> Unit -> (Units, Unit) -- attack?
phase6 game@(Game space units) unit = (units, unit)

phase9 :: Game -> Unit -> (Units, Unit)
phase9 game unit = (units_ game, unit)

oneRound :: Game -> Game
oneRound (Game space units) =
    Game space (Map.foldl perUnit units units) where
        perUnit units' = turn (Game space units')

initGame :: Int -> IO Game
initGame elfAttack = do
    input <- readFile "15.input"
    return $ parse elfAttack input

smallGame :: IO Game
smallGame = do
    input <- readFile "15.small.input"
    return $ parse 3 input

main :: IO ()
main = do
    -- part 1
    game <- initGame 3
    print '0'
    putStr $ render game
    print '1'
    putStr $ render $ oneRound game
