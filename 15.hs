#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Control.Exception (assert)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Debug.Trace  

type Space = Set Coord -- (y, x) for all non-wall coords
data Kind = Elf | Goblin deriving (Eq, Show)
data Unit = Unit { pos_ :: Coord
                 , kind_ :: Kind
                 } deriving (Eq, Show)
type Units = Map Coord Unit
data Game = Game { space_ :: Space
                 , units_ :: Units
                 }

parse :: String -> Game
parse = buildGame . concat . zipWith parseLine [0 ..] . lines where
    parseLine y = zipWith (\x c -> ((y, x), c)) [0 ..]
    buildGame :: [(Coord, Char)] -> Game
    buildGame = go (Set.empty, Map.empty) where
        go (space, units) [] = Game space units
        go (space, units) ((pos, c) : rest) = go (space', units') rest where
            space' = if c /= '#' then Set.insert pos space else space
            units' = case c of
                'E' -> Map.insert pos (Unit pos Elf) units
                'G' -> Map.insert pos (Unit pos Goblin) units
                _ -> units

render :: Game -> String
render (Game space units) =
    unlines $ map renderLine [ymin - 1 .. ymax + 1] where
        ((ymin, xmin), (ymax, xmax)) = boundingBox $ Set.toList space
        renderLine y = map (renderPos y) [xmin - 1 .. xmax + 1]
        renderPos y x
          | (y, x) `Map.member` units = case units Map.! (y, x) of
                (Unit _ Elf) -> 'E'
                (Unit _ Goblin) -> 'G'
          | (y, x) `Set.member` space = '.'
          | otherwise= '#'

up :: Coord -> Coord
up (y, x) = (pred y, x)

down :: Coord -> Coord
down (y, x) = (succ y, x)

left :: Coord -> Coord
left (y, x) = (y, pred x)

right :: Coord -> Coord
right (y, x) = (y, succ x)

adjacent :: Space -> Coord -> Space -- adjacent coords that exist in space
adjacent space c = Set.intersection space around where
    around = Set.fromDistinctAscList [up c, left c, right c, down c]

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
inRange game@(Game space units) unit = trace (show unit ++ ": " ++ show enemyAdjs) enemyAdjs where
    avail = available game
    enemyPos = Map.keys $ enemies units unit
    enemyAdjs = Set.unions $ map (adjacent avail) enemyPos

paths :: Space -> Coord -> Coord -> [[Coord]] -- Enumerate paths from a to b
paths space a b = go space a b [[]] [] where
    go space a b inprogress found
        | a == b = inprogress ++ found -- done!
        -- | a == b = trace (show b ++ "*" ++ show inprogress ++ "*" ++ show found) inprogress ++ found -- done!
        | otherwise = concatMap (\c -> go space' a c inprogress' found) adj where
            inprogress' = map (\alt -> b : alt) inprogress
            adj = adjacent space b
            -- adj = trace (show b ++ "/" ++ show inprogress ++ "/" ++ show found) $ adjacent space b
            space' = Set.delete b space

shortest :: [[Coord]] -> (Int, [[Coord]])
shortest = foldr minimal (maxBound :: Int, []) where
    minimal path (len, shortest)
        | length path < len = (length path, [path])
        | length path == len = (len, path : shortest)
        | length path > len = (len, shortest)

enemyInRange :: Game -> Unit -> Maybe Unit
enemyInRange (Game space units) unit = chooseFirst adjacentEnemies where
        enemies' = enemies units unit
        adjacent' = adjacent space $ pos_ unit
        adjacentEnemies = Map.restrictKeys enemies' adjacent'
        chooseFirst = listToMaybe . Map.elems

move :: Game -> Unit -> Unit
move game unit = unit { pos_ = up $ pos_ unit }

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
    let avail = available game
        (len, paths') = shortest $ concatMap (paths avail (pos_ unit)) inrange
        -- sort these shortest paths are sorted, so that our appropriate next
        -- step is at the front of the _first_ path.
        sortedPaths = sort paths'
    in if null sortedPaths
        then (units, unit) -- no available paths, nothing to do
        else phase5 game unit (head (head sortedPaths))

phase5 :: Game -> Unit -> Coord -> (Units, Unit) -- move one step
phase5 game@(Game space units) unit pos = trace ("Moving " ++  show unit ++ " to " ++ show pos) (units, unit') where
    avail = available game
    unit' = assert (pos `Set.member` adjacent avail (pos_ unit)) unit { pos_ = pos }

phase9 :: Game -> Unit -> (Units, Unit)
phase9 game unit = (units_ game, unit)

turn' :: Game -> Unit -> Units
turn' (Game space units) unit
    | not $ hasEnemies units unit = units -- combat has ended
    | otherwise = units'' where
        units' = Map.delete (pos_ unit) units
        game' = Game space units'
        inrange = inRange game' unit
        enemy = enemyInRange game' unit
        unit' = if isNothing enemy then move game' unit else unit
        -- TODO: Attack!
        units'' = Map.insert (pos_ unit') unit' units'

oneRound :: Game -> Game
oneRound (Game space units) =
    Game space (Map.foldl perUnit units units) where
        perUnit units' unit = turn (Game space units') $ traceShowId unit

initGame :: IO Game
initGame = do 
    input <- readFile "15.input"
    return $ parse input

smallGame :: IO Game
smallGame = do
    input <- readFile "15.small.input"
    return $ parse input

smallSearch :: IO ()
smallSearch = do
    game <- smallGame
    putStr $ render game
    let p1 = (1, 1)
    let p2 = (1, 2)
    let space = space_ game
    let space' = Set.delete (3, 2) $ Set.delete (1, 4) $ Set.delete (3, 5) space
    let (len, paths') = shortest $ paths space' p1 p2
    print $ length paths'
    print len
    print paths'

main :: IO ()
main = do
    game <- initGame
    -- part 1
    print '0'
    putStr $ render game
    print '1'
    putStr $ render $ oneRound game
