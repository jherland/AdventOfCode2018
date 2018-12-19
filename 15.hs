#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Control.Exception (assert)
import Data.List (intercalate, nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace
import Utils

type Space = Set Coord -- (y, x) for all non-wall coords
data Kind = Elf | Goblin deriving (Eq, Ord, Show)
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

enemies :: Units -> Unit -> Units
enemies units unit = case kind_ unit of
    Elf -> Map.filter ((== Goblin) . kind_) units
    Goblin -> Map.filter ((== Elf) . kind_) units

hasEnemies :: Units -> Unit -> Bool
hasEnemies units = not. Map.null . enemies units

kindsLeft :: Game -> [Kind] -- what kinds are left in combat
kindsLeft (Game _ units) = nub $ map kind_ $ Map.elems units

openSquares :: Game -> Space
openSquares (Game space units) = foldr Set.delete space $ Map.keys units

inRange :: Game -> Unit -> Space -- Open squares adjacent to enemies
inRange game@(Game _ units) unit = enemyAdjs where
    avail = openSquares game
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

nearest :: Space -> Coord -> Space -> Maybe (Coord, Int) -- find b nearest a
nearest space a bs =
    let distmap = distanceFrom space a
        bDistances = Map.restrictKeys distmap bs
     in if Map.null bDistances
           then Nothing
           else Just (swap $ minimum $ map swap $ Map.toList bDistances)

nextMove :: Space -> Space -> Coord -> Maybe Coord
nextMove space inrange src = case nearest space src inrange of
    Nothing -> Nothing
    Just (target, dist) -> moveTo where
        maybeAdj = trace ("  Chose target " ++ show target ++ " at distance " ++
            show dist) $ nearest space target $ adjacent space src
        moveTo = fst <$> maybeAdj

move :: Game -> Unit -> Coord -> Unit
move game unit to =
    let possibleMoves = adjacent (openSquares game) (pos_ unit)
     in assert (to `Set.member` possibleMoves) unit { pos_ = to }
 
nearEnemy :: Game -> Unit -> Maybe Unit
nearEnemy (Game space units) unit = chooseWeakest adjacentEnemies where
        enemies' = enemies units unit
        adjacent' = adjacent space $ pos_ unit
        adjacentEnemies = Map.restrictKeys enemies' adjacent'
        chooseWeakest = listToMaybe . sortOn weakest . Map.elems
        weakest enemy = (hp_ enemy, pos_ enemy)

attack :: Unit -> Unit -> Unit
attack attacker victim = victim { hp_ = hp_ victim - ap_ attacker }

turn :: Game -> Unit -> (Units, Unit)
turn = phase1

phase1 :: Game -> Unit -> (Units, Unit) -- Are there any enemies?
phase1 (Game space units) unit
    | not $ hasEnemies units unit = (units, unit) -- no enemies, end of combat
    | otherwise = (units''', unit') where -- lift unit from board while in turn
        units' = trace (show unit) $ Map.delete (pos_ unit) units
        (units'', unit') = phase2 (Game space units') unit
        units''' = Map.insert (pos_ unit') unit' units'' -- place back on board

phase2 :: Game -> Unit -> (Units, Unit) -- Are there any target squares?
phase2 game@(Game _ units) unit =
    let inrange = inRange game unit
    in if Set.null inrange
        then (units, unit) -- no available target squares, nothing to do
        else phase3 game unit inrange

phase3 :: Game -> Unit -> Space -> (Units, Unit) -- Do we need to move?
phase3 game unit inrange
    | pos_ unit `Set.member` inrange = phase5 game unit -- already in range
    | otherwise = phase4 game unit inrange -- need to move

phase4 :: Game -> Unit -> Space -> (Units, Unit) -- Figure out where to move
phase4 game@(Game _ units) unit inrange =
    let moveTo' = nextMove (openSquares game) inrange (pos_ unit)
        moveTo = trace (" Move " ++ show unit ++ " -> " ++ show moveTo') moveTo'
    in maybe (trace " No move found" (units, unit)) (phase5 game . move game unit) moveTo

phase5 :: Game -> Unit -> (Units, Unit) -- Can we attack?
phase5 game@(Game _ units) unit =
    case nearEnemy game unit of
      Nothing -> (units, unit) -- no enemy to attack
      Just victim -> trace (" Attacking " ++ show victim) $ phase6 game unit (attack unit victim)

phase6 :: Game -> Unit -> Unit -> (Units, Unit) -- Dead?
phase6 (Game _ units) unit victim
    | hp_ victim <= 0 = trace (" Died: " ++ show victim) (Map.delete (pos_ victim) units, unit)
    | otherwise = (Map.insert (pos_ victim) victim units, unit)

oneRound :: Game -> Game
oneRound (Game space units) = Game space units''' where
    units''' = fst (foldl perUnit (units, []) $ Map.keys units)
    perUnit (units', skip) pos
        | pos `elem` skip = (units', skip) -- a unit moved into this square
        | Map.notMember pos units' = (units', skip) -- this unit has died
        | otherwise = (units'', pos_ unit : skip) where
            (units'', unit) = turn (Game space units') (units' Map.! pos)

untilCombatEnd :: Game -> Int -> (Game, Int)
untilCombatEnd game rounds
    | length (kindsLeft game) <= 1 = (game, rounds)
    | otherwise = untilCombatEnd (oneRound game) (rounds + 1)

initGame :: Int -> IO Game
initGame elfAttack = do
    input <- readFile "15.input"
    return $ parse elfAttack input

elvesOnSteroids :: Int -> IO (Int, Game, Int)
elvesOnSteroids elfAttack = do
    game <- initGame elfAttack
    let elfCount g = length $ filter (==Elf) $ map kind_ $ Map.elems $ units_ g
    let before = elfCount game
    let (end, rounds) = untilCombatEnd game 0
    let after = elfCount end
    if before == after
       then return (elfAttack, end, rounds)
       else elvesOnSteroids (elfAttack + 1)

main :: IO ()
main = do
    -- part 1
    game <- initGame 3
    putStr $ render game
    let (end, rounds) = untilCombatEnd game 0
    putStr $ render end
    print ((rounds - 1) * sum(map hp_ $ Map.elems $ units_ end))
    -- part 2
    (_, end', rounds') <- elvesOnSteroids 4
    putStr $ render end'
    print ((rounds' - 1) * sum(map hp_ $ Map.elems $ units_ end'))
