#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
import Utils

-- Given point p and a list of points c, return the smallest distance from p
-- to any c, and the point(s) c (one or more) that are found at that distance
nearest :: Coord -> [Coord] -> (Int, [Coord])
nearest p coords = go p (manhattanDist p (head coords), []) coords where
    go _ (lowdist, cands) [] = (lowdist, cands)
    go p' (lowdist, cands) (c:cs) =
        let
            cdist = manhattanDist p' c
        in case compare cdist lowdist of
            LT -> go p' (cdist, [c]) cs
            EQ -> go p' (lowdist, c:cands) cs
            GT -> go p' (lowdist, cands) cs

-- Given point p and a list of points c, return the sum of distances from
-- p to each c
distToAll :: Coord -> [Coord] -> Int
distToAll p = sum . map (manhattanDist p)

-- Generate all coords within the given bounding box
inside :: Bounds -> [Coord]
inside ((xmin, ymin), (xmax, ymax)) =
    [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]

-- Generate all coords at the edge of the given bounding box
perimeter :: Bounds -> [Coord]
perimeter ((xmin, ymin), (xmax, ymax)) =
    [(xmin, y) | y <- [ymin .. ymax]] ++
    [(xmax, y) | y <- [ymin .. ymax]] ++
    [(x, ymin) | x <- [xmin .. xmax]] ++
    [(x, ymax) | x <- [xmin .. xmax]]

-- For a given set of coordinates in a grid, map each coordinate c to the
-- number of grid points that are strictly nearer to c than any other c
areas :: [Coord] -> [Coord] -> Map Coord Int
areas coords grid = go coords grid Map.empty where
    go _ [] m = m
    go cs (g:gs) m = let (near:others) = snd $ nearest g cs in
        if others /= []
        then go cs gs m
        else go cs gs $ Map.alter incArea near m where
            incArea Nothing = Just 1
            incArea (Just n) = Just (n + 1)

-- For a Voronoi diagram over a grid using Manhattan distance metrics, all
-- cells intersecting the perimeter of a bounding box around all the points
-- will extend infinitely as the bounding box expands. Hence, when looking
-- for _finite_ cells, we can simply discard all cells that intersect the
-- perimeter of the bounding box.

main :: IO ()
main = do
    input <- readFile "06.input"
    -- let input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
    let coords = parseMany (coord <* eof) $ lines input
    let box = boundingBox coords
    -- part 1
    let allAreas = areas coords $ inside box
    let edgeAreas = areas coords $ perimeter box
    let finiteAreas = Map.difference allAreas edgeAreas
    let maxArea = maximum $ map snd $ Map.toList finiteAreas
    print maxArea
    -- part 2
    let region = filter (<10000) . map (`distToAll` coords) $ inside box
    print $ length region
