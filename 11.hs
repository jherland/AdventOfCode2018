#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Utils

powerLevel :: Int -> Coord -> Int
powerLevel serial (x, y) = digit - 5 where
    rackId = x + 10
    level = ((rackId * y) + serial) * rackId
    digit = (level `div` 100) `mod` 10

type Region = Bounds

regionCoords :: Region -> [Coord]
regionCoords = coordsWithin

regionSum :: (Coord -> Int) -> Region -> Int
regionSum f = sum . map f . regionCoords

regionsWithin :: Int -> Region -> [Region]
regionsWithin sz ((xmin, ymin), (xmax, ymax)) =
    [((x, y), (x + sz - 1, y + sz - 1)) | x <- xs, y <- ys] where
        xs = [xmin .. xmax + 1 - sz]
        ys = [ymin .. ymax + 1 - sz]

regionSize :: Region -> Int
regionSize ((xmin, ymin), (xmax, ymax)) =
    min (xmax + 1 - xmin) (ymax + 1 - ymin)

maxRegion :: (Region -> Int) -> [Region] -> (Int, Region)
maxRegion f rs = maximum (zip (map f rs) rs)

grow :: ([Region] -> (Int, Region)) -> Int -> Region -> [(Int, Int, Region)]
grow select sz grid = (score, sz, winner) : grow select (sz + 1) grid where
    (score, winner) = select (regionsWithin sz grid)

main :: IO ()
main = do
    input <- readFile "11.input"
    let serial = fromJust $ readMaybe @Int $ head $ lines input
    let grid = ((1, 1), (300, 300))
    let select = maxRegion . regionSum $ powerLevel serial
    -- part 1
    let (_, ((x, y), _)) = select (regionsWithin 3 grid)
    putStrLn $ show x ++ "," ++ show y
    -- part 2
    let candidates = takeWhile (\(s, _, _) -> s > 0) $ grow select 3 grid
    let (_, size, ((x', y'), _)) = maximum candidates
    putStrLn $ show x' ++ "," ++ show y' ++ "," ++ show size
