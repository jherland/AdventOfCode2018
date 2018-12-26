#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Utils

data Acre = Open | Trees | Lumberyard deriving (Eq, Ord)

instance Show Acre where
    show Open = "."
    show Trees = "|"
    show Lumberyard = "#"

type Area = Map Coord Acre

parse :: String -> Area
parse = buildArea . concat . zipWith parseLine [0 ..] . lines where
    parseLine y = zipWith (\x c -> ((y, x), c)) [0 ..]
    buildArea = go Map.empty where
        go area [] = area
        go area ((pos, c) : rest) = go (Map.insert pos acre area) rest where
            acre = case c of
                '.' -> Open
                '|' -> Trees
                '#' -> Lumberyard
                _ -> undefined

render :: Area -> String
render area =
    unlines $ map renderLine [ymin .. ymax] where
        ((ymin, xmin), (ymax, xmax)) = boundingBox $ Map.keys area
        renderLine y = concatMap (renderPos y) [xmin .. xmax]
        renderPos y x = show $ area Map.! (y, x)

adjacent :: Coord -> [Coord]
adjacent (y, x) = [
    (y - 1, x - 1), (y - 1, x), (y - 1, x + 1),
    (y,     x - 1),             (y,     x + 1),
    (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]

tally :: [Acre] -> (Int, Int, Int)
tally acres = (os, ts, ls) where
    os = length $ filter (== Open) acres
    ts = length $ filter (== Trees) acres
    ls = length $ filter (== Lumberyard) acres

next :: Area -> Coord -> Acre -> Acre
next area pos a =
    let (_, ts, ls) = tally $ mapMaybe (area Map.!?) (adjacent pos)
     in case a of
          Open -> if ts >= 3 then Trees else Open
          Trees -> if ls >= 3 then Lumberyard else Trees
          Lumberyard -> if ls >= 1 && ts >= 1 then Lumberyard else Open

minute :: Area -> Area
minute area = Map.mapWithKey (next area) area

readArea :: String -> IO Area
readArea path = do
    input <- readFile path
    return $ parse input

resourceValue :: Area -> Int
resourceValue area = ts * ls where
    (_, ts, ls) = tally $ Map.elems area

findRepeats :: (Eq a, Ord a) => [a] -> (Int, Int)
findRepeats = go Map.empty . zip [0..] where
    go _ [] = undefined
    go stats ((i, a) : as)
      | a `Map.notMember` stats = go (Map.insert a i stats) as
      | otherwise = (first, i - first) where
          first = stats Map.! a

main :: IO ()
main = do
    area <- readArea "18.input"
    -- part 1
    let area' = iterate minute area !! 10
    -- putStr $ render area'
    print $ resourceValue area'
    -- part 2
    let (start, period) = findRepeats $ iterate minute area
    let i = (1000000000 - start) `mod` period + start
    print $ resourceValue $ iterate minute area !! i
