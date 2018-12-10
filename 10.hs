#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.ReadP
import Utils

type Light = (Coord, Coord) -- (position, velocity)
type Frame = [Light]

coord' :: ReadP Coord
coord' = between (char '<') (char '>') coord

light :: ReadP Light
light = do
    position <- string  "position=" >> coord'
    velocity <- string " velocity=" >> coord'
    return (position, velocity)

step :: Frame -> Frame
step = map stepOne where
    stepOne ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx, vy))

size :: Bounds -> Int
size ((xmin, ymin), (xmax, ymax)) = (xmax - xmin) * (ymax - ymin)

bounds :: Frame -> Bounds
bounds = boundingBox . map fst

play :: Frame -> [(Int, Frame)]
play = map (\frame -> (size (bounds frame), frame)) . iterate step

congregate :: Frame -> (Int, Frame, Int) -- return minimal (size, frame, gen)
congregate frame = firstMinima $ zipWith each frames generations where
    frames = play frame
    generations = iterate (+1) 0
    each (sz, f) gen = (sz, f, gen)

render :: Frame -> [String]
render f = go (bounds f) (map fst f) where
    go ((xmin, ymin), (xmax,ymax)) ps =
        [[pixel x y ps | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
    pixel x y ps = if (x, y) `elem` ps then '#' else '.'

main :: IO ()
main = do
    input <- readFile "10.input"
    let start = parseMany light $ lines input
    let (_, end, seconds) = congregate start
    -- part 1
    putStrLn $ unlines $ render end
    -- part 2
    print seconds
