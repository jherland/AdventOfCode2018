#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type Coord = (Int, Int)  -- (x, y)
type Light = (Coord, Coord) -- (position, velocity)
type Frame = [Light]
type Bounds = (Coord, Coord) -- (min, max)

digits :: ReadP Int
digits = read <$> (skipSpaces >> many1 (satisfy (\c -> isDigit c || '-' == c)))

parseLight :: ReadP Light
parseLight = do
    px <- string "position=<" >> digits
    py <- char ',' >> digits
    vx <- string "> velocity=<" >> digits
    vy <- char ',' >> digits <* char '>' <* eof
    return ((px, py), (vx, vy))

parse :: [String] -> Frame
parse = map fst . concatMap (readP_to_S parseLight)

stepLight :: Light -> Light
stepLight ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx, vy))

step :: Frame -> Frame
step = map stepLight

bounds :: Frame -> Bounds
bounds = foldr1 minmax where
    minmax ((px, py), _) ((xmin, ymin), (xmax, ymax)) =
        ((min xmin px, min ymin py), (max xmax px, max ymax py))

size :: Bounds -> Int
size ((xmin, ymin), (xmax, ymax)) = (xmax - xmin) * (ymax - ymin)

firstMin :: Ord a => [a] -> a
firstMin (a:as) = go a as where
    go low [] = low
    go low (next:rest) = if next < low then go next rest else low
firstMin [] = undefined

play :: Frame -> [(Int, Frame)]
play = map (\frame -> (size (bounds frame), frame)) . iterate step

congregate :: Frame -> (Int, Frame, Int) -- return minimal (size, frame, gen)
congregate frame = firstMin $ zipWith each frames generations where
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
    let start = parse $ lines input
    let (_, end, seconds) = congregate start
    -- part 1
    putStrLn $ unlines $ render end
    -- part 2
    print seconds
