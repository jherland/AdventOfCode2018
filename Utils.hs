module Utils where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

-- 2D geometry helpers

type Coord = (Int, Int) -- (x, y)
type Bounds = (Coord, Coord) -- (top left, bottom right)

boundingBox :: [Coord] -> Bounds
boundingBox (p:ps) = foldr minmax (p, p) ps where
    minmax (x, y) ((xmin, ymin), (xmax, ymax)) =
        ((min xmin x, min ymin y), (max xmax x, max ymax y))

coordsWithin :: Bounds -> [Coord] -- Generate all coords within a bounding box
coordsWithin ((xmin, ymin), (xmax, ymax)) =
    [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- generic helpers

theOne :: [a] -> a -- return the only item in the list, or fail
theOne [a] = a
theOne [] = error "expected one item, found none"
theOne as = error $ "expected one item, found " ++ show(length as)

firstDup :: (Eq a, Ord a) => [a] -> Maybe a
firstDup = go Set.empty where
    go _ [] = Nothing
    go seen (a : as)
        | Set.member a seen = Just a
        | otherwise = go (Set.insert a seen) as

firstMinima :: Ord a => [a] -> a
firstMinima (a:as) = go a as where
    go low [] = low
    go low (cur:rest) = if low < cur then low else go cur rest
firstMinima [] = undefined

-- parser helpers

int :: ReadP Int
int = do
    sign <- skipSpaces >> option ' ' (char '-' <|> char '+')
    let mul = if sign == '-' then (-1) else 1
    digits <- many1 (satisfy isDigit)
    return $ mul * read digits

coord :: ReadP Coord
coord = do
    x <- int
    y <- char ',' >> int
    return (x, y)

parseOne :: ReadP a -> String -> a
parseOne parser = fst . theOne . readP_to_S parser

parseMany :: ReadP a -> [String] -> [a]
parseMany parser = map (fst . theOne . readP_to_S parser)
