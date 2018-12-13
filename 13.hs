#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing, maybeToList)
import Utils

--             -    |    /    \    +
data Segment = HZ | VT | T1 | T2 | CX deriving (Eq, Show)
--               ^   v   >   <
data Direction = N | S | E | W deriving (Eq, Show)
data Turn = L | A | R deriving (Eq, Ord, Enum, Show) -- Left/Ahead/Right
type Track = (Coord, Segment) -- (y, x) -> HZ/VT/T1/T2
type Cart = (Coord, (Direction, Turn)) -- (y, x) -> (N/S/E/W, Left)
-- Yeah... we're flipping x/y in Coords here to process Carts (i.e. maps,
-- i.e. sorted lists of coords) in the right order (each row left to right,
-- then top to bottom)

type Tracks = Map Coord Segment
type Carts = Map Coord (Direction, Turn)

type Collision = Coord

parsePos :: Coord -> Char -> (Maybe Track, Maybe Cart)
parsePos pos c = case c of
    '-'  -> (Just (pos, HZ), Nothing)
    '|'  -> (Just (pos, VT), Nothing)
    '/'  -> (Just (pos, T1), Nothing)
    '\\' -> (Just (pos, T2), Nothing)
    '+'  -> (Just (pos, CX), Nothing)
    '^'  -> (Just (pos, VT), Just (pos, (N, L)))
    'v'  -> (Just (pos, VT), Just (pos, (S, L)))
    '>'  -> (Just (pos, HZ), Just (pos, (E, L)))
    '<'  -> (Just (pos, HZ), Just (pos, (W, L)))
    ' '  -> (Nothing, Nothing)
    _ -> error "No parse"

parseGrid :: String -> (Tracks, Carts)
parseGrid input =
    collect $ concat $ zipWith parseLine [0 ..] $ lines input where
        parseLine y line = map (\(x, c) -> parsePos (y, x) c) $ zip [0 ..] line
        collect l = (buildMapFromMaybes tracks, buildMapFromMaybes carts) where
            buildMapFromMaybes ms = Map.fromList $ catMaybes ms
            (tracks, carts) = unzip l

nextCoord :: Coord -> Direction -> Coord
nextCoord (y, x) N = (y - 1, x)
nextCoord (y, x) S = (y + 1, x)
nextCoord (y, x) E = (y, x + 1)
nextCoord (y, x) W = (y, x - 1)

turn :: Segment -> (Direction, Turn) -> (Direction, Turn)
turn HZ (N, _) = undefined
turn HZ (S, _) = undefined
turn HZ (E, t) = (E, t)
turn HZ (W, t) = (W, t)
turn VT (N, t) = (N, t)
turn VT (S, t) = (S, t)
turn VT (E, _) = undefined
turn VT (W, _) = undefined
turn T1 (N, t) = (E, t)
turn T1 (S, t) = (W, t)
turn T1 (E, t) = (N, t)
turn T1 (W, t) = (S, t)
turn T2 (N, t) = (W, t)
turn T2 (S, t) = (E, t)
turn T2 (E, t) = (S, t)
turn T2 (W, t) = (N, t)
turn CX (N, L) = (W, A)
turn CX (N, A) = (N, R)
turn CX (N, R) = (E, L)
turn CX (S, L) = (E, A)
turn CX (S, A) = (S, R)
turn CX (S, R) = (W, L)
turn CX (E, L) = (N, A)
turn CX (E, A) = (E, R)
turn CX (E, R) = (S, L)
turn CX (W, L) = (S, A)
turn CX (W, A) = (W, R)
turn CX (W, R) = (N, L)

move :: Tracks -> Cart -> Cart
move tracks (pos, (dir, nturn)) = (pos', dirturn') where
    pos' = nextCoord pos dir
    segment' = tracks Map.! pos'
    dirturn' = turn segment' (dir, nturn)

collides :: Carts -> Cart -> Maybe Collision
collides carts (pos, _) = if pos `Map.member` carts then Just pos else Nothing

tick :: Tracks -> Carts -> (Carts, [Collision])
tick tracks_ carts_ = go tracks_ carts_ (Map.keys carts_) [] where
    go _ carts [] colls = (carts, colls)
    go tracks carts (pos:rest) colls
        | Map.notMember pos carts = go tracks carts rest colls
        | otherwise = go tracks carts'' rest colls' where
            cart = (pos, carts Map.! pos)
            carts' = Map.delete pos carts
            cart' = move tracks cart
            collision = collides carts' cart'
            colls' = maybeToList collision <> colls
            carts'' = if isNothing collision
                then uncurry Map.insert cart' carts'
                else Map.delete (fst cart') carts'

crash :: Tracks -> Carts -> (Carts, [Collision]) -- tick until first collision
crash tracks carts = let
    (carts', colls) = tick tracks carts in
    if null colls then crash tracks carts' else (carts', colls)

destructionDerby :: Tracks -> Carts -> Cart -- crash until only one cart left
destructionDerby tracks carts = let (carts', _) = crash tracks carts in
    if Map.size carts' <= 1
    then theOne $ Map.toList carts'
    else destructionDerby tracks carts'

main :: IO ()
main = do
    input <- readFile "13.input"
    let (tracks, carts) = parseGrid input
    -- part 1
    let (_, colls) = crash tracks carts
    let collision = head colls
    putStrLn $ show (snd collision) ++ "," ++ show (fst collision)
    -- part 2 -- keep crashing until only one cart left
    let survivor = destructionDerby tracks carts
    putStrLn $ show (snd $ fst survivor) ++ "," ++ show (fst $ fst survivor)
