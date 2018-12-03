#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet

data Claim = Claim {
    id_ :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int }
    deriving Show

parse :: String -> Maybe Claim -- "#{id} @ {x},{y}: {w}x{h}"
parse s = go $ splitOn " " s where
    go [_id, "@", loc, size] = buildClaim (parseId _id) (parseLoc $ splitOn "," loc) (parseSize $ splitOn "x" size) where
        parseId ('#' : id') = readMaybe id' :: Maybe Int
        parseId _ = Nothing
        parseLoc [x', y'_] = (readMaybe x' :: Maybe Int, readMaybe y' :: Maybe Int) where
            y' = head $ splitOn ":" y'_
        parseLoc _ = (Nothing, Nothing)
        parseSize [w', h'] = (readMaybe w' :: Maybe Int, readMaybe h' :: Maybe Int)
        parseSize _ = (Nothing, Nothing)
        buildClaim (Just id'') ((Just x''), (Just y'')) ((Just w''), (Just h'')) = Just (Claim id'' x'' y'' w'' h'')
        buildClaim _ _ _ = Nothing
    go _ = Nothing

type Coord = (Int, Int)
type Ids = [Int]

insertCoord :: Int -> Coord -> Map Coord Ids -> Map Coord Ids
insertCoord id' coord m = Map.alter append coord m where
    append Nothing = Just [id']
    append (Just v) = Just (id' : v)

insertClaim :: Claim -> Map Coord Ids -> Map Coord Ids
insertClaim claim m = foldr (insertCoord (id_ claim)) m coords where
    coords = [(i, j) | i <- enumFromTo start_x end_x, j <- enumFromTo start_y end_y]
    start_x = x claim
    end_x = start_x + (w claim) - 1
    start_y = y claim
    end_y = start_y + (h claim) - 1

assignments :: [Claim] -> Map Coord Ids
assignments claims = go Map.empty claims where
    go m [] = m
    go m (c : cs) = go (insertClaim c m) cs

main :: IO ()
main = do
    input <- readFile "03.input"
    let claims = catMaybes $ map parse $ lines input
    let assigned = assignments claims
    let contested = Map.filterWithKey (\_ v -> (length v) > 1) assigned
    -- part 1
    print $ Map.size contested
    -- part 2
    let allIds = IntSet.fromList [id_ claim | claim <- claims]
    let conflictingIds = IntSet.fromList $ concat [v | (_, v) <- Map.toList contested]
    print $ head $ IntSet.toList $ IntSet.difference allIds conflictingIds
