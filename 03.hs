#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import Text.ParserCombinators.ReadP
import Utils

data Claim = Claim {
    id_ :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int }
    deriving Show

claim :: ReadP Claim
claim = do
    _id <- char '#' >> int
    _x <- string " @ " >> int
    _y <- char ',' >> int
    _w <- string ": " >> int
    _h <- char 'x' >> int <* eof
    return $ Claim _id _x _y _w _h

type Ids = [Int]

insertCoord :: Int -> Coord -> Map Coord Ids -> Map Coord Ids
insertCoord id' = Map.alter append where
    append Nothing = Just [id']
    append (Just v) = Just (id' : v)

insertClaim :: Claim -> Map Coord Ids -> Map Coord Ids
insertClaim claim_ m = foldr (insertCoord (id_ claim_)) m coords where
    coords = [(i, j) | i <- [start_x .. end_x], j <- [start_y .. end_y]]
    start_x = x claim_
    end_x = start_x + w claim_ - 1
    start_y = y claim_
    end_y = start_y + h claim_ - 1

assignments :: [Claim] -> Map Coord Ids
assignments = foldr insertClaim Map.empty

main :: IO ()
main = do
    input <- readFile "03.input"
    let claims = parseMany claim $ lines input -- mapMaybe parse (lines input)
    let assigned = assignments claims
    let contested = Map.filterWithKey (\_ v -> length v > 1) assigned
    -- part 1
    print $ Map.size contested
    -- part 2
    let allIds = IntSet.fromList [id_ c | c <- claims]
    let contestedIds = IntSet.fromList $ concatMap snd (Map.toList contested)
    print $ head $ IntSet.toList $ IntSet.difference allIds contestedIds
