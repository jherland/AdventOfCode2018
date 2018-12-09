#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

data Tree = Node [Tree] [Int] deriving Show

build :: [Int] -> (Tree, [Int])
build (c : m : nums) = (node, nums'') where
    node = Node children metadata
    (children, nums') = go c [] nums
    (metadata, nums'') = splitAt m nums'
    go 0 spawn ns = (reverse spawn, ns)
    go n spawn ns = go (n - 1) (child : spawn) ns' where
        (child, ns') = build ns
build _ = undefined

metadatas :: Tree -> [[Int]]
metadatas (Node children metadata) = metadata : concatMap metadatas children

value :: Tree -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum $ map value $ extract ms cs where
    extract [] _ = []
    extract (i:is) as = a ++ extract is as where
        a = if i <= 0 || i > length as then [] else [as !! (i - 1)]

main :: IO ()
main = do
    input <- readFile "08.input"
    -- let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let (tree, []) = build $ map (read @Int) (words input)
    -- part 1
    print $ sum $ concat $ metadatas tree
    -- part 2
    print $ value tree
