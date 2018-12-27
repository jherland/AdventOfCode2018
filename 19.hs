#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP
import Utils
import Debug.Trace

type RegOrVal = Int
type Args = (RegOrVal, RegOrVal, RegOrVal)
type Instruction = (String, Args) 

type Regs = (Int, Int, Int, Int, Int, Int)
type Op = (Regs -> Args -> Regs)

type Ops = M.Map String Op
type Program = IM.IntMap Instruction

load :: Regs -> Int -> Int
load (r0, _, _, _, _, _) 0 = r0
load (_, r1, _, _, _, _) 1 = r1
load (_, _, r2, _, _, _) 2 = r2
load (_, _, _, r3, _, _) 3 = r3
load (_, _, _, _, r4, _) 4 = r4
load (_, _, _, _, _, r5) 5 = r5
load _ _ = undefined

store :: Regs -> Int -> Int -> Regs
store (_, r1, r2, r3, r4, r5) 0 value = (value, r1, r2, r3, r4, r5)
store (r0, _, r2, r3, r4, r5) 1 value = (r0, value, r2, r3, r4, r5)
store (r0, r1, _, r3, r4, r5) 2 value = (r0, r1, value, r3, r4, r5)
store (r0, r1, r2, _, r4, r5) 3 value = (r0, r1, r2, value, r4, r5)
store (r0, r1, r2, r3, _, r5) 4 value = (r0, r1, r2, r3, value, r5)
store (r0, r1, r2, r3, r4, _) 5 value = (r0, r1, r2, r3, r4, value)
store _ _ _ = undefined

ops :: Ops
ops = M.fromList [
    ("addr", \ rs (a, b, c) -> store rs c (load rs a + load rs b)),
    ("addi", \ rs (a, b, c) -> store rs c (load rs a + b)),
    ("mulr", \ rs (a, b, c) -> store rs c (load rs a * load rs b)),
    ("muli", \ rs (a, b, c) -> store rs c (load rs a * b)),
    ("banr", \ rs (a, b, c) -> store rs c (load rs a .&. load rs b)),
    ("bani", \ rs (a, b, c) -> store rs c (load rs a .&. b)),
    ("borr", \ rs (a, b, c) -> store rs c (load rs a .|. load rs b)),
    ("bori", \ rs (a, b, c) -> store rs c (load rs a .|. b)),
    ("setr", \ rs (a, _, c) -> store rs c (load rs a)),
    ("seti", \ rs (a, _, c) -> store rs c a),
    ("gtir", \ rs (a, b, c) -> store rs c (bool 0 1 $ a > load rs b)),
    ("gtri", \ rs (a, b, c) -> store rs c (bool 0 1 $ load rs a > b)),
    ("gtrr", \ rs (a, b, c) -> store rs c (bool 0 1 $ load rs a > load rs b)),
    ("eqir", \ rs (a, b, c) -> store rs c (bool 0 1 $ a == load rs b)),
    ("eqri", \ rs (a, b, c) -> store rs c (bool 0 1 $ load rs a == b)),
    ("eqrr", \ rs (a, b, c) -> store rs c (bool 0 1 $ load rs a == load rs b))]

ipBinding :: ReadP Int
ipBinding = string "#ip " >> int <* eof

instruction :: ReadP Instruction
instruction = do
    op <- count 4 get
    arg1 <- skipSpaces >> int
    arg2 <- skipSpaces >> int
    arg3 <- skipSpaces >> int <* eof
    if op `M.notMember` ops then pfail else return (op, (arg1, arg2, arg3))

execute :: Regs -> Instruction -> Regs
execute regs (op, args) = (ops M.! op) regs $ debug s args where
    s = show regs ++ " - " ++ op ++ " " ++ show args

run :: Int -> Int -> Regs -> Program -> Regs
run ipreg ip regs program =
    let done = ip `IM.notMember` program
        regs' = store regs ipreg ip
        instr = program IM.! ip
        regs'' = execute regs' instr
        ip' = load regs'' ipreg + 1
     in if done then regs else run ipreg ip' regs'' program

debug :: String -> a -> a
debug = trace

main :: IO ()
main = do
    input <- readFile "19.input"
    let (first : rest) = lines input
    let ipBoundTo = parseOne ipBinding first
    let program = IM.fromList $ zip [0..] $ parseMany (instruction <* eof) rest
    putStrLn $ "IP bound to " ++ show ipBoundTo
    print program
    -- part 1
--    let regs1 = run ipBoundTo 0 (0, 0, 0, 0, 0, 0) program
--    print $ load regs1 0
    -- part 2
    let regs2 = run ipBoundTo 0 (1, 0, 0, 0, 0, 0) program
    print $ load regs2 0
