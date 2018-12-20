#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Utils

type OpCode = Int
type RegOrVal = Int
type Args = (RegOrVal, RegOrVal, RegOrVal)
type Instruction = (OpCode, Args) 

type Regs = (Int, Int, Int, Int)
type Op = (Regs -> Args -> Regs)

type Ops = M.Map String Op
type OpCodes = IM.IntMap String
type OpCodeAlternatives = IM.IntMap (S.Set String)

data Sample = Sample {
    before_ :: Regs,
    instr_ :: Instruction,
    after_ :: Regs }
    deriving Show

load :: Regs -> Int -> Int
load (r0, _, _, _) 0 = r0
load (_, r1, _, _) 1 = r1
load (_, _, r2, _) 2 = r2
load (_, _, _, r3) 3 = r3
load _ _ = undefined

store :: Regs -> Int -> Int -> Regs
store (_, r1, r2, r3) 0 value = (value, r1, r2, r3)
store (r0, _, r2, r3) 1 value = (r0, value, r2, r3)
store (r0, r1, _, r3) 2 value = (r0, r1, value, r3)
store (r0, r1, r2, _) 3 value = (r0, r1, r2, value)
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

registers :: ReadP Regs
registers = do
    contents <- between (char '[') (char ']') (
        many1 (satisfy (`elem` "012345679, ")))
    case readMaybe @Regs $ "(" ++ contents ++ ")" of
        Just regs -> return regs
        Nothing -> pfail

instruction :: ReadP Instruction
instruction = do
    opcode <- int
    arg1 <- skipSpaces >> int
    arg2 <- skipSpaces >> int
    arg3 <- skipSpaces >> int
    return (opcode, (arg1, arg2, arg3))

sample :: ReadP Sample
sample = do
    before <- string "Before:" >> skipSpaces >> registers <* char '\n'
    instr <- instruction <* char '\n'
    after <- string "After:" >> skipSpaces >> registers <* string "\n\n"
    return (Sample before instr after)

samples :: ReadP [Sample]
samples = many sample <* string "\n\n"

testSample :: Sample -> Ops -- Submap of ops that fulfills this sample
testSample (Sample before (_, args) after) = M.filter testOp ops where
    testOp op = op before args == after

opcodeAlts :: Sample -> (Int, [String]) -- Possible opcode alternatives
opcodeAlts s@(Sample _ (opcode, _) _) = (opcode, M.keys $ testSample s)

reduceOpcodes :: OpCodeAlternatives -> OpCodes
reduceOpcodes alts = go (IM.assocs alts) [] IM.empty where
    go [] [] done = done
    go [] pending done = go pending [] done
    go ((code, names) : todo) pending done = go todo pending' done' where
            resolved = IM.elems done
            remainingWords = S.filter (`notElem` resolved) names
            foundWord = S.size remainingWords == 1
            name = S.elemAt 0 remainingWords
            pending' = if foundWord
                          then pending
                          else (code, remainingWords) : pending
            done' = if foundWord
                       then IM.insert code name done
                       else done

deduceOpcodes :: [(OpCode, [String])] -> OpCodes
deduceOpcodes = go IM.empty where
    go deduced [] = reduceOpcodes deduced
    go deduced ((code, alts):tests) = go deduced' tests where
        deduced' = IM.insertWith S.intersection code (S.fromList alts) deduced

execute :: OpCodes -> Regs -> Instruction -> Regs
execute opcodes regs (opcode, args) = (ops M.! (opcodes IM.! opcode)) regs args

run :: OpCodes -> Regs -> [Instruction] -> Regs
run opcodes = foldl (execute opcodes)

main :: IO ()
main = do
    input <- readFile "16.input"
    let (tests, rest) = theOne $ readP_to_S samples input
    let program = parseMany (instruction <* eof) $ lines rest
    -- part 1
    let opcodeAlternatives = map opcodeAlts tests
    let threeOrMore = filter (>=3) . map (length . snd)
    print $ length $ threeOrMore opcodeAlternatives
    -- part 2
    let opcodes = deduceOpcodes opcodeAlternatives
    let (r0, _, _, _) = run opcodes (0, 0, 0, 0) program
    print r0
