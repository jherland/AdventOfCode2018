#!/usr/bin/env stack
-- stack --resolver lts-12.10 script --package containers

{-# OPTIONS_GHC -Wall #-}

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Text.ParserCombinators.ReadP
import Utils

type Step = Char
type Dep = (Step, Step)  -- fst must finish before snd can begin
type Deps = Map Step [Step] -- key cannot start until [val] are finished

dep :: ReadP Dep
dep = do
    a <- string "Step " >> get
    b <- string " must be finished before step " >> get <*
         string " can begin." <* eof
    return (a, b)

-- Convert (a, b) pairs into mapping b -> [a]
buildDeps :: [Dep] -> Deps
buildDeps = go Map.empty where
    go m [] = m
    go m ((a, b):os) = go addDep os where
        addDep = Map.insertWith (++) b [a] $ Map.insertWith (++) a [] m

-- Return steps that have zero dependencies
avail :: Deps -> [Step]
avail = Map.keys . fst . Map.partition null

-- Return next available step (using alphabet to break ties)
nextAvail :: Deps -> Maybe Step
nextAvail = listToMaybe . avail

-- Start a job by removing it from the map keys, i.e. no longer available
startJob :: Step -> Deps -> Deps
startJob = Map.delete

-- Finish a job by removing it from other jobs' dependencies
finishJob :: Step -> Deps -> Deps
finishJob s = Map.map (filter (/=s))

-- Return all steps in sorted order
inOrder :: Deps -> [Step]
inOrder = go [] where
    go finished jobs = case nextAvail jobs of
        Nothing -> reverse finished
        Just s -> go (s:finished) jobs' where
            jobs' = finishJob s $ startJob s jobs

-- How long does it take to complete a job
workTime :: Step -> Int
workTime c = 60 + ord c - (ord 'A' - 1)

data Worker = Worker {
    job :: Maybe Step,
    remains :: Int }
    deriving (Eq, Show)
data State = State {
    t :: Int,
    todo :: Deps,
    done :: [Step],
    workers :: [Worker] }
    deriving Show

idleWorker :: Worker
idleWorker = Worker Nothing 0

-- Return next state of a Worker, deliver finished jobs and return to idle
work2idle :: (Worker, Deps) -> (Worker, Deps, Maybe Step)
work2idle (Worker Nothing _, jobs) = (idleWorker, jobs, Nothing) -- still idle
work2idle (Worker (Just job') remains', jobs) =
    let remains'' = remains' - 1 in
    if remains'' > 0
    then (Worker (Just job') remains'', jobs, Nothing) -- still working
    else (idleWorker, finishJob job' jobs, Just job') -- finished

-- Return next state of a Worker, picking up new jobs if idle
idle2work :: (Worker, Deps) -> (Worker, Deps)
idle2work (Worker Nothing _, jobs) =
    maybe (idleWorker, jobs) begin (nextAvail jobs) where
        begin newjob = (newWorker, startJob newjob jobs) where
            newWorker = Worker (Just newjob) (workTime newjob)
idle2work (w, jobs) = (w, jobs) -- already busy

-- Deliver finished jobs
handoff :: (Deps, [Worker]) -> (Deps, [Worker], [Step])
handoff (jobs, ws) = go jobs ws [] [] where
    go jobs' [] reported finished = (jobs', reported, finished)
    go jobs' (w:ws') reported finished =
        go jobs'' ws' (w':reported) finished' where
            (w', jobs'', maybeDone) = work2idle (w, jobs')
            finished' = maybeToList maybeDone ++ finished

-- Pick up new jobs
pickup :: (Deps, [Worker]) -> (Deps, [Worker])
pickup (jobs, ws) = go jobs ws [] where
    go jobs' [] assigned = (jobs', assigned)
    go jobs' (w:ws') assigned =
        go jobs'' ws' (w':assigned) where
            (w', jobs'') = idle2work (w, jobs')

-- Daily standup for the workers, advance each of them, and thread Deps through
standup :: (Deps, [Worker]) -> (Deps, [Worker], [Step])
standup (jobs, ws) = (jobs'', ws'', finished) where
    (jobs', ws', finished) = handoff (jobs, ws)
    (jobs'', ws'') = pickup (jobs', ws')

-- Return state at t0, workers have just picked up their first jobs
initState :: Int -> Deps -> State
initState numWorkers jobs =
    State { t = 0, todo = jobs', done = [], workers = initWorkers } where
        idleWorkers = replicate numWorkers idleWorker
        (jobs', initWorkers) = pickup (jobs, idleWorkers)

-- Advance one time unit and return updated state
next :: State -> State
next (State t' jobs finished ws) =
    State { t = t' + 1, todo = jobs', done = done', workers = ws' } where
        (jobs', ws', nowDone) = standup (jobs, ws)
        done' = nowDone ++ finished

-- Tick over the clock until all workers are idle
deathMarch :: State -> IO State
deathMarch s =
    if all (==idleWorker) (workers s)
    then return s
    else do
        putStr $ showState s
        deathMarch $ next s

-- Show current state
showState :: State -> String
showState s = show (t s) ++ ":" ++ workerStr ++ " | " ++ doneStr ++ "\n" where
        workerStr = concatMap (\w -> [' ', fromMaybe '.' (job w)]) (workers s)
        doneStr = reverse (done s)

-- Pipe output through 'dot -Txlib'
dot :: [Dep] -> String
dot o = "digraph ordering {\n" ++ concatMap go o ++ "}\n" where
    go (a, b) = "    " ++ [a] ++ " -> " ++ [b] ++ "\n"

main :: IO ()
main = do
    input <- readFile "07.input"
    let deps = parseMany dep $ lines input
    let jobs = buildDeps deps
    -- part 1
    print $ inOrder jobs
    -- part 2
    finished <- deathMarch $ initState 5 jobs
    print $ t finished
