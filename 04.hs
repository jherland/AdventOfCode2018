#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative ((<|>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Time
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Utils

type Guard = Int
data Event =
      BeginShift Guard
    | FallAsleep
    | WakeUp
    deriving (Eq, Show)
data Record = Record {
    when :: UTCTime,
    what :: Event }
    deriving Show

timestamp :: ReadP UTCTime -- "1518-10-14 00:05" -> 1518-10-14 00:05:00 UTC
timestamp = do
    s <- count 16 (satisfy (const True))
    case readMaybe (s <> ":00") :: Maybe UTCTime of
        Just t -> return t
        Nothing -> pfail

event :: ReadP Event
event =
    (string "falls asleep" >> return FallAsleep) <|>
    (string "wakes up" >> return WakeUp) <|>
    BeginShift <$> between (string "Guard #") (string " begins shift") int

record :: ReadP Record
record = do
    t <- between (char '[') (char ']') timestamp
    skipSpaces
    Record t <$> event

-- ffwd timestamp to next midnight
nextMidnight :: UTCTime -> UTCTime
nextMidnight t = UTCTime (succ $ utctDay t) 0

-- If time not within [00:00, 01:00), ffwd until next midnight
normalizeTime :: UTCTime -> UTCTime
normalizeTime t = if t < oneAM then t else nextMidnight t where
    oneAM = UTCTime (utctDay t) (60 * 60)

-- Return next timestamp within [00:00, 01:00)
nextMinute :: UTCTime -> UTCTime
nextMinute t = normalizeTime (addUTCTime 60 t)

type Awake = Bool
type GuardState = (UTCTime, Guard, Awake)

nextGuard :: Guard -> Event -> Guard
nextGuard _ (BeginShift g) = g
nextGuard cur _ = cur

generateStates :: UTCTime -> UTCTime -> Guard -> Awake -> [GuardState]
generateStates start end guard awake
    | start < end = (start, guard, awake) :
                    generateStates (nextMinute start) end guard awake
    | otherwise = []

watch :: [Record] -> [GuardState]
watch [] = undefined
watch (first : rest) = go firstGuard first rest where
    firstGuard = nextGuard 0 (what first)
    go g from [] =
        go' (nextGuard g (what from))
            (when from)
            (nextMidnight (when from))
            (what from)
    go g from (to : future) =
        go' (nextGuard g (what from)) (when from) (when to) (what from) <>
        go (nextGuard g (what from)) to future
    go' g start end event' =
        generateStates start end (nextGuard g event') (event' /= FallAsleep)

watchByGuard :: [GuardState] -> IntMap [GuardState]
watchByGuard = IntMap.fromListWith (++) . map (\(t, g, a) -> (g, [(t, g, a)]))

minutesAsleep :: [GuardState] -> Int -- Number of entries where not awake
minutesAsleep = length . filter (\(_, _, awake) -> not awake)

watchByMinute :: [GuardState] -> IntMap [GuardState]
watchByMinute = IntMap.fromListWith (++) . map kv_from_state where
    kv_from_state (t, g, a) = (getMinute t, [(t, g, a)])
    getMinute = floor . toRational . (/60) . utctDayTime

readRecords :: IO [Record]
readRecords = do
    input <- readFile "04.input"
    return . parseMany record . sort . lines $ input

mostAsleep :: IntMap [GuardState] -> (Int, Int)
mostAsleep =
    swap . IntMap.foldrWithKey maxValue (0, 0) . IntMap.map minutesAsleep where
        maxValue k v mx = max mx (v, k)

main :: IO ()
main = do
    records <- readRecords
    let minutes = watch records
    let by_guard = watchByGuard minutes
    -- part 1
    let sloth = fst $ mostAsleep by_guard
    let sloth_by_minute = watchByMinute (
                            fromJust (IntMap.lookup sloth by_guard))
    let sleepytime = fst $ mostAsleep sloth_by_minute
    print $ sloth * sleepytime
    -- part 2
    let when_most_asleep_by_guard = IntMap.map mostAsleep (
                                        IntMap.map watchByMinute by_guard)
    let (_, minute', sloth') = IntMap.foldrWithKey maxAsleep (0, 0, 0)
                                when_most_asleep_by_guard where
            maxAsleep g (minute, asleep) mx = max mx (asleep, minute, g)
    print $ sloth' * minute'
