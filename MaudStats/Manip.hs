module MaudStats.Manip
( GroupedIPs
, IPPair
, allDaysBetween
, fillMissing
, groupIPPairs
, groupIPPairsUniq
, unix2date
) where

import Data.DateTime
import Data.Int      (Int64)
import Data.List     (groupBy, nub, sortBy, (\\))
import Data.Ord      (comparing)
import qualified Data.MultiMap as MultiMap

type IPPair = (DateTime, String)
type GroupedIPs = [(DateTime, [String])]

{-|
 - groupIPPairs takes the list of pairs (datetime, ip) and returns a list of pair [(datetime, [ip])].
 -}
groupIPPairs :: [IPPair] -> GroupedIPs
groupIPPairs = MultiMap.assocs . MultiMap.fromList . onlyDays
               where
               onlyDays :: [IPPair] -> [IPPair]
               onlyDays = map (\(dt, ip) -> (onlyDay dt, ip))
               onlyDay dt = let (y, m, d) = toGregorian' dt in fromGregorian' y m d

{-|
 - groupIPPairsUniq is like groupIPPairs, but uniques ips
 -}
groupIPPairsUniq :: [IPPair] -> GroupedIPs
groupIPPairsUniq = map (\(f, s) -> (f, nub s)) . groupIPPairs

{-|
 - fillMissing checks if there are missing dates in input and, if so, adds a pair (missingDay, []) to it
 -}
fillMissing :: GroupedIPs -> GroupedIPs
fillMissing pairs = let allDays = allDaysBetween start end
                        start   = fst $ head pairs
                        end     = fst $ last pairs
                        missing = allDays \\ (nub $ map fst pairs)
                    in  sortBy (comparing fst) $ (map (\day -> (day, [])) missing) ++ pairs

unix2date :: Int64 -> DateTime
unix2date = fromSeconds . fromIntegral

{-|
 - allDaysBetween takes a start and an end DateTime and returns an array containing all
 - days in between (included start and end)
 -}
allDaysBetween :: DateTime -> DateTime -> [DateTime]
allDaysBetween start end = start : [addSeconds (i * 86400) start | i <- [1..diff]]
                           where
                           diff = (diffSeconds end start) `div` 86400
