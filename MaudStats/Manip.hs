module MaudStats.Manip
( GroupedIPs
, IPPair
, groupIPPairs
, groupIPPairsUniq
, unix2date
) where

import Data.DateTime (DateTime, fromSeconds)
import Data.Int      (Int64)
import Data.List     (groupBy, nub)
import qualified Data.MultiMap as MultiMap

type IPPair = (DateTime, String)
type GroupedIPs = [(DateTime, [String])]

{-|
 - groupIPPairs takes the list of pairs (datetime, ip) and returns a list of pair [(datetime, [ip])].
 -}
groupIPPairs :: [IPPair] -> GroupedIPs
groupIPPairs = MultiMap.assocs . MultiMap.fromList -- FIXME: restrict granularity to DAYS, not seconds

{-|
 - groupIPPairsUniq is like groupIPPairs, but uniques ips
 -}
groupIPPairsUniq :: [IPPair] -> GroupedIPs
groupIPPairsUniq = map (\(f, s) -> (f, nub s)) . groupIPPairs

unix2date :: Int64 -> DateTime
unix2date = fromSeconds . fromIntegral
