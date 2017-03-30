module MaudStats.Manip
( IPPair
, groupVisits
, groupVisitsUniq
, unix2date
) where

import Data.DateTime (DateTime, fromSeconds)
import Data.Int      (Int64)
import Data.List     (groupBy, nub)
import qualified Data.MultiMap as MultiMap

type IPPair = (DateTime, String)

{-|
 - groupVisits takes the list of pairs (unixtime, ip) and returns a list of pair [(datetime, [ip])].
 -}
groupVisits :: [IPPair] -> [(DateTime, [String])]
groupVisits = MultiMap.assocs . MultiMap.fromList

{-|
 - groupVisitsUniq is like groupVisits, but uniques ips
 -}
groupVisitsUniq :: [IPPair] -> [(DateTime, [String])]
groupVisitsUniq = map (\(f, s) -> (f, nub s)) . groupVisits

unix2date :: Int64 -> DateTime
unix2date = fromSeconds . fromIntegral
