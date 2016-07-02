module MaudStats.Manip
( groupVisits
, groupVisitsUniq
) where

import Data.DateTime (DateTime)
import Data.List     (groupBy, nub)
import MaudStats.Types
import MaudStats.Fetch

{-|
 - groupVisits takes a list [(date, ip)] and returns a list [(day, [(date, ip)])]
 - with the ips grouped by days.
 -}
groupVisits :: [IPPair] -> [(DateTime, [IPPair])]
groupVisits [] = []
groupVisits pairs = map pairWithDay $ groupBy isSameDay pairs
                    where
                    pairWithDay :: [IPPair] -> (DateTime, [IPPair])
                    pairWithDay [] = error "Empty list given to pairWithDay!"
                    pairWithDay list = (getDay . fst . head $ list, list)

{-|
 - Like groupVisits, but uniques IPs and returns but a [(day, [ip])]
 -}
groupVisitsUniq :: [IPPair] -> [(DateTime, [String])]
groupVisitsUniq pairs = map getIps $ groupVisits pairs
                        where
                        getIps :: (DateTime, [IPPair]) -> (DateTime, [String])
                        getIps (date, prs) = (date, nub $ map snd prs)

