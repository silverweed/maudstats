{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Database.MongoDB
import MaudStats.Fetch
import MaudStats.Display
import MaudStats.Manip
import Data.Text hiding (find)

data Conf = Conf { dbUrl    :: String
                 , dbName   :: Database
                 , diffTime :: Int
                 }

conf = Conf { dbUrl    = "database.jail"
            , dbName   = "maud"
            , diffTime = 24
            }

main = fetchStatsAndRun printNum

run pipe = access pipe ReadStaleOk (dbName conf)

fetchStatsAndRun action = do pipe <- connect (host $ dbUrl conf)
                             e <- run pipe postsWithIp 
                             close pipe
                             let pairs = pairIps e
                             let visits = groupVisitsUniq pairs
                             action visits

-- FIXME: this ought to stay in Fetch, but compiler cannot resolve ambiguity
-- (and neither can we :/)
{-|
 - postsWithIp returns all the posts with an associated IP, sorted by date.
 -}
postsWithIp = find (select ["author.ip" =: ["$exists" =: True]] "posts") 
                { project = ["_id" =: 0, "author.ip" =: 1, "date" =: 1]
                , sort    = ["date" =: 1]
                } >>= rest

