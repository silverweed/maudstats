{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Data.DateTime (toSeconds)
import Data.List (map)
import Data.Text hiding (find, map)
import Database.MongoDB
import MaudStats.Fetch
import MaudStats.Display
import MaudStats.Manip
import MaudStats.ReadLog

data Conf = Conf { dbUrl    :: String
                 , dbName   :: Database
                 , diffTime :: Int
                 , logFile  :: String
                 }

conf = Conf { dbUrl    = "localhost"
            , dbName   = "maud"
            , diffTime = 24
            , logFile  = "nginx-access.log"
            }

main :: IO ()
{-main = fetchStatsAndRun printNum-}
main = do visits <- readLogFile (logFile conf)
          print $ groupIPPairs visits
          {-let uniqvisits = groupIPPairsUniq visits-}
          {-posts <- fetchPosts $ host $ dbUrl conf-}
          {-let uniqposts = groupIPPairsUniq posts-}
          {-putStrLn $ emitData uniqvisits uniqposts-}
{-main = readLogFile (logFile conf) >>= printAll-}

run pipe = access pipe ReadStaleOk (dbName conf)

fetchPosts :: Host -> IO [IPPair] -- -> IO [(DateTime, [String])]
fetchPosts dbUrl = do pipe <- connect dbUrl
                      e <- run pipe postsWithIp
                      close pipe
                      let pairs = pairIps e
                      return pairs
                      {-return $ groupIPPairsUniq pairs-}

fetchStatsAndRun action = do pipe <- connect (host $ dbUrl conf)
                             e <- run pipe postsWithIp
                             close pipe
                             let pairs = pairIps e
                             let visits = groupIPPairsUniq pairs
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

