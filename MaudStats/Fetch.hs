{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module MaudStats.Fetch 
( pairIps
, allIps
) where

import Database.MongoDB
import Data.DateTime
import MaudStats.Types
import Data.List (nub)

{-|
 - pairIps extracts a list of pairs (date, ip) from the documents
 -}
pairIps :: [Document] -> [IPPair]
pairIps [] = []
pairIps (doc:docs) | date /= Nothing && ip /= Nothing = let (Just d, Just i) = (date, ip)
                                                        in (d, i) : pairIps docs
                   | otherwise = pairIps docs
                   where
                   ip   = doc !? "author.ip"
                   date = doc !? "date"


{-|
 - allIps returns a list with all the IPs in the documents given, uniqued.
 -}
allIps :: [Document] -> [String]
allIps [] = []
allIps docs = nub $ map extractIp $ docs
              where
              extractIp doc = case doc !? "author.ip" of
                              Just v  -> v
                              Nothing -> ""
