{-# LANGUAGE BangPatterns #-}
module MaudStats.ReadLog
( readLogFile
) where

import Control.DeepSeq
import System.IO
import Data.DateTime   (DateTime, fromGregorian')
import Data.List       (break, elemIndex, isInfixOf, words)
import MaudStats.Manip (IPPair)

{-|
 - readLogFile reads a given nginx log file and returns a list [(date, ip)]
 -}
readLogFile :: String -> IO [IPPair]
readLogFile fname = do handle <- openFile fname ReadMode
                       hSetBuffering handle LineBuffering
                       input  <- hGetContents handle
                       return $ aggregateLines' $ lines input
                       --withFile fname ReadMode $ \handle -> aggregateLines handle []

aggregateLines' :: [String] -> [IPPair]
aggregateLines' [] = []
aggregateLines' (line:lns) = let !l = (ipPairFrom line) in l:aggregateLines' lns

{-|
 - aggregateLines builds the list of ip pairs, filtering only interesting entries
 - (selected via `filterCR`)
 -}
aggregateLines :: Handle -> [IPPair] -> IO [IPPair]
aggregateLines handle list = hIsEOF handle >>= \isEof ->
                                case isEof of
                                    True  -> return list
                                    False -> hGetLine handle >>= \line ->
                                        case filterCR line of
                                            True  -> aggregateLines handle $ (ipPairFrom line):list
                                            False -> aggregateLines handle list

filterCR :: String -> Bool
filterCR = isInfixOf "crunchy.rocks"

{-|
 - ipPairFrom takes a raw log line and returns a pair (date, ip)
 -}
ipPairFrom :: String -> IPPair
ipPairFrom line = let w      = words line
                      ip     = head w
                      date   = tail $ w !! 3
                      -- This manual date parsing is faster than parseDateTime
                      (a, _) = break (==':') date
                      [sday, smonth, syear] = words $ map (\c -> if c == '/' then ' ' else c) a
                      day    = read sday  :: Int
                      year   = read syear :: Integer
                      month  = case smonth `elemIndex` ["Jan","Feb","Mar","Apr","May","Jun"
                                                       ,"Jul","Aug","Sep","Oct","Nov","Dec"]
                               of Just i  -> i
                                  Nothing -> error $ "Invalid month: " ++ smonth
                  in (fromGregorian' year month day, ip)
