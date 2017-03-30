module MaudStats.ReadLog
( readLogFile
) where

import System.IO       (Handle, IOMode(..), withFile, hGetLine, hClose, hIsEOF)
import Data.DateTime   (DateTime, parseDateTime)
import Data.List       (isInfixOf, words)
import MaudStats.Manip (IPPair)

{-|
 - readLogFile reads a given nginx log file and returns a list [(date, ip)]
 -}
readLogFile :: String -> IO [IPPair]
readLogFile fname = withFile fname ReadMode $ \handle -> aggregateLines handle []

{-|
 - aggregateLines builds the list of ip pairs, filtering only interesting entries
 - (selected via `filterCR`)
 -}
aggregateLines :: Handle -> [IPPair] -> IO [IPPair]
aggregateLines handle list = do isEof <- hIsEOF handle
                                case isEof of
                                    True  -> return list
                                    False -> do line <- hGetLine handle
                                                case filterCR line of
                                                    True  -> aggregateLines handle $ (ipPairFrom line):list
                                                    False -> aggregateLines handle list

filterCR :: String -> Bool
filterCR = isInfixOf "crunchy.rocks"

{-|
 - ipPairFrom takes a raw log line and returns a pair (date, ip)
 -}
ipPairFrom :: String -> IPPair
ipPairFrom line = let w = words line
                      ip = head w
                      date = tail $ w !! 3
                  in case parseDateTime "%d/%h/%Y:%H:%M:%S" date of
                      Just d  -> (d, ip)
                      Nothing -> error $ "Wrong date format: " ++ date
