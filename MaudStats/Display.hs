module MaudStats.Display
( printAll
, printNum
) where

import Data.DateTime (DateTime, fromSeconds, toGregorian', formatDateTime)
import Data.UnixTime (diffUnixTime, secondsToUnixDiffTime, fromEpochTime, udtSeconds)

{-|
 - printAll prints the results as (datetime)|[list of ips]
 -}
printAll :: (Show a) => [(DateTime, a)] -> IO ()
printAll [] = return ()
printAll ((date, p):pairs) = do 
                             putStr $ readableDate date
                             putStr "|"
                             print p
                             printAll pairs

{-|
 - printNum prints the results as (datetime)|<number of unique ips>
 -}
printNum :: (Show a) => [(DateTime, [a])] -> IO ()
printNum [] = return ()
printNum ((date, p):pairs) = do
                             putStr $ readableDate date
                             putStr "|"
                             print $ length p
                             printNum pairs

readableDate :: DateTime -> String
readableDate = formatDateTime "%d %h %Y"
