module MaudStats.Display
( printAll
, printNum
) where

import Data.DateTime (DateTime, fromSeconds, toGregorian')
import Data.UnixTime (diffUnixTime, secondsToUnixDiffTime, fromEpochTime, udtSeconds)

{-|
 - printAll prints the results as (datetime) [list of ips]
 -}
printAll :: (Show a) => [(DateTime, a)] -> IO ()
printAll [] = return ()
printAll ((date, p):pairs) = do 
                             putStr $ show $ toGregorian' date
                             putStr " "
                             print p
                             printAll pairs

{-|
 - printNum prints the results as (datetime) <number of unique ips>
 -}
printNum :: (Show a) => [(DateTime, [a])] -> IO ()
printNum [] = return ()
printNum ((date, p):pairs) = do
                             putStr $ show $ toGregorian' date
                             putStr " "
                             print $ length p
                             printNum pairs
