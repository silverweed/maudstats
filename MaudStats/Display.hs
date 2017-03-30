module MaudStats.Display
( printAll
, printNum
) where

import Data.DateTime      (DateTime, formatDateTime, fromSeconds, toGregorian')
import Data.List          (intercalate)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.UnixTime      (diffUnixTime, fromEpochTime, secondsToUnixDiffTime, udtSeconds)
import MaudStats.Manip    (IPPair)

sep = "|"

{-|
 - printAll prints the results as (datetime)|[list of ips]
 -}
printAll :: (Show a) => [(DateTime, a)] -> IO ()
printAll [] = return ()
printAll ((date, p):pairs) = do 
                             putStr $ readableDate date
                             putStr sep
                             print p
                             printAll pairs

{-|
 - printNum prints the results as (datetime)|<number of unique ips>
 -}
printNum :: (Show a) => [(DateTime, [a])] -> IO ()
printNum [] = return ()
printNum ((date, p):pairs) = do
                             putStr $ readableDate date
                             putStr sep
                             print $ length p
                             printNum pairs

readableDate :: DateTime -> String
readableDate = formatDateTime "%d %h %Y"

{-|
 - emitData takes 2 lists of ip pairs and structures them into JSON data.
 -}
emitData :: [IPPair] -- The list of unique visiting ips
         -> [IPPair] -- The list of posting ips
         -> String   -- The resulting JSON
emitData visiting posting =
        intercalate "\n"
        [ "'use strict'"
        , "var ext = {"
        , "    var labels = ["
        , generateLabels visiting
        , "    ];"
        , "};"
        ]
        where
        -- Generate all days from first to last
        generateLabels :: [IPPair] -> String
        generateLabels pairs = intercalate "" $ map readableDate . toDateTime $ start : [addDays i start | i <- [1..diff]]
                               where
                               -- FIXME: do the correct datetime conversions
                               datetime2day :: DateTime -> Day
                               datetime2day = fromGregorian $ (!! 2) $ toGregorian'
                               start = datetime2day $ fst $ head pairs
                               diff  = diffDays (datetime2day $ fst $ last pairs) start
