module MaudStats.Display
( emitData
, generateData
, generateLabels
, printAll
, printNum
) where

import Data.DateTime
import Data.List       (intercalate)
import Data.UnixTime   (diffUnixTime, fromEpochTime, secondsToUnixDiffTime, udtSeconds)
import MaudStats.Manip (GroupedIPs, allDaysBetween, fillMissing)

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
readableDate = formatDateTime "%d/%h/%Y"

{-|
 - emitData takes 2 lists of ip pairs and structures them into JSON data.
 -}
emitData :: GroupedIPs -- The list of unique visiting ips (grouped by day)
         -> GroupedIPs -- The list of posting ips (grouped by day)
         -> String     -- The resulting JSON
emitData visiting posting =
        intercalate "\n"
        [ "'use strict'"
        , "var ext = {"
        , "    labels: ["
        , "        " ++ (intercalate "," $ generateLabels visiting)
        , "    ],"
        , "    visits: " ++ (show $ generateData visiting) ++ ","
        , "    posts: " ++ (show $ generateData posting)
        , "};"
        ]

-- Generate all days from first to last
generateLabels :: GroupedIPs -> [String]
generateLabels pairs = map (show . readableDate) $ allDaysBetween start end
                       where
                       start = fst $ head pairs
                       end   = fst $ last pairs

generateData :: GroupedIPs -> [Int]
generateData = map (length . snd) . fillMissing
