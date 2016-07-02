module MaudStats.Types
( DateType
, IPPair
) where

import Data.Int (Int64)

type DateType = Int64 
type IPPair = (DateType, String)
