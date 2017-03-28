module MaudStats.Types
( DateType
, IPPair
, LogIPPair
) where

import Data.Int (Int64)
import Data.DateTime (DateTime)

type DateType = Int64
type IPPair = (DateType, String)
type LogIPPair = (DateTime, String)
