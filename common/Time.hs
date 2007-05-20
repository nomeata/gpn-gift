module Time where

import System.Time
import Text.Printf

data Time = Time { tDay :: Int, tHour :: Int, tMin :: Int } deriving (Eq, Ord)
data RunTime = RunTime { rtHour :: Int, rtMin :: Int } deriving (Eq, Ord)

instance Show Time where
	show time = show (tDay time, tHour time, tMin time)
instance Read Time where
	readsPrec d str = [(Time day hour min,out)]
	  where ((day,hour,min),out):_ = readsPrec d str 

instance Show RunTime where
	show time = show (rtHour time, rtMin time)
instance Read RunTime where
	readsPrec d str = [(RunTime hour min,out)]
	  where ((hour,min),out):_ = readsPrec d str 

printTime  time = printf "Tag %d − %02.0d:%02.0d" (tDay time) (tHour time) (tMin time)
printRunTime time = printf "%02.0dh%02.0d" (rtHour time) (rtMin time)

now = do
	time <- getClockTime >>= toCalendarTime
	return $ Time { tDay = ctDay time - 20, tHour = ctHour time, tMin = ctMin time}

addRunTime start rt= fix $ sum 
  where sum = start {tHour = tHour start + rtHour rt, tMin = tMin start + rtMin rt}
  	fix = fixd . fixh
	fixh time = let (hd, m) = tMin time `divMod` 60 in
			time {tHour = tHour time + hd, tMin = m}
	fixd time = let (dd, h) = tHour time `divMod` 24 in
			time {tDay = tDay time + dd, tHour = h}
