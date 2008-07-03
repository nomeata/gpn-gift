
import Text.Printf
import Data.IORef
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import IO
import Network

import Time
import DatT

main = do
	input <- getContents
	let fahrplan = catMaybes $ zipWith parse_ng (lines input) [1..]

	conf <- read `liftM` readFile "data/ngupload.cnf"
	let port = read (fromMaybe "2342" $ lookup "port" conf) :: Int
	let host =       fromMaybe "localhost" $ lookup "host" conf
	h <- connectTo host (PortNumber (fromIntegral port))
	hSetBuffering h LineBuffering
	login h conf

	hPrint h (SetFahrplan fahrplan)
	reply <- hGetLine h
	hPrint h Quit
	putStrLn reply

parse_ng line i  | null text = Nothing
                 | otherwise = Just $ Event {
			eID = i,
			eName = text,
			eRoom = raum,
			eTime = start_time,
			eRunTime = run_time
		}
  where w = words line
  	tag_s = w !! 0
	time_s = w !! 1
	raum_s = w !! 2
	text  = unwords $ drop 3 w
	raum | raum_s == "ÜBERALL"  = Ueberall
	     | raum_s == "IRGENDWO" = Irgendwo
	     | raum_s == "NIRGENDS" = Nirgends
	     | otherwise           = error $ "Can not parse room " ++ show raum_s
	start_time = Time { tDay = read tag_s, tHour = read hour, tMin = read min}
	run_time   = RunTime {rtHour = 1, rtMin = 0}
	(hour,_:min) = span (/=':') time_s
	

login h conf = do
	hPutStrLn h (fromJust $ lookup "username" conf)
	hPutStrLn h (fromJust $ lookup "password" conf)

