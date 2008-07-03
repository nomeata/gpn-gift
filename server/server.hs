{-# OPTIONS_GHC -fimplicit-params #-}
------------------------------------------------------------------------------------
--                server.hs - The GPN interaktiv Fahrplan Server                  --
------------------------------------------------------------------------------------
--   Copyright (C) 2007 by Frederick Bullik                                       --
--   Frederick.Bullik@gmx.de                                                      --
--                                                                                --
--   This program is free software; you can redistribute it and/or modify         --
--   it under the terms of the GNU General Public License as published by         --
--   the Free Software Foundation; either version 2 of the License, or            --
--   (at your option) any later version.                                          --
--                                                                                --
--   This program is distributed in the hope that it will be useful,              --
--   but WITHOUT ANY WARRANTY; without even the implied warranty of               --
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                --
--   GNU General Public License for more details.                                 --
--                                                                                --
--   You should have received a copy of the GNU General Public License            --
--   along with this program; if not, write to the                                --
--   Free Software Foundation, Inc.,                                              --
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                    --
--                                                                                --
------------------------------------------------------------------------------------

import System.IO
import System.Posix.Signals
import Network
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch) -- We want catch from C.E

import FileRef
import MSignal

import DatT

------------------------------------------------------------------------------------
-- Main Loop
------------------------------------------------------------------------------------

main :: IO()
main = withSocketsDo $ do
	installHandler sigPIPE Ignore Nothing
	putStr "Starting up\n" 
	conf <- read `liftM` readFile "data/server.cnf" -- reading the server.cnf
	pwdFile <- newFileRef (fromJust $ lookup "passwd" conf) -- Implicit Parameter
	let ?pwdFile = pwdFile :: FileRef Passwd
	dataFile <- newFileRef (fromJust $ lookup "data" conf )
	let ?dataFile = dataFile :: FileRef Fahrplan
	readFileRef ?dataFile >>= \d -> putStrLn $ "Read " ++ (show (length d)) ++ " events."
	changeS <- newMSignal
	let ?changeS = changeS
	let port = read (fromJust $ lookup "port" conf) :: Int
	servSock <- listenOn $ PortNumber (fromIntegral port)
	acceptloop servSock True
	putStr "See you in Space Cowboy...\n"
------------------------------------------------------------------------------------
-- Database Stuff
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Main Loop
------------------------------------------------------------------------------------
acceptloop socket False = sClose socket
acceptloop socket True = do
	(cHandle, cName, cPort) <- accept socket
	forkIO $ (do
		putStrLn $ "Incoming request from: " ++ show cName ++ ":" ++ show cPort
		hSetBuffering cHandle LineBuffering
		login cHandle
		) `finally` (do
		hClose cHandle `catch` const(return ())
		putStrLn $ "Done with: " ++ show cName ++ ":" ++ show cPort
		)
	acceptloop socket True

login h = do
       	userName <- hGetLine h
       	putStrLn ("Client auth as: " ++ userName)
       	passwd <- hGetLine h
       	putStrLn ("Password Read")
	auth_res <- auth  userName passwd 
       	case auth_res of
		Nothing    -> putStrLn "Login failed..."
	 	Just perms -> catch (talk h perms) $ \e -> do
				 hPutStrLn h   "Some Error Happened, Good bye" `catch` const(return ())
				 putStrLn    $ "Error while talking to client: " ++ show e
			 	
------------------------------------------------------------------------------------
-- Password portection 
------------------------------------------------------------------------------------

auth userName passwd = do
	pwdData <- readFileRef ?pwdFile
	return $ lookup (userName, passwd) pwdData

------------------------------------------------------------------------------------
-- Network Comunication Parsing
------------------------------------------------------------------------------------

talk h perm = do
		command <- read `liftM` hGetLine h
		putStrLn $ "Got command " ++ show command
		reply command
  where wrap req act = do
		if req `elem` perm then act
		                   else hPutStrLn h "Sorry, you don't have what it takes!"
		talk h perm
  	reply Quit = do
  		hPutStrLn h "Goodbye..."
        reply (SetFahrplan f) = wrap CanSet $ do
		result <- replaceFahrplan f
		when (isNothing result) $ sendMSignal ?changeS ()
		case result of
			Nothing  -> hPutStrLn h "Sucessfully replaced fahrplan"
			Just why -> hPutStrLn h ("Could not add event: " ++ why)
        reply (Commit e) = wrap CanCommit $ do
		result <- addToFahrplan e
		when (isNothing result) $ sendMSignal ?changeS ()
		case result of
			Nothing  -> hPutStrLn h "Sucessfully added event to fahrplan"
			Just why -> hPutStrLn h ("Could not add event: " ++ why)
        reply (Edit e) = wrap CanEdit $ do
		result <- modifyFahrplan e
		when (isNothing result) $ sendMSignal ?changeS ()
		case result of
			Nothing  -> hPutStrLn h "Sucessfully edited event"
			Just why -> hPutStrLn h ("Could not edit event: " ++ why)
        reply (Delete id) = wrap CanDelete $ do
		result <- removeFromFahrplan id
		when (isNothing result) $ sendMSignal ?changeS ()
		case result of
			Nothing  -> hPutStrLn h "Sucessfully removed event"
			Just why -> hPutStrLn h ("Could not remove event: " ++ why)
        reply ShowFahrplan = wrap CanRead $ do
		fahrplan <- readFileRef ?dataFile
		hPrint h fahrplan
        reply Listen = wrap CanRead $ do
		receiveMSignal ?changeS
		fahrplan <- readFileRef ?dataFile
		hPrint h fahrplan
	
------------------------------------------------------------------------------------
-- Fahrplan DB Handling
------------------------------------------------------------------------------------

inRoom room fahrplan = filter (\e -> eRoom e == room) fahrplan

sameRoom event = inRoom (eRoom event)

sameTime e1 e2 = not ((eTime e1 < eTime e2 && eEndTime e1 <= eTime e2) ||
                      (eTime e2 < eTime e1 && eEndTime e2 <= eTime e1))

findConflict event fahrplan = find (sameTime event) relevants
  where relevants = sameRoom event fahrplan

validChar = not . isControl -- Hauptsache keine NewLines. Sonst noch Wünsche?

errorIf msg test = if test then Just msg else Nothing

replaceFahrplan fp = do
	fahrplan <- readFileRef ?dataFile
	let result = join $ find (isJust) [ -- Things to Check
		--"Ungültiger Eventname" `errorIf` not (all validChar (eName event)),
		--"Leerer Name" `errorIf` null (eName event),
		--(\e -> "Konflikt mit " ++ eName e) `fmap` findConflict event fahrplan
		]
	when (isNothing result) $ do
		writeFileRef ?dataFile fp	
	return result

addToFahrplan event = do
	fahrplan <- readFileRef ?dataFile
	let result = join $ find (isJust) [ -- Things to Check
		"Ungültiger Eventname" `errorIf` not (all validChar (eName event)),
		"Leerer Name" `errorIf` null (eName event),
		(\e -> "Konflikt mit " ++ eName e) `fmap` findConflict event fahrplan
		]
	when (isNothing result) $ do
		let high_id = maximum $ 0 : map (eID) fahrplan
		let event' = event { eID = succ high_id}
		writeFileRef ?dataFile (event':fahrplan)	
	return result

modifyFahrplan event = do 
	fahrplan <- readFileRef ?dataFile
	let rest_fahrplan = filter (\e -> eID e /= eID event) fahrplan
	let orig_event = find (\e -> eID e == eID event) fahrplan
	let result = join $ find (isJust) [ -- Things to Check
		"Zu bearbeitendes Event nicht verfügbar" `errorIf` isNothing orig_event,
		"Ungültiger Eventname" `errorIf` not (all validChar (eName event)),
		"Leerer Name" `errorIf` null (eName event),
		(\e -> "Konflikt mit " ++ eName e) `fmap` findConflict event rest_fahrplan
		]
	when (isNothing result) $ do
		writeFileRef ?dataFile (event:rest_fahrplan)	
	return result
		
removeFromFahrplan id = do 
	fahrplan <- readFileRef ?dataFile
	let rest_fahrplan = filter (\e -> eID e /= id) fahrplan
	let orig_event = find (\e -> eID e == id) fahrplan
	let result = join $ find (isJust) [ -- Things to Check
		"Zu löschendes Event nicht verfügbar" `errorIf` isNothing orig_event
		]
	when (isNothing result) $ do
		writeFileRef ?dataFile rest_fahrplan
	return result
		

