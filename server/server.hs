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
import Network
import Data.Maybe
import Control.Monad

import FileRef
import DatT

------------------------------------------------------------------------------------
-- Main Loop
------------------------------------------------------------------------------------

main :: IO()
main = withSocketsDo $ do
	putStr "Starting up\n" 
	conf <- read `liftM` readFile "data/server.cnf" -- reading the server.cnf
	passFile <- newFileRef (fromJust $ lookup "passwd" conf)
	let port = read (fromJust $ lookup "port" conf) :: Int
	servSock <- listenOn $ PortNumber (fromIntegral port)
	acceptloop servSock passFile True
	putStr "See you in Space Cowboy...\n"
------------------------------------------------------------------------------------
-- Database Stuff
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Main Loop
------------------------------------------------------------------------------------
acceptloop :: Socket -> FileRef Passwd -> Bool -> IO()
acceptloop socket pwdFile False = sClose socket
acceptloop socket pwdFile True = do
	(cHandle, cName, cPort) <- accept socket
	putStrLn ("Incoming request from: " ++ show cName)
     	putStrLn ("His Port is: " ++ show cPort)
	hSetBuffering cHandle LineBuffering
	login cHandle pwdFile
	hClose cHandle
	acceptloop socket pwdFile True

login h pwdFile = do
       	userName <- hGetLine h
       	putStrLn ("Client auth as: " ++ userName)
       	passwd <- hGetLine h
       	putStrLn ("Password Read")
	auth_res <- auth pwdFile userName passwd 
       	case auth_res of
		Nothing    -> return ()
	 	Just perms -> return () -- here interaction
			 	
------------------------------------------------------------------------------------
-- Password portection 
------------------------------------------------------------------------------------

auth pwdFile userName passwd = return Nothing

------------------------------------------------------------------------------------
-- Network Comunication Parsing
------------------------------------------------------------------------------------

