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
import DatT
import Data.Maybe

------------------------------------------------------------------------------------
-- Main Loop
------------------------------------------------------------------------------------

main :: IO()
main = withSocketsDo $ do
	putStr "Starting up\n" 
	conf <- readFile "data/server.cnf" -- reading the server.cnf
	passFile <- readFile (fromJust $ lookup "passwd" (read conf :: Option))
	let port = fromJust $ lookup "port" (read conf :: Option)
	servSock <- listenOn $ PortNumber (fromIntegral (read (port) ::Int))
	acceptloop servSock passFile True
	putStr "See you in Space Cowboy...\n"
------------------------------------------------------------------------------------
-- Database Stuff
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Main Loop
------------------------------------------------------------------------------------
acceptloop :: Socket -> String -> Bool -> IO()
acceptloop socket  pwdFile False = sClose socket
acceptloop socket  pwdFile True = do
		       (cHandle, cName, cPort) <- accept socket
		       hSetBuffering cHandle LineBuffering
		       putStr ("Incoming request from: " ++ show cName ++ "\n")
		       putStr ("His Port is: " ++ show cPort  ++ "\n")
		       userName <- hGetLine cHandle
		       putStr ("Client auth as: " ++ userName ++ "\n")
		       let u = read pwdFile :: Passwd
		       passwd <- hGetLine cHandle
		       hClose cHandle
		       sClose socket
------------------------------------------------------------------------------------
-- Parser for the Password portection 
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Comunication Parsing
------------------------------------------------------------------------------------

