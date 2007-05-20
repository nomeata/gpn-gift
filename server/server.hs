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
	g <- readFile "data/server.cnf" -- reading the server.cnf
	w <- readFile (fromJust $ lookup "passwd" (read g:: Option))
	let p = fromMaybe "autsch" $ lookup "port" (read g :: Option)
	servSock <- listenOn $ PortNumber (fromIntegral (read (p) ::Int))
	acceptloop servSock w True
	putStr "See you in Space Cowboy...\n"
------------------------------------------------------------------------------------
-- Database Stuff
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Main Loop
------------------------------------------------------------------------------------
acceptloop :: Socket -> String -> Bool -> IO()
acceptloop a  b False = sClose a
acceptloop a  b True = do
		       (cHandle, cName, cPort) <- accept a
		       hSetBuffering cHandle LineBuffering
		       putStr ("Incoming request from: " ++ show cName ++ "\n")
		       putStr ("His Port is: " ++ show cPort  ++ "\n")
		       u <- hGetLine cHandle
		       putStr ("Client auth as: " ++ u ++ "\n")
		       p <- hGetLine cHandle
		       hClose cHandle
		       sClose a
------------------------------------------------------------------------------------
-- Parser for the Password portection 
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- Network Comunication Parsing
------------------------------------------------------------------------------------

