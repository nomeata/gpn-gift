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
import CfgPars 
import Network

------------------------------------------------------------------------------------
-- Main Loop
------------------------------------------------------------------------------------

main :: IO()
main = withSocketsDo $ do
	putStr "Starting up\n" 
	g <- readFile "data/server.cnf" -- reading the server.cnf
	p <- readFile (parser "passwd" g) -- reading the passwd.cnf file
	putStr "Initialiesing Fahrplay DB\n"
	let db = initfdb []
	-- Down there we starting up a server
	servSock <- listenOn $ PortNumber (fromIntegral (read (parser "port" g) :: Int))
	putStr ("listen on Port: " ++ (parser "port" g) ++ "\n")
	acceptloop servSock p True

------------------------------------------------------------------------------------
-- Database Stuff
------------------------------------------------------------------------------------
initfdb :: String -> [(Integer,String,String,Int,(Int,Int),(Int,Int))]
initfdb [] = []
initfdb a = [] -- To be written
------------------------------------------------------------------------------------
-- Network Main Loop
------------------------------------------------------------------------------------
acceptloop :: Socket -> String -> Bool -> IO()
acceptloop a b False = sClose a
acceptloop a b True = do
		       (cHandle, cName, cPort) <- accept a
		       hSetBuffering cHandle LineBuffering
		       putStr ("Incoming request from: " ++ show cName ++ "\n")
		       putStr ("His Port is: " ++ show cPort  ++ "\n")
		       u <- hGetLine cHandle
		       putStr ("Client auth as: " ++ u ++ "\n")
		       let c = (login u (users b))
		       netan c cHandle 
		       pexit a b c
		       p <- hGetLine cHandle
		       putStr ("User " ++ u ++ " has sent Passwd\n" )
		       let c = chkpass p (prspwd u b)
		       netan c cHandle
		       pexit a b c
		       hClose cHandle
		       sClose a
------------------------------------------------------------------------------------
-- Parser for the Password portection 
------------------------------------------------------------------------------------

-- get a list of Usernames, String is the passwd file
users :: String -> [String]
users [] = []
users (x:xs) | (x == '(') = key xs : users xs
	     | otherwise = users xs

-- get the password to a username, String 1 is usernam, 2 the paswd file 
prspwd :: String -> String -> String
prspwd [] b = value 0 b  
prspwd a [] = []
prspwd (x:xs) (y:ys) | (x == y) = prspwd xs ys
                     | otherwise = prspwd (x:xs) ys

-- prspriv get the priveleges of an user, paremeters are the same as by prspasswd
prspriv :: String -> String -> [String]
prspriv [] (x:xs) = privi xs
prspriv a [] = []
prspriv (x:xs) (y:ys) | (x == y) = prspriv xs ys
                      | otherwise = prspriv (x:xs) ys

privi :: String -> [String]
privi [] = []
privi (x:xs) | (x == '[') = privii xs : privi xs 
             | (x == ',') = privii xs : privi xs
	     | (x == ']') = privi []
	     | otherwise  = privi xs

privii :: String -> String
privii [] = []
privii (x:xs) | (x == ',') = privii []
	      | (x == ']') = privii []
	      | otherwise = x : privii xs

------------------------------------------------------------------------------------
-- Network Comunication Parsing
------------------------------------------------------------------------------------
compse :: Int -> Bool
compse a | (a == 1) = True
         | otherwise = False

secomp :: String -> [String] -> Int
secomp a b = length ( filter (== a)  b)

--checks is user valid?
login :: String -> [String] -> Bool
login a b = compse (secomp a b)

chkpass :: String -> String -> Bool
chkpass a b | (a == b) = True
	    | otherwise = False

netan :: Bool -> Handle -> IO()
netan a b | (a == True) = hPutStr b "Yups\n"
	  | otherwise = hPutStr b "Error\n"
	                
pexit :: Socket -> String -> Bool -> IO()
pexit a b c | (c == False) = acceptloop a b False 
	    | otherwise = putStr "User or Pass OK\n"

