module CfgPars (parser) where
--------------------------------------------
--Parser
--------------------------------------------
-- A Parser for the config files
-- ----------------------------------------

-- Beautify the config
-- by removing \n and spaces
normal :: String -> String
normal [] = []
normal (x:xs) | (x == ' ')   =  normal xs
              | (x == '\n' ) =  normal xs
              | otherwise = x : normal xs

--Convert the string into a list of Tuples
--in the form (key,value)
--this is much easayer to pars
mklist :: String -> [(String,String)]
mklist [] = []
mklist (x:xs) | (x == '(') = (key xs ,value 0 xs) : mklist xs
             | otherwise = mklist xs

-- the value of the tuple witch represents the key the config file tuple
key :: String -> String
key [] = []
key (x:xs) | (x == ',') = key []
       	   | otherwise = x : key xs

-- the value wutch represents the value in the config file tuple
value :: Integer -> String -> String
value 0 [] = []
value a (x:xs) | (x == ',') = value 1 xs
               | (x == ')') = value 0 []
	       | (a == 1 ) = x : value 1 xs
	       | otherwise = value 0 xs

-- Matches a String against a Key value of the config tuple
match :: String -> [(String,String)] -> String
match a [] = []
match a (x:xs)  = findKey a x ++ match a xs

--find a value insde the tuple
findKey :: String -> (String,String) -> String
findKey a (c,d) | (a == c) = d
		| otherwise = ""

--Glues the parser togeter
parser :: String -> String -> String
parser a b = match a ( mklist ( (normal b) ) )

