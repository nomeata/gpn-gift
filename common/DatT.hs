module DatT where

import Time

type Option = [(String, String)] 

type Fahrplan = [Event]

type Event = (Integer, String, String, Time, RunTime)

type Passwd = [((String,String),[String])]

data ClientCommand =
	Commit Event   |
	Delete Integer | -- Event Id
	Edit   Event   |
	Quit
  deriving (Show, Read)
