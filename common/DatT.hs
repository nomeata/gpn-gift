module DatT where

import Time

type Option = [(String, String)] 

type Fahrplan = [Event]

type Event = (Integer, String, String, Time, RunTime)

eName    (_, s, _, _, _) = s
eRoom    (_, _, s, _, _) = s
eTime    (_, _, _, t, _) = t
eRunTime (_, _, _, _, t) = t
eEndTime event = eTime event `addRunTime` eRunTime event


type Passwd = [((String,String),[String])]

data ClientCommand =
	Commit Event   |
	Delete Integer | -- Event Id
	Edit   Event   |
	Quit
  deriving (Show, Read)
