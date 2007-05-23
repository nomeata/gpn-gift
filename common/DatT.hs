module DatT where

import Time

type Option = [(String, String)] 

type Fahrplan = [Event]

data Room = Chaos | HackCenter | Balcony deriving (Eq, Show, Read)

type Event = (Integer, String, Room, Time, RunTime)

eID      (n, s, _, _, _) = n
eName    (_, s, _, _, _) = s
eRoom    (_, _, r, _, _) = r
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
