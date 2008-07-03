module DatT where

import Time

-- | General Option List
type Option = [(String, String)] 

-- | A list of events
type Fahrplan = [Event]

-- | An enumeration of rooms at the GPN6
--data Room = KlammerAuf | KlammerZu | Balcony | Otherwhere deriving (Eq, Show, Read, Ord)
-- | An enumeration of rooms at the GPN7
data Room = Ueberall | Nirgends | Irgendwo deriving (Eq, Show, Read, Ord)

-- | An event, containing fields for: ID, Name, Room, Start Time, End Time. To be accessed using the following accessors.
data Event = Event {
	eID	:: Integer,
	eName	:: String,
	eRoom   :: Room,
	eTime	:: Time,
	eRunTime:: RunTime
	} deriving (Eq, Ord, Show, Read)

eEndTime:: Event -> Time
eEndTime event = eTime event `addRunTime` eRunTime event

-- | User database (Username\/Password pairs and list of permissions)
data Permission = CanRead | CanCommit | CanEdit | CanDelete | CanSet deriving (Eq, Read, Show)
type Passwd = [((String,String),[Permission])]

-- | Possible commands to be received by the client
data ClientCommand =
	Commit Event  		| -- ^ Adding a new event (Event Id ignored)
	Delete Integer 		| -- ^ Deleting the event with the given id
	Edit   Event   		| -- ^ Replace the event with the same id
	ShowFahrplan   		| -- ^ Print the current Fahrplan
	SetFahrplan Fahrplan 	| -- ^ Replace the complete Fahrplan
	Listen       		| -- ^ Wait until the Fahrplan changes, and then print it /once/
	Quit            	  -- ^ Close the connection
  deriving (Show, Read)
