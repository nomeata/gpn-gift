module MSignal (MSignal, newMSignal, sendMSignal, receiveMSignal) where 

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

newtype MSignal a = MS (MVar a)


newMSignal = MS `liftM` newEmptyMVar

sendMSignal (MS mv) v = do
	forkIO $ takeMVar mv >> return () -- Cleanup afterwards
	putMVar mv v

receiveMSignal (MS mv) = 
	readMVar mv
	
