-- | The FileRef object is similar to a 'IORef' with the difference that it reads it's initial data from a given file and writes, upon modification, the new data to that file.
module FileRef (FileRef, newFileRef, readFileRef, writeFileRef) where

import System.FilePath
import System.Directory
import Data.IORef
import qualified Data.ByteString.Char8 as BS -- Faster and Strict


-- | The FileRef data type is opaque and can store only data that can be written and read using 'Show' and 'Read'.
data (Show a, Read a) => FileRef a = FR FilePath (IORef a)

-- | Creates a new 'FileRef' and reads the data from the given file
newFileRef :: (Show a, Read a) => FilePath -> IO (FileRef a)
newFileRef path = do
	absPath <- makeRelativeToCurrentDirectory path
	content <- BS.readFile absPath
	ref <- newIORef (read (BS.unpack content))
	return $ FR absPath ref

-- | Returns the data stored in the 'FileRef'
readFileRef :: (Show a, Read a) => FileRef a -> IO a
readFileRef (FR _ ref) = readIORef ref

-- | Stores new data in the 'FileRef', updating the file.
writeFileRef :: (Show a, Read a) => FileRef a -> a -> IO ()
writeFileRef (FR path ref) v = do
	writeIORef ref v
	BS.writeFile path (BS.pack (show v))
