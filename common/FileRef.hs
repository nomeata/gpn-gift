module FileRef (FileRef, newFileRef, readFileRef, writeFileRef) where

import System.FilePath
import Data.IORef
import qualified Data.ByteString.Char8 as BS -- Faster and Strict

data (Show a, Read a) => FileRef a = FR FilePath (IORef a)

newFileRef :: (Show a, Read a) => FilePath -> IO (FileRef a)
newFileRef path = do
	absPath <- makeRelativeToCurrentDirectory path
	content <- BS.readFile absPath
	ref <- newIORef (read (BS.unpack content))
	return $ FR absPath ref

readFileRef :: (Show a, Read a) => FileRef a -> IO a
readFileRef (FR _ ref) = readIORef ref

writeFileRef :: (Show a, Read a) => FileRef a -> a -> IO ()
writeFileRef (FR path ref) v = do
	writeIORef ref v
	BS.writeFile path (BS.pack (show v))
