module FileRef (FileRef, newFileRef, readFileRef, writeFileRef) where

import System.FilePath
import Data.IORef

data (Show a, Read a) => FileRef a = FR FilePath (IORef a)

newFileRef :: (Show a, Read a) => FilePath -> IO (FileRef a)
newFileRef path = do
	absPath <- makeRelativeToCurrentDirectory path
	content <- readFile absPath
	ref <- newIORef (read content)
	return $ FR absPath ref

readFileRef :: (Show a, Read a) => FileRef a -> IO a
readFileRef (FR _ ref) = readIORef ref

writeFileRef :: (Show a, Read a) => FileRef a -> a -> IO ()
writeFileRef (FR path ref) v = do
	writeIORef ref v
	writeFile path (show v)
