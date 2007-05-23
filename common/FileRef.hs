module FileRef (FileRef, newFileRef, readFileRef, writeFileRef) where

data FileRef a = FileRef

newFileRef :: FilePath -> IO (FileRef a)
newFileRef = undefined

readFileRef :: FileRef a -> IO a
readFileRef = undefined

writeFileRef :: FileRef a -> a -> IO ()
writeFileRef = undefined
