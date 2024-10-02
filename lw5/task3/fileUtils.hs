module FileUtils (
    readFileSync,
    writeFileSync,
    insert,
    removeAt
) where

import System.Directory (doesFileExist)
import System.IO (openFile, IOMode (ReadMode, WriteMode), hGetContents', hClose, hPutStr)

-- Reads whole file content. If file doesn't exist, returns an empty string
readFileSync :: FilePath -> IO String 
readFileSync filePath = do
    fileExists <- doesFileExist filePath
    if not fileExists
        then return ""
        else readSyncFromExistingFile

    where
        readSyncFromExistingFile = do
            -- File handlers are used to prevent uncontrollable async IO operations
            -- Open a file handler that can only read
            openFileHandler <- openFile filePath ReadMode
            -- Somehow read file content
            content <- hGetContents' openFileHandler
            -- Force handler to close
            hClose openFileHandler
            -- Return read content
            return content

-- Rewrites file content with given string. If file doesn't exist, creates a new file with that content
writeFileSync :: FilePath -> String -> IO()
writeFileSync filePath newContent = do
    -- File handlers are used to prevent uncontrollable async IO operations
    -- Open a file handler that can only write
    writeFileHandler <- openFile filePath WriteMode
    -- Write new file content
    hPutStr writeFileHandler newContent
    -- Force handler to close
    hClose writeFileHandler
    return ()

-- Insert a line into a file at the given line number (not index)
insert :: FilePath -> String -> Int -> IO()
insert filePath newString lineNumber = do
    fileContent <- readFileSync filePath 

    let linesOfContent = lines fileContent
    let (before, after) = splitAt (lineNumber - 1) linesOfContent
    let newFileContent = insertAt (lineNumber - 1) newString linesOfContent

    writeFileSync filePath ( unlines newFileContent )
    return ()

    where 
        insertAt index str list
            | index < 0 = error "Index cannot be negative"
            | otherwise = take index (list ++ repeat "") ++ [str] ++ drop index list

-- Removing a line at a given number (not index) from file
removeAt :: FilePath -> Int -> IO()
removeAt filePath lineNumber = do
    fileContent <- readFileSync filePath

    let linesOfContent = lines fileContent
    let newFileContent = unlines ( removeLineAtIndex (lineNumber - 1) linesOfContent )

    writeFileSync filePath newFileContent

    where 
        -- Remove a line at a specific index from a list of strings
        removeLineAtIndex index list
            | index < 0 || index >= length list = list
            | otherwise = take index list ++ drop (index + 1) list