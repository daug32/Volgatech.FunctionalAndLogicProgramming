import System.Environment (getArgs)
import qualified FileUtils

main :: IO ()
main = do
    args <- getArgs

    if length args < 3
        then writeHelp
        else do
            let filePath = head args
            let lineNumber = read ( args !! 1 ) :: Int
            let newLine = concat ( take ( length args - 2 ) ( drop 2 args ) )
            insertAtFile filePath lineNumber newLine

    where 
        writeHelp = do
            putStrLn "Usage:\n\t*.exe <filePath> <lineNumber> <newLine>"
            putStrLn "Example:\n\tinsertLine.exe file.txt 10 \"Hello, world!\""

        insertAtFile filePath lineNumber newLine
            | lineNumber < 1 = putStrLn "LineNumber is not an index. It must starts from 1"
            | otherwise = FileUtils.insert filePath newLine lineNumber