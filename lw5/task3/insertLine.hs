import System.Environment (getArgs)
import qualified FileUtils

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filePath, lineNumber, newLine] -> insertAtFile filePath ( read lineNumber :: Int ) newLine
        _ -> writeHelp

    where 
        writeHelp = do
            putStrLn "Usage:\n\t*.exe <filePath> <lineNumber> <newLine>"
            putStrLn "Example:\n\tinsertLine.exe file.txt 10 \"Hello, world!\""

        insertAtFile filePath lineNumber newLine
            | lineNumber < 1 = putStrLn "LineNumber is not an index. It must starts from 1"
            | otherwise = FileUtils.insert filePath newLine lineNumber