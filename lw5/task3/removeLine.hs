import System.Environment (getArgs)
import qualified FileUtils

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filePath, lineNumberStr] -> removeLineFromFile filePath ( read lineNumberStr :: Int )
        _ -> writeHelp

    where
        writeHelp = do
            putStrLn "Usage:\n\t*.exe <filePath> <lineNumber>"
            putStrLn "Example:\n\tremoveLine.exe input.txt 10"

        removeLineFromFile filePath lineNumber
            | lineNumber < 1 = putStrLn "LineNumber is not an index. It must starts from 1"
            | otherwise = FileUtils.removeAt filePath lineNumber