import System.Environment (getArgs)
import qualified FileUtils

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filePath] -> displayFileContent filePath
        _ -> writeHelp

    where 
        writeHelp = do
            putStrLn "Usage:\n\t*.exe <filePath>"
            putStrLn "Example:\n\tlistFile.exe input.txt"

        -- Функция для отображения содержимого файла с номерами строк
        displayFileContent filePath = do
            content <- FileUtils.readFileSync filePath
            let linesOfContent = lines content
            mapM_ (\(lineNumber, line) -> putStrLn ( show lineNumber ++ ": " ++ line) ) (zip [1..] linesOfContent)