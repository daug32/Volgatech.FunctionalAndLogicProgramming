-- Скопировать информацию из одного файла в другой, 
-- заменив знаки пунктуации заданным с клавиатуры символом.
-- Имена файлов указываются в командной строке.

import System.Environment (getArgs)
import System.IO (hGetContents, hPutStr, withFile, IOMode(..))
import Data.Char (isPunctuation)

-- Функция для замены знаков препинания на указанный символ
replacePunctuation :: Char -> String -> String
replacePunctuation replacementChar = map (\c -> if isPunctuation c then replacementChar else c)

-- Основная функция для копирования содержимого файла с заменой знаков препинания
main :: IO ()
main = do
    -- Чтение аргументов командной строки
    args <- getArgs
    case args of
        [sourceFile, destFile] -> do
            -- Чтение символа для замены знаков препинания с клавиатуры
            putStrLn "Введите символ для замены знаков препинания:"
            replacementChar <- getLine
            let replacement = head replacementChar

            -- Чтение содержимого исходного файла
            content <- readFile sourceFile

            -- Замена знаков препинания на указанный символ
            let modifiedContent = replacePunctuation replacement content

            -- Запись измененного содержимого в целевой файл
            writeFile destFile modifiedContent
            putStrLn "Файл скопирован и знаки препинания заменены успешно."
        _ -> putStrLn "Использование: *.exe <исходныйФайл> <целевойФайл>"