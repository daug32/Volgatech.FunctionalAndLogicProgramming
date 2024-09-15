module DeclarationFunctions where

-- 1. oddEven(L)
oddEven :: [a] -> [a]
    -- Что делает: Функция перестановки местами соседних элементов списка
    -- Параметр L: Список элементов
    -- Возвращает: Список элементов, в котором соседние элементы переставлены местами
    
-- Пустой список и список из одного элемента не изменяются
oddEven (a:b:list) = b:a:oddEven list
oddEven x = x

-- Например:
runOddEvenExamples :: IO()
runOddEvenExamples = do
    print ( oddEven ( [] :: [Int] ) ) -- вернет [] 
    print ( oddEven [1] ) -- вернет [1] 
    print ( oddEven [1,2,3] ) -- вернет [2,1,3] 
    print ( oddEven [2,5,7,9,1,8] ) -- вернет [5,2,9,7,8,1] 

-- 2. insert(L, A, n)
insert :: [a] -> a -> Int -> [a]
    -- Что делает: Вставляет элемент A в список L на позицию n
    -- Параметр L: Список элементов, в который нужно вставить новый элемент
    -- Параметр A: Элемент, который необходимо вставить в список
    -- Параметр n: Индекс, на который необходимо вставить элемент
    -- Возвращает: Новый список, в котором элемент стоит на позиции n 

insert list item index =
    take index list 
    ++ [item] 
    ++ drop index list
    
-- Например:
runInsertExamples :: IO()
runInsertExamples = do
    print ( insert [] 10 0 ) -- вернет [10]
    print ( insert [1] 10 0 ) -- вернет [10,1]
    print ( insert [1] 10 1 ) -- вернет [1,10]
    print ( insert [1] 10 100 ) -- вернет [1,10]
        
-- 3. listSumm(L1, L2)
listSumm :: [Int] -> [Int] -> [Int]
    -- Что делает: Складывает элементы двух списоков с одинакоым индексом
    -- Принимает: Два списка, которые нужно сложить поэлементно
listSumm (x:xs) (y:ys) = (x + y) : listSumm xs ys
listSumm [] x = x
listSumm x [] = x

-- Например: 
runListSummExamples :: IO()
runListSummExamples = do
    print ( listSumm [1,2,3] [10,20,30] ) -- вернет [11,22,33]
    print ( listSumm [1,2,3] [10] ) -- вернет [11,2,3]
    print ( listSumm [] [10] ) -- вернет [10]

-- 4. position(L, A)
position :: Eq a => [a] -> a -> Int
    -- Что делает: Ищет индекс первого найденного элемента в списке L, эквивалентного параметру A, или -1, если элемент не найден
    -- Параметр L: Список L, в котором нужно найти элемент
    -- Параметр A: Объект, который нужно найти в списке
    -- Возвращает: Индекс первого найденного элемента или -1, если таких нет

position list item = findPosition list item 0
  where
    findPosition [] _ _ = -1
    findPosition (x:xs) a index
      | x == a    = index
      | otherwise = findPosition xs a (index + 1)

-- Например:
runPositionExamples :: IO()
runPositionExamples = do
    print ( position [] 1 ) -- вернет -1
    print ( position [1,2,3] 1 ) -- вернет 0
    print ( position [1,2,3] 3) -- вернет 2

-- 5. sum1(n)
sum1 :: Int -> Int
    -- Что делает: Вычисляет сумму целлых чисел в диапазоне [1; n]
    -- Параметр n: Максимальное число, которое должно быть включено в сумму

sum1 n = sum [1..n]

-- Например:
runSum1Examples :: IO()
runSum1Examples = do
    print( sum1 0 ) -- вернет 0
    print( sum1 1 ) -- вернет 1
    print( sum1 5 ) -- вернет 15 (1 + 2 + 3 + 4 + 5)

-- 6. sum2(n)
sum2 :: Int -> Int
    -- Что делает: Вычисляет сумму разности n и каждого целого числа в диапазоне [1; n]
    -- Параметр n: Максимальное число, для которого нужно произвести сумму

sum2 n = sum [n - i | i <- [1..n]]

-- Например: 
runSum2Examples :: IO()
runSum2Examples = do
    print( sum2 0 ) -- вернет 0
    print( sum2 1 ) -- вернет 0
    print( sum2 5 ) -- вернет 10 ( 5-1 + 5-2 + 5-3 + 5-4 + 5-5 = 10 )

main :: IO() 
main = do 
    runOddEvenExamples
    runInsertExamples
    runListSummExamples
    runPositionExamples
    runSum1Examples
    runSum2Examples