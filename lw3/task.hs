import Data.List (nub)

listnums :: Int -> [Int]
listnums 0 = []
listnums n = n : listnums (n - 1)

secondlastlist :: [[a]] -> [a]
secondlastlist = map last

myunion :: Eq a => [a] -> [a] -> [a]
myunion xs ys = nub (xs ++ ys)

mysubst :: Eq a => [a] -> [a] -> [a]
mysubst xs ys = filter (`notElem` ys) xs

nposlist :: Int -> [[a]] -> [a]
nposlist n = map (!! n)

main :: IO ()
main = do
    print $ listnums 5 -- [5, 4, 3, 2, 1]
    print $ listnums 0 -- []
    print $ listnums 1 -- [1]

    print $ secondlastlist [[1,2,3], [4,5], [6]] -- [3, 5, 6]
    print $ secondlastlist [[1], [2], [3]] -- [1, 2, 3]

    print $ myunion [1, 2, 3] [3, 4, 5] -- [1, 2, 3, 4, 5]
    print $ myunion [1, 2, 3, 3, 100000] [2, 3, 4] -- [1, 2, 3, 101, 4]
    print $ myunion [] [1, 2, 3] -- [1, 2, 3]

    print $ mysubst [1, 2, 3] [3, 4, 5] -- [1, 2]
    print $ mysubst [1, 2, 3] [1, 2, 3] -- []
    print $ mysubst [1, 2, 3] [] -- [1, 2, 3]

    print $ nposlist 1 [[1,2,3], [4,5,6], [7,8,9]] -- [2, 5, 8]
    print $ nposlist 0 [[1,2], [3,4], [5,6]] -- [1, 3, 5]
    print $ nposlist 2 [[1,2,3], [4,5,6]] -- [3, 6]
    