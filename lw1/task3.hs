-- Просто и красиво через генерацию списка
inifiniteListWay :: [Int]
inifiniteListWay = take 20 [ 1, 3.. ]

-- Через map
filterWithMap :: [Int]
filterWithMap = take 20 ( map ( \x -> 2 * x + 1 ) [0..] )

-- Через списочное выражение
filterWithoutMap :: [Int]
filterWithoutMap = take 20 [x | x <- [0..], x `mod` 2 == 1 ]

main :: IO()
main = do
    print( inifiniteListWay )
    print( filterWithMap )
    print( filterWithoutMap )
