getTriangularNumbers :: [Int]
getTriangularNumbers = take 50 [toTriangularNumber x | x <- [1..]]
    where 
        toTriangularNumber x = x * (x + 1) `div` 2

main :: IO()
main = print( getTriangularNumbers )