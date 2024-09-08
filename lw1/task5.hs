getQuadraticNumbers :: [Int]
getQuadraticNumbers = take 50 [x * (x + 1 ) * (2 * x + 1) `div` 6 | x <- [1..]]

main :: IO()
main = print( getQuadraticNumbers )