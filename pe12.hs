fact :: Int -> Int -> [Int]
fact x y
    | x == y         = [x]
    | x `mod` y == 0 = y : fact (x `div` y) y
    | y == 2         = fact x 3
    | otherwise      = fact x (y+2)

--returns a list of factors of an integer
primeFactors :: Int -> [Int]
primeFactors x
    | x == 0    = []
    | x == 1    = []
    | x == 2    = [2]
    | otherwise = fact x 2

numberOfFactors :: Int -> Int
numberOfFactors = product . map ((+1) . length) . group . primeFactors


triangleNumbers :: [Int]
triangleNumbers = [sum [1..n] | n <- [1..]]

triangleFactors :: Int -> Maybe Int
triangleFactors num = find (\n -> numberOfFactors n > num) triangleNumbers