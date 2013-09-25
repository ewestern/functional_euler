
--is_palindrome :: String -> Bool
--is_palindrome (x:xs)
--    | xs == [] = True
--    | x == tail xs = True
--    | length xs == 1 && x == head xs = True
--    | otherwise = False



--main :: IO()
--main = do
--    triangle <- readFile "./pe_67.txt"
--    putStrLn $ show $ searchMemoized $ (listText triangle)

--genPalin :: Int -> String ->[String]
--genPalin top center = centers ++ map (genPalin top) new 
--    where new = [(show end) ++ center ++ (show end) | end <- [0..9] , center <- centers]

genPalin top acc = map palin acc
    where palin center = [(show end) ++ center ++ (show end) | end <- [1..9]]