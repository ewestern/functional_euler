nums :: [Int]
nums = [999,998..1]

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if x == (last xs)
                        then isPalindrome $ init xs
                        else False 

descendingProducts :: [Int]
descendingProducts = 
-- iterators through
findProduct :: (Int, (Int, Int))
findProduct = findP nums nums

  where
    findP a@(x:xs) b@(y:ys)
      | isPalindrome $ show (x * y) = (x * y, (x, y))
      | otherwise = if x >= y
                      then findP xs b
                      else findP ys a

