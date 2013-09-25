
import Data.Map


buildList :: [Int] -> [Int]
buildList xs
    | num == 1          = xs
    | num `mod` 2 == 0  = buildList (xs ++ [num `div` 2])
    | num `mod` 2 == 1  = buildList (xs ++ [(3 * num) + 1])
    where num = last xs

mapFunc x
    |

collatz :: Int -> (Int, Int)
collatz num = findMax $ fromList [ (length $ buildList [n], n) | n <- [1..num]]