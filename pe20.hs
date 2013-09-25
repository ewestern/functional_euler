import Data.Char

mem_fact :: Int -> Integer
mem_fact = (map fact [0..] !!)
    where   fact 1 = 1
            fact n = n * mem_fact ((fromIntegral n)-1)

sum_fact :: Int -> Int
sum_fact num = foldl (\acc c -> acc + (digitToInt c)) 0 (show . mem_fact $ num)