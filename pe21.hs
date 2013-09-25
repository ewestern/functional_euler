divisors :: Int -> [Int]
divisors n = [d | d <- [1..(n `div` 2)], n `mod` d == 0]


sum_div :: Int -> Int
sum_div n = sum $ divisors n

--is_ami :: (Num a) => a -> a -> Bool
is_ami :: Int -> Int -> Bool
is_ami x y = (sum_div x) ==  y && (sum_div y) == x

--am_nums :: Int -> [Int]
--am_nums n = 

cart_prod :: Int -> [(Int, Int)]
cart_prod a = [(x, y) | x <- [1..a], y <- [1..a], x /= y ]


sum_ami :: Int -> Int
sum_ami n = sum [(fst tup) + (snd tup) | tup <- cart_prod n, (is_ami (fst tup) (snd tup) )]