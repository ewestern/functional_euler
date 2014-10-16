module Primes where
import Data.List.Ordered

primesTo :: [Int]
primesTo m = eratos [2..m]
  where
    eratos [] = []
    -- the first element is prime by stipulaition
    -- the tail is the difference between the remaining ints and multiples of the current prime up to m
    eratos (x:xs) = x : eratos (xs `minus` [x*x, x*x+x..m])
    
primesPE :: [Int]   
primesPE = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    -- ~ delays evaluation of value
    -- ps  
    sieve (x:xs) q ps@ ~(p:t)
        -- explictly stop list intersection as soon as possible.
        --  if the current prime is less than the square of the previous (i.e., the next composite), just sieve the remainder of the list
        | x < q     = x : sieve xs q ps
        -- otherwise, we don't know what the next composite will be.  
        | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t 
