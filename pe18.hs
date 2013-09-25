import Data.List
import Data.Maybe

searchMemoized :: [Int] -> Int
searchMemoized t = searchMemoized' 0
    where searchMemoized' = (map search [0 ..] !!)
            where search ind | (leftChild ind) >= length t = t !! ind
                             | otherwise = max left right
                    where   left = t !! ind + searchMemoized' (leftChild ind)
                            right = t !! ind + searchMemoized' (rightChild ind)

subTree :: Int -> Int -> [Int] -> [Int]
subTree fr to ls 
	| fr >= length ls = []
	| otherwise = (take ((to-fr)+ 1) $ drop fr ls) ++ (subTree (leftChild fr) (rightChild to) ls)

leftChild :: Int -> Int
leftChild ind = ind + (currRow ind)

rightChild :: Int -> Int
rightChild ind = ind + (currRow ind) + 1 

currRow :: Int -> Int
currRow ind = 1 + (length $ takeWhile (\x -> ind + 1 > x) [sum [1..n] | n <- [1..]])


--estimate remain ls = sum $ maxN remain ls
numRows :: [Int] -> Int
numRows ls = currRow ((length ls) -1 )

maxN :: Int -> ([Int] -> [Int])
maxN num = foldl (maxHelp num) []

maxHelp n acc val
	| length acc < n  = val : acc
	| val > minimum acc = val : [v | v <- acc, v /= minimum acc]
	| otherwise = acc



listText :: String -> [Int]
listText s = map (\x -> read x :: Int) $ words s


main :: IO()
main = do
    triangle <- readFile "./pe_67.txt"
    putStrLn $ show $ searchMemoized $ (listText triangle)

