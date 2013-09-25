import Data.Char
import Data.Maybe

singles = [(1, "one"),
			(2, "two"),
			(3, "three"),
			(4, "four"),
			(5, "five"),
			(6, "six"),
			(7, "seven"),
			(8, "eight"),
			(9, "nine")]

teens = [(10, "ten"),
			(11, "eleven"),
			(12, "twelve"),
			(13, "thirteen"),
			(14, "fourteen"),
			(15, "fifteen"),
			(16, "sixteen"),
			(17, "seventeen"),
			(18, "eighteen"),
			(19, "nineteen")]

tees = [(2, "twenty"),
			(3, "thirty"),
			(4, "forty"),
			(5, "fifty"),
			(6, "sixty"),
			(7, "seventy"),
			(8, "eighty"),
			(9, "ninety")
			]

countLetters :: Int -> Int
countLetters num = sum $ map (\x -> length $ toWords $ show x) [1..num] 



toWords :: String -> String
toWords (x:xs) 
	| x == '0' && len > 1 		= "" ++ toWords xs
	| x == '0'					= ""
	| len == 1 					= fromJust (lookup (digitToInt x) singles)
	| len == 2 && x == '1' 		= fromJust (lookup (read (x:xs)) teens)
	| len == 2 				 	= fromJust (lookup (digitToInt x) tees) ++ toWords xs
	| len == 3 && xs == "00" 	= fromJust (lookup (digitToInt x) singles) ++ "hundred"
	| len == 3 					= fromJust (lookup (digitToInt x) singles) ++ "hundredand" ++ toWords xs
	| len == 4 					= fromJust (lookup (digitToInt x) singles) ++ "thousand" ++ toWords xs
	where len = length (x:xs)
