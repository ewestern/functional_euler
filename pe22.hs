

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y1 | y1 <-xs, y1 < x] ++ [x] ++ quicksort [y2 | y2 <-xs, y2 >= x]

wordValue

tallyWords :: [String]
tallyWords xs = map
  where f w = 

main = do
  args <- getArgs
  names <- lines <$> readFile (args !! 0)
  
  return ()
