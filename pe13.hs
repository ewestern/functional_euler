import Control.Monad
import Data.Char

main = forever $ do
    handle <- openFile "pe13num.txt" ReadMode
    contents <- getContents handle
