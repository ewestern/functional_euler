import Data.Maybe


days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
numDays = [("January", 31), ("February", 28), ("March", 31), ("April", 30), ("May", 31), ("June", 30),("July", 31), ("August", 31), ("September", 30), ("October", 31), ("November", 30), ("December", 31)]

monthD month = zip (cycle days)

year num 
	| 
[1..fromMaybe 0 (lookup month numDays)]

years